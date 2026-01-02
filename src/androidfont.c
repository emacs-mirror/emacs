/* Android fallback font driver.

Copyright (C) 2023-2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

/* Due to the terrible nature of the Android Typeface subsystems, this
   font driver is only used as a fallback when sfntfont-android.c
   fails to enumerate any fonts at all.  */

#include <config.h>

#include "lisp.h"
#include "dispextern.h"
#include "composite.h"
#include "blockinput.h"
#include "charset.h"
#include "frame.h"
#include "window.h"
#include "fontset.h"
#include "androidterm.h"
#include "character.h"
#include "coding.h"
#include "font.h"
#include "termchar.h"
#include "pdumper.h"
#include "android.h"

#ifndef ANDROID_STUBIFY

#include <android/log.h>

struct android_emacs_font_driver
{
  jclass class;
  jmethodID list;
  jmethodID match;
  jmethodID list_families;
  jmethodID open_font;
  jmethodID has_char;
  jmethodID text_extents;
  jmethodID encode_char;
  jmethodID draw;

  /* Static methods.  */
  jmethodID create_font_driver;
};

struct android_emacs_font_spec
{
  jclass class;
  jfieldID foundry;
  jfieldID family;
  jfieldID adstyle;
  jfieldID registry;
  jfieldID width;
  jfieldID weight;
  jfieldID slant;
  jfieldID size;
  jfieldID spacing;
  jfieldID avgwidth;
  jfieldID dpi;
};

struct android_emacs_font_metrics
{
  jclass class;
  jfieldID lbearing;
  jfieldID rbearing;
  jfieldID width;
  jfieldID ascent;
  jfieldID descent;
};

struct android_emacs_font_object
{
  jclass class;
  jfieldID min_width;
  jfieldID max_width;
  jfieldID pixel_size;
  jfieldID height;
  jfieldID space_width;
  jfieldID average_width;
  jfieldID ascent;
  jfieldID descent;
  jfieldID underline_thickness;
  jfieldID underline_position;
  jfieldID baseline_offset;
  jfieldID relative_compose;
  jfieldID default_ascent;
  jfieldID encoding_charset;
  jfieldID repertory_charset;
};

struct android_integer
{
  jclass class;
  jmethodID constructor;
  jmethodID int_value;
};

struct androidfont_info
{
  /* The font pseudo-vector object.  */
  struct font font;

  /* The Java-side font.  */
  jobject object;

  /* Cached glyph metrics arranged in a two dimensional array.  */
  struct font_metrics **metrics;
};

struct androidfont_entity
{
  /* The font entity pvec.  */
  struct font_entity font;

  /* The Java-side font entity.  */
  jobject object;
};

/* Method and class identifiers associated with the EmacsFontDriver
   class.  */

static struct android_emacs_font_driver font_driver_class;

/* Field and class identifiers associated with the
   EmacsFontDriver$FontSpec class.  */

static struct android_emacs_font_spec font_spec_class;

/* Method and class identifiers associated with the Integer class.  */

static struct android_integer integer_class;

/* Field and class identifiers associated with the
   EmacsFontDriver$FontMetrics class.  */

static struct android_emacs_font_metrics font_metrics_class;

/* Field and class identifiers associated with the
   EmacsFontDriver$FontObject class.  */

static struct android_emacs_font_object font_object_class;

/* The font cache.  */

static Lisp_Object font_cache;

/* The Java-side font driver.  */

static jobject font_driver;



/* Initialize the class and method identifiers for functions in the
   EmacsFontDriver class, and place them in `font_driver_class'.  */

static void
android_init_font_driver (void)
{
  jclass old;

  font_driver_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "org/gnu/emacs/EmacsFontDriver");
  eassert (font_driver_class.class);

  old = font_driver_class.class;
  font_driver_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!font_driver_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)					\
  font_driver_class.c_name							\
    = (*android_java_env)->GetMethodID (android_java_env,			\
					font_driver_class.class,		\
					name, signature);			\
  eassert (font_driver_class.c_name);

  FIND_METHOD (list, "list", "(Lorg/gnu/emacs/EmacsFontDriver$FontSpec;)"
	       "[Lorg/gnu/emacs/EmacsFontDriver$FontEntity;");
  FIND_METHOD (match, "match", "(Lorg/gnu/emacs/EmacsFontDriver$FontSpec;)"
	       "Lorg/gnu/emacs/EmacsFontDriver$FontEntity;");
  FIND_METHOD (list_families, "listFamilies", "()[Ljava/lang/String;");
  FIND_METHOD (open_font, "openFont", "(Lorg/gnu/emacs/EmacsFontDriver$Font"
	       "Entity;I)Lorg/gnu/emacs/EmacsFontDriver$FontObject;");
  FIND_METHOD (has_char, "hasChar", "(Lorg/gnu/emacs/EmacsFontDriver$Font"
	       "Spec;I)I");
  FIND_METHOD (text_extents, "textExtents", "(Lorg/gnu/emacs/EmacsFontDriver"
	       "$FontObject;[ILorg/gnu/emacs/EmacsFontDriver$FontMetrics;)V");
  FIND_METHOD (encode_char, "encodeChar", "(Lorg/gnu/emacs/EmacsFontDriver"
	       "$FontObject;I)I");
  FIND_METHOD (draw, "draw", "(Lorg/gnu/emacs/EmacsFontDriver$FontObject;"
	       "Lorg/gnu/emacs/EmacsGC;Lorg/gnu/emacs/EmacsDrawable;[IIIIZ)I");

  font_driver_class.create_font_driver
    = (*android_java_env)->GetStaticMethodID (android_java_env,
					      font_driver_class.class,
					      "createFontDriver",
					      "()Lorg/gnu/emacs/"
					      "EmacsFontDriver;");
  eassert (font_driver_class.create_font_driver);
#undef FIND_METHOD
}

/* Initialize the class and field identifiers for functions in the
   EmacsFontDriver$FontSpec class, and place them in
   `font_spec_class'.  */

static void
android_init_font_spec (void)
{
  jclass old;

  font_spec_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "org/gnu/emacs/EmacsFontDriver"
				      "$FontSpec");
  eassert (font_spec_class.class);

  old = font_spec_class.class;
  font_spec_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!font_spec_class.class)
    emacs_abort ();

#define FIND_FIELD(c_name, name, signature)					\
  font_spec_class.c_name							\
    = (*android_java_env)->GetFieldID (android_java_env,			\
				       font_spec_class.class,			\
				       name, signature);			\
  eassert (font_spec_class.c_name);

  FIND_FIELD (foundry, "foundry", "Ljava/lang/String;");
  FIND_FIELD (family, "family", "Ljava/lang/String;");
  FIND_FIELD (adstyle, "adstyle", "Ljava/lang/String;");
  FIND_FIELD (registry, "registry", "Ljava/lang/String;");
  FIND_FIELD (width, "width", "Ljava/lang/Integer;");
  FIND_FIELD (weight, "weight", "Ljava/lang/Integer;");
  FIND_FIELD (slant, "slant", "Ljava/lang/Integer;");
  FIND_FIELD (size, "size", "Ljava/lang/Integer;");
  FIND_FIELD (spacing, "spacing", "Ljava/lang/Integer;");
  FIND_FIELD (avgwidth, "avgwidth", "Ljava/lang/Integer;");
  FIND_FIELD (dpi, "dpi", "Ljava/lang/Integer;");
#undef FIND_FIELD
}

static void
android_init_font_metrics (void)
{
  jclass old;

  font_metrics_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "org/gnu/emacs/EmacsFontDriver"
				      "$FontMetrics");
  eassert (font_metrics_class.class);

  old = font_metrics_class.class;
  font_metrics_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!font_metrics_class.class)
    emacs_abort ();

#define FIND_FIELD(c_name, name, signature)					\
  font_metrics_class.c_name							\
    = (*android_java_env)->GetFieldID (android_java_env,			\
				       font_metrics_class.class,		\
				       name, signature);			\
  eassert (font_metrics_class.c_name);

  FIND_FIELD (lbearing, "lbearing", "S");
  FIND_FIELD (rbearing, "rbearing", "S");
  FIND_FIELD (width, "width", "S");
  FIND_FIELD (ascent, "ascent", "S");
  FIND_FIELD (descent, "descent", "S");
#undef FIND_FIELD
}

static void
android_init_integer (void)
{
  jclass old;

  integer_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "java/lang/Integer");
  eassert (integer_class.class);

  old = integer_class.class;
  integer_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!integer_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)					\
  integer_class.c_name								\
    = (*android_java_env)->GetMethodID (android_java_env,			\
					integer_class.class,			\
					name, signature);			\
  eassert (integer_class.c_name);

  FIND_METHOD (constructor, "<init>", "(I)V");
  FIND_METHOD (int_value, "intValue", "()I");
#undef FIND_METHOD
}

static void
android_init_font_object (void)
{
  jclass old;

  font_object_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "org/gnu/emacs/EmacsFontDriver"
				      "$FontObject");
  eassert (font_object_class.class);

  old = font_object_class.class;
  font_object_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!font_object_class.class)
    emacs_abort ();

#define FIND_FIELD(c_name, name, signature)					\
  font_object_class.c_name							\
    = (*android_java_env)->GetFieldID (android_java_env,			\
				       font_object_class.class,			\
				       name, signature);			\
  eassert (font_object_class.c_name);

  FIND_FIELD (min_width, "minWidth", "I");
  FIND_FIELD (max_width, "maxWidth", "I");
  FIND_FIELD (pixel_size, "pixelSize", "I");
  FIND_FIELD (height, "height", "I");
  FIND_FIELD (space_width, "spaceWidth", "I");
  FIND_FIELD (average_width, "averageWidth", "I");
  FIND_FIELD (ascent, "ascent", "I");
  FIND_FIELD (descent, "descent", "I");
  FIND_FIELD (underline_thickness, "underlineThickness", "I");
  FIND_FIELD (underline_position, "underlinePosition", "I");
  FIND_FIELD (baseline_offset, "baselineOffset", "I");
  FIND_FIELD (relative_compose, "relativeCompose", "I");
  FIND_FIELD (default_ascent, "defaultAscent", "I");
  FIND_FIELD (encoding_charset, "encodingCharset", "I");
  FIND_FIELD (repertory_charset, "repertoryCharset", "I");
#undef FIND_FIELD
}

static Lisp_Object
androidfont_get_cache (struct frame *frame)
{
  return font_cache;
}

/* Initialize the Java side of the font driver if it has not already
   been initialized.  This is only done whenever necessary because the
   font driver otherwise uses a lot of memory, as it has to keep every
   typeface open.  */

static void
androidfont_check_init (void)
{
  jmethodID method;
  jobject old;

  if (font_driver)
    return;

  /* Log a loud message.  This font driver really should not be
     used.  */
  __android_log_print (ANDROID_LOG_WARN, __func__,
		       "The Android font driver is being used."
		       "  Please investigate why this is so.");

  method = font_driver_class.create_font_driver;

  /* Initialize the font driver on the Java side.  */
  font_driver
    = (*android_java_env)->CallStaticObjectMethod (android_java_env,
						   font_driver_class.class,
						   method);
  android_exception_check ();

  old = font_driver;
  font_driver
    = (*android_java_env)->NewGlobalRef (android_java_env, font_driver);
  ANDROID_DELETE_LOCAL_REF (old);
}

/* Return a local reference to an instance of EmacsFontDriver$FontSpec
   with the same values as FONT.  */

static jobject
androidfont_from_lisp (Lisp_Object font)
{
  jobject spec, integer;
  jstring string;
  Lisp_Object tem;

  spec = (*android_java_env)->AllocObject (android_java_env,
					   font_spec_class.class);
  android_exception_check ();

#define DO_SYMBOL_FIELD(field, index)						\
  tem = AREF (font, index);							\
  if (SYMBOLP (tem))								\
    {										\
      /* Java seems to DTRT with the Emacs string encoding, so this does	\
	 not matter at all.  */							\
      string = (*android_java_env)->NewStringUTF (android_java_env,		\
						  SSDATA (SYMBOL_NAME (tem)));	\
      android_exception_check_1 (spec);						\
										\
      (*android_java_env)->SetObjectField (android_java_env, spec,		\
					   font_spec_class.field,		\
					   string);				\
      ANDROID_DELETE_LOCAL_REF (string);					\
    }										\

  DO_SYMBOL_FIELD (foundry, FONT_FOUNDRY_INDEX);
  DO_SYMBOL_FIELD (family, FONT_FAMILY_INDEX);
  DO_SYMBOL_FIELD (adstyle, FONT_ADSTYLE_INDEX);
  DO_SYMBOL_FIELD (registry, FONT_REGISTRY_INDEX);

#undef DO_SYMBOL_FIELD

#define DO_CARDINAL_FIELD(field, value)						\
  if (value != -1)								\
    {										\
      integer = (*android_java_env)->NewObject (android_java_env,		\
						integer_class.class,		\
						integer_class.constructor,	\
						(jint) value);			\
      android_exception_check_1 (spec);						\
										\
      (*android_java_env)->SetObjectField (android_java_env, spec,		\
					   font_spec_class.field,		\
					   integer);				\
      ANDROID_DELETE_LOCAL_REF (integer);					\
    }

  DO_CARDINAL_FIELD (width, FONT_WIDTH_NUMERIC (font));
  DO_CARDINAL_FIELD (weight, FONT_WEIGHT_NUMERIC (font));
  DO_CARDINAL_FIELD (slant, FONT_SLANT_NUMERIC (font));
  DO_CARDINAL_FIELD (size, (FIXNUMP (AREF (font, FONT_SIZE_INDEX))
			    ? XFIXNUM (AREF (font, FONT_SIZE_INDEX))
			    : -1));
  DO_CARDINAL_FIELD (spacing, (FIXNUMP (AREF (font, FONT_SPACING_INDEX))
			       ? XFIXNUM (AREF (font, FONT_SPACING_INDEX))
			       : -1));
  DO_CARDINAL_FIELD (avgwidth, (FIXNUMP (AREF (font, FONT_AVGWIDTH_INDEX))
				? XFIXNUM (AREF (font, FONT_AVGWIDTH_INDEX))
				: -1));
  DO_CARDINAL_FIELD (dpi, (FIXNUMP (AREF (font, FONT_DPI_INDEX))
			   ? XFIXNUM (AREF (font, FONT_DPI_INDEX))
			   : -1));

#undef DO_CARDINAL_FIELD

  return spec;
}

static void
androidfont_from_java (jobject spec, Lisp_Object entity)
{
  jobject tem;
  jint value;
  const char *string;

#define DO_SYMBOL_FIELD(field, index)						\
  tem = (*android_java_env)->GetObjectField (android_java_env,			\
					     spec,				\
					     font_spec_class.field);		\
  if (tem)									\
    {										\
      string = (*android_java_env)->GetStringUTFChars (android_java_env,	\
						       tem, NULL);		\
      if (!string)								\
	memory_full (0);							\
      ASET (entity, index, intern (string));					\
      (*android_java_env)->ReleaseStringUTFChars (android_java_env,		\
						  tem, string);			\
      ANDROID_DELETE_LOCAL_REF (tem);						\
    }

  DO_SYMBOL_FIELD (foundry, FONT_FOUNDRY_INDEX);
  DO_SYMBOL_FIELD (family, FONT_FAMILY_INDEX);
  DO_SYMBOL_FIELD (adstyle, FONT_ADSTYLE_INDEX);
  DO_SYMBOL_FIELD (registry, FONT_REGISTRY_INDEX);

#undef DO_SYMBOL_FIELD
#define DO_CARDINAL_FIELD(field, index, is_style)			\
  tem = (*android_java_env)->GetObjectField (android_java_env,		\
					     spec,			\
					     font_spec_class.field);	\
  if (tem)								\
    {									\
      value								\
	= (*android_java_env)->CallIntMethod (android_java_env,		\
					      tem,			\
					      integer_class.int_value);	\
      if (!is_style)							\
	ASET (entity, index, make_fixnum (value));			\
      else								\
	FONT_SET_STYLE (entity, index, make_fixnum (value));		\
      ANDROID_DELETE_LOCAL_REF (tem);					\
    }

  DO_CARDINAL_FIELD (width, FONT_WIDTH_INDEX, true);
  DO_CARDINAL_FIELD (weight, FONT_WEIGHT_INDEX, true);
  DO_CARDINAL_FIELD (slant, FONT_SLANT_INDEX, true);
  DO_CARDINAL_FIELD (size, FONT_SIZE_INDEX, false);
  DO_CARDINAL_FIELD (spacing, FONT_SPACING_INDEX, false);
  DO_CARDINAL_FIELD (avgwidth, FONT_AVGWIDTH_INDEX, false);
  DO_CARDINAL_FIELD (dpi, FONT_DPI_INDEX, false);

#undef DO_CARDINAL_FIELD
}

/* Transfer the values from FONT, which must be some kind of font
   entity, */

static Lisp_Object
androidfont_list (struct frame *f, Lisp_Object font_spec)
{
  jobject spec, array, tem;
  jarray entities;
  jsize i, size;
  Lisp_Object value, entity;
  struct androidfont_entity *info;

  /* Maybe initialize the font driver.  */
  androidfont_check_init ();

  spec = androidfont_from_lisp (font_spec);
  array = (*android_java_env)->CallObjectMethod (android_java_env,
						 font_driver,
						 font_driver_class.list,
						 spec);
  android_exception_check_1 (spec);
  ANDROID_DELETE_LOCAL_REF (spec);

  entities = (jarray) array;
  size = (*android_java_env)->GetArrayLength (android_java_env,
					      entities);
  value = Qnil;

  for (i = 0; i < size; ++i)
    {
      entity = font_make_entity_android (VECSIZE (struct androidfont_entity));
      info = (struct androidfont_entity *) XFONT_ENTITY (entity);

      /* The type must be set correctly, or font_open_entity won't be
	 able to find the right font driver.  */
      ASET (entity, FONT_TYPE_INDEX, Qandroid);

      /* Clear this now in case GC happens without it set, which can
	 happen if androidfont_from_java runs out of memory.  */
      info->object = NULL;

      tem = (*android_java_env)->GetObjectArrayElement (android_java_env,
							entities, i);
      androidfont_from_java (tem, entity);

      /* Now, make a global reference to the Java font entity.  */
      info->object = (*android_java_env)->NewGlobalRef (android_java_env,
							(jobject) tem);
      android_exception_check_2 (tem, entities);
      ANDROID_DELETE_LOCAL_REF (tem);

      value = Fcons (entity, value);
    }

  ANDROID_DELETE_LOCAL_REF (entities);
  return Fnreverse (value);
}

static Lisp_Object
androidfont_match (struct frame *f, Lisp_Object font_spec)
{
  jobject spec, result;
  Lisp_Object entity;
  struct androidfont_entity *info;

  /* Maybe initialize the font driver.  */
  androidfont_check_init ();

  spec = androidfont_from_lisp (font_spec);
  result = (*android_java_env)->CallObjectMethod (android_java_env,
						  font_driver,
						  font_driver_class.match,
						  spec);
  android_exception_check_1 (spec);
  ANDROID_DELETE_LOCAL_REF (spec);

  entity = font_make_entity_android (VECSIZE (struct androidfont_entity));
  info = (struct androidfont_entity *) XFONT_ENTITY (entity);

  /* The type must be set correctly, or font_open_entity won't be able
     to find the right font driver.  */
  ASET (entity, FONT_TYPE_INDEX, Qandroid);

  info->object = NULL;
  androidfont_from_java (result, entity);
  info->object = (*android_java_env)->NewGlobalRef (android_java_env,
						    (jobject) result);
  android_exception_check_1 (result);
  ANDROID_DELETE_LOCAL_REF (result);

  return entity;
}

static int
androidfont_draw (struct glyph_string *s, int from, int to,
		  int x, int y, bool with_background)
{
  struct androidfont_info *info;
  jarray chars;
  int rc;
  jobject gcontext, drawable;

  /* Maybe initialize the font driver.  */
  androidfont_check_init ();

  static_assert (sizeof (unsigned int) == sizeof (jint));
  info = (struct androidfont_info *) s->font;

  gcontext = android_resolve_handle (s->gc->gcontext);
  drawable = android_resolve_handle (FRAME_ANDROID_DRAWABLE (s->f));
  chars = (*android_java_env)->NewIntArray (android_java_env,
					    to - from);
  android_exception_check ();

  (*android_java_env)->SetIntArrayRegion (android_java_env, chars,
					  0, to - from,
					  (jint *) s->char2b + from);

  info = (struct androidfont_info *) s->font;
  prepare_face_for_display (s->f, s->face);

  rc = (*android_java_env)->CallIntMethod (android_java_env,
					   font_driver,
					   font_driver_class.draw,
					   info->object,
					   gcontext, drawable,
					   chars, (jint) x, (jint) y,
					   (jint) s->width,
					   (jboolean) with_background);
  android_exception_check_1 (chars);
  ANDROID_DELETE_LOCAL_REF (chars);

  return rc;
}

static Lisp_Object
androidfont_open_font (struct frame *f, Lisp_Object font_entity,
		       int pixel_size)
{
  struct androidfont_info *font_info;
  struct androidfont_entity *entity;
  struct font *font;
  Lisp_Object font_object;
  jobject old;
  jint value;

  /* Maybe initialize the font driver.  */
  androidfont_check_init ();

  if (XFIXNUM (AREF (font_entity, FONT_SIZE_INDEX)) != 0)
    pixel_size = XFIXNUM (AREF (font_entity, FONT_SIZE_INDEX));
  else if (pixel_size == 0)
    {
      /* This bit was copied from xfont.c.  The values might need
	 adjustment.  */

      if (FRAME_FONT (f))
	pixel_size = FRAME_FONT (f)->pixel_size;
      else
	pixel_size = 12;
    }

  entity = (struct androidfont_entity *) XFONT_ENTITY (font_entity);

  block_input ();
  font_object = font_make_object (VECSIZE (struct androidfont_info),
				  font_entity, pixel_size);
  ASET (font_object, FONT_TYPE_INDEX, Qandroid);
  font_info = (struct androidfont_info *) XFONT_OBJECT (font_object);
  font = &font_info->font;
  font->driver = &androidfont_driver;

  /* Clear font_info->object and font_info->metrics early in case GC
     happens later on! */
  font_info->object = NULL;
  font_info->metrics = NULL;
  unblock_input ();

  font_info->object
    = (*android_java_env)->CallObjectMethod (android_java_env,
					     font_driver,
					     font_driver_class.open_font,
					     entity->object,
					     (jint) pixel_size);
  android_exception_check ();

  old = font_info->object;
  font_info->object
    = (*android_java_env)->NewGlobalRef (android_java_env, old);
  android_exception_check_1 (old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!font_info->object)
    return Qnil;

  /* Copy the font attributes from the Java object.  */
  androidfont_from_java (font_info->object, font_object);

  /* Copy font attributes inside EmacsFontDriver$FontObject.  */
#define DO_CARDINAL_FIELD(field)					\
  value									\
    = (*android_java_env)->GetIntField (android_java_env,		\
					font_info->object,		\
					font_object_class.field);	\
  font->field = value;

  DO_CARDINAL_FIELD (min_width);
  DO_CARDINAL_FIELD (max_width);
  DO_CARDINAL_FIELD (pixel_size);
  DO_CARDINAL_FIELD (height);
  DO_CARDINAL_FIELD (space_width);
  DO_CARDINAL_FIELD (average_width);
  DO_CARDINAL_FIELD (ascent);
  DO_CARDINAL_FIELD (descent);
  DO_CARDINAL_FIELD (underline_thickness);
  DO_CARDINAL_FIELD (underline_position);
  DO_CARDINAL_FIELD (baseline_offset);
  DO_CARDINAL_FIELD (relative_compose);
  DO_CARDINAL_FIELD (default_ascent);
  DO_CARDINAL_FIELD (encoding_charset);
  DO_CARDINAL_FIELD (repertory_charset);

#undef DO_CARDINAL_FIELD

  /* This should eventually become unnecessary.  */
  font->props[FONT_NAME_INDEX] = Ffont_xlfd_name (font_object, Qnil, Qt);

  return font_object;
}

static void
androidfont_close_font (struct font *font)
{
  struct androidfont_info *info;
  int i;

  /* Maybe initialize the font driver.  */
  androidfont_check_init ();

  info = (struct androidfont_info *) font;

  /* Free the font metrics cache if it exists.  */

  if (info->metrics)
    {
      for (i = 0; i < 256; ++i)
	xfree (info->metrics[i]);
      xfree (info->metrics);
    }

  info->metrics = NULL;

  /* If info->object is NULL, then FONT was unsuccessfully created,
     and there is no global reference that has to be deleted.

     Alternatively, FONT may have been closed by font_close_object,
     with this function called from GC.  */

  if (!info->object)
    return;

  (*android_java_env)->DeleteGlobalRef (android_java_env,
					info->object);
  info->object = NULL;
}

static int
androidfont_has_char (Lisp_Object font, int c)
{
  struct androidfont_info *info;
  struct androidfont_entity *entity;

  /* Maybe initialize the font driver.  */
  androidfont_check_init ();

  if (FONT_ENTITY_P (font))
    {
      entity = (struct androidfont_entity *) XFONT_ENTITY (font);

      return (*android_java_env)->CallIntMethod (android_java_env,
						 font_driver,
						 font_driver_class.has_char,
						 entity->object, (jint) c);
    }
  else
    {
      info = (struct androidfont_info *) XFONT_OBJECT (font);

      return (*android_java_env)->CallIntMethod (android_java_env,
						 font_driver,
						 font_driver_class.has_char,
						 info->object, (jint) c);
    }
}

static unsigned
androidfont_encode_char (struct font *font, int c)
{
  struct androidfont_info *info;

  /* Maybe initialize the font driver.  */
  androidfont_check_init ();

  info = (struct androidfont_info *) font;

  return (*android_java_env)->CallIntMethod (android_java_env,
					     font_driver,
					     font_driver_class.encode_char,
					     info->object, (jchar) c);
}

static void
androidfont_cache_text_extents (struct androidfont_info *info,
				unsigned int glyph,
				struct font_metrics *metrics)
{
  int i;

  /* Glyphs larger than 65535 can't be cached.  */
  if (glyph >= 256 * 256)
    return;

  if (!info->metrics)
    info->metrics = xzalloc (256 * sizeof *info->metrics);

  if (!info->metrics[glyph / 256])
    {
      info->metrics[glyph / 256]
	= xnmalloc (256, sizeof **info->metrics);

      /* Now, all the metrics in that array as invalid by setting
	 lbearing to SHRT_MAX.  */
      for (i = 0; i < 256; ++i)
	info->metrics[glyph / 256][i].lbearing = SHRT_MAX;
    }

  /* Finally, cache the glyph.  */
  info->metrics[glyph / 256][glyph % 256] = *metrics;
}

static bool
androidfont_check_cached_extents (struct androidfont_info *info,
				  unsigned int glyph,
				  struct font_metrics *metrics)
{
  if (info->metrics && info->metrics[glyph / 256]
      && info->metrics[glyph / 256][glyph % 256].lbearing != SHRT_MAX)
    {
      *metrics = info->metrics[glyph / 256][glyph % 256];
      return true;
    }

  return false;
}

static void
androidfont_text_extents (struct font *font, const unsigned int *code,
			  int nglyphs, struct font_metrics *metrics)
{
  struct androidfont_info *info;
  jarray codepoint_array;
  jobject metrics_object;
  short value;

  /* Maybe initialize the font driver.  */
  androidfont_check_init ();

  info = (struct androidfont_info *) font;

  if (nglyphs == 1
      && androidfont_check_cached_extents (info, *code, metrics))
    return;

  /* Allocate the arrays of code points and font metrics.  */
  codepoint_array
    = (*android_java_env)->NewIntArray (android_java_env,
					nglyphs);
  if (!codepoint_array)
    {
      (*android_java_env)->ExceptionClear (android_java_env);
      memory_full (0);
    }

  static_assert (sizeof (unsigned int) == sizeof (jint));

  /* Always true on every Android device.  */
  (*android_java_env)->SetIntArrayRegion (android_java_env,
					  codepoint_array,
					  0, nglyphs,
					  (jint *) code);

  metrics_object
    = (*android_java_env)->AllocObject (android_java_env,
					font_metrics_class.class);

  (*android_java_env)->CallVoidMethod (android_java_env,
				       font_driver,
				       font_driver_class.text_extents,
				       info->object, codepoint_array,
				       metrics_object);

  if ((*android_java_env)->ExceptionCheck (android_java_env))
    {
      (*android_java_env)->ExceptionClear (android_java_env);
      ANDROID_DELETE_LOCAL_REF (metrics_object);
      ANDROID_DELETE_LOCAL_REF (codepoint_array);
      memory_full (0);
    }

#define DO_CARDINAL_FIELD(field)					\
  value									\
    = (*android_java_env)->GetShortField (android_java_env,		\
					  metrics_object,		\
					  font_metrics_class.field);	\
  metrics->field = value;

  DO_CARDINAL_FIELD (lbearing);
  DO_CARDINAL_FIELD (rbearing);
  DO_CARDINAL_FIELD (width);
  DO_CARDINAL_FIELD (ascent);
  DO_CARDINAL_FIELD (descent);

#undef DO_CARDINAL_FIELD

  ANDROID_DELETE_LOCAL_REF (metrics_object);
  ANDROID_DELETE_LOCAL_REF (codepoint_array);

  /* Emacs spends a lot of time in androidfont_text_extents, which
     makes calling JNI too slow.  Cache the metrics for this single
     glyph.  */

  if (nglyphs == 1)
    androidfont_cache_text_extents (info, *code, metrics);
}

static Lisp_Object
androidfont_list_family (struct frame *f)
{
  Lisp_Object families;
  jarray family_array;
  jobject string;
  jsize i, length;
  const char *family;

  /* Return if the Android font driver is not initialized.  Loading
     every font under Android takes a non trivial amount of memory,
     and is not something that should be done when the user tries to
     list all of the font families.  */

  if (!font_driver)
    return Qnil;

  family_array
    = (*android_java_env)->CallObjectMethod (android_java_env,
					     font_driver,
					     font_driver_class.list_families);
  android_exception_check ();

  length = (*android_java_env)->GetArrayLength (android_java_env,
						family_array);
  families = Qnil;

  for (i = 0; i < length; ++i)
    {
      string = (*android_java_env)->GetObjectArrayElement (android_java_env,
							   family_array, i);
      family = (*android_java_env)->GetStringUTFChars (android_java_env,
						       (jstring) string, NULL);

      if (!family)
	{
	  ANDROID_DELETE_LOCAL_REF (string);
	  ANDROID_DELETE_LOCAL_REF (family_array);
	}

      families = Fcons (build_string_from_utf8 (string), families);
      (*android_java_env)->ReleaseStringUTFChars (android_java_env,
						  (jstring) string,
						  family);
      ANDROID_DELETE_LOCAL_REF (string);
    }

  ANDROID_DELETE_LOCAL_REF (family_array);
  return Fnreverse (families);
}

struct font_driver androidfont_driver =
  {
    .type = LISPSYM_INITIALLY (Qandroid),
    .case_sensitive = true,
    .get_cache = androidfont_get_cache,
    .list = androidfont_list,
    .match = androidfont_match,
    .draw = androidfont_draw,
    .open_font = androidfont_open_font,
    .close_font = androidfont_close_font,
    .has_char = androidfont_has_char,
    .encode_char = androidfont_encode_char,
    .text_extents = androidfont_text_extents,
    .list_family = androidfont_list_family,
  };

static void
syms_of_androidfont_for_pdumper (void)
{
  register_font_driver (&androidfont_driver, NULL);
}

void
syms_of_androidfont (void)
{
  DEFSYM (Qfontsize, "fontsize");

  pdumper_do_now_and_after_load (syms_of_androidfont_for_pdumper);

  font_cache = list (Qnil);
  staticpro (&font_cache);
}

void
init_androidfont (void)
{
  if (!android_init_gui)
    return;

  android_init_font_driver ();
  android_init_font_spec ();
  android_init_font_metrics ();
  android_init_font_object ();
  android_init_integer ();

  /* The Java font driver is not initialized here because it uses a lot
     of memory.  */
}

void
android_finalize_font_entity (struct font_entity *entity)
{
  struct androidfont_entity *info;

  info = (struct androidfont_entity *) entity;

  if (info->object)
    (*android_java_env)->DeleteGlobalRef (android_java_env,
					  info->object);

  /* Not sure if this can be called twice.  */
  info->object = NULL;
}

#endif
