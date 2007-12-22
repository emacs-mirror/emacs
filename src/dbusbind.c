/* Elisp bindings for D-Bus.
   Copyright (C) 2007 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"

#ifdef HAVE_DBUS
#include <stdlib.h>
#include <stdio.h>
#include <dbus/dbus.h>
#include "lisp.h"
#include "frame.h"
#include "termhooks.h"
#include "keyboard.h"


/* Subroutines.  */
Lisp_Object Qdbus_get_unique_name;
Lisp_Object Qdbus_call_method;
Lisp_Object Qdbus_send_signal;
Lisp_Object Qdbus_register_signal;
Lisp_Object Qdbus_unregister_signal;

/* D-Bus error symbol.  */
Lisp_Object Qdbus_error;

/* Lisp symbols of the system and session buses.  */
Lisp_Object QCdbus_system_bus, QCdbus_session_bus;

/* Lisp symbols of D-Bus types.  */
Lisp_Object QCdbus_type_byte, QCdbus_type_boolean;
Lisp_Object QCdbus_type_int16, QCdbus_type_uint16;
Lisp_Object QCdbus_type_int32, QCdbus_type_uint32;
Lisp_Object QCdbus_type_int64, QCdbus_type_uint64;
Lisp_Object QCdbus_type_double, QCdbus_type_string;
Lisp_Object QCdbus_type_object_path, QCdbus_type_signature;
Lisp_Object QCdbus_type_array, QCdbus_type_variant;
Lisp_Object QCdbus_type_struct, QCdbus_type_dict_entry;

/* Hash table which keeps function definitions.  */
Lisp_Object Vdbus_registered_functions_table;

/* Whether to debug D-Bus.  */
Lisp_Object Vdbus_debug;


/* We use "xd_" and "XD_" as prefix for all internal symbols, because
   we don't want to poison other namespaces with "dbus_".  */

/* Raise a Lisp error from a D-Bus ERROR.  */
#define XD_ERROR(error)							\
  {									\
    char s[1024];							\
    strcpy (s, error.message);						\
    dbus_error_free (&error);						\
    /* Remove the trailing newline.  */					\
    if (strchr (s, '\n') != NULL)					\
      s[strlen (s) - 1] = '\0';						\
    xsignal1 (Qdbus_error, build_string (s));				\
  }

/* Macros for debugging.  In order to enable them, build with
   "make MYCPPFLAGS='-DDBUS_DEBUG'".  */
#ifdef DBUS_DEBUG
#define XD_DEBUG_MESSAGE(...)		\
  {					\
    char s[1024];			\
    sprintf (s, __VA_ARGS__);		\
    printf ("%s: %s\n", __func__, s);	\
    message ("%s: %s", __func__, s);	\
  }
#define XD_DEBUG_VALID_LISP_OBJECT_P(object)				\
  if (!valid_lisp_object_p (object))					\
    {									\
      XD_DEBUG_MESSAGE ("%s Assertion failure", __LINE__);		\
      xsignal1 (Qdbus_error, build_string ("Assertion failure"));	\
    }

#else /* !DBUS_DEBUG */
#define XD_DEBUG_MESSAGE(...)		\
  if (!NILP (Vdbus_debug))		\
    {					\
      char s[1024];			\
      sprintf (s, __VA_ARGS__);		\
      message ("%s: %s", __func__, s);	\
    }
#define XD_DEBUG_VALID_LISP_OBJECT_P(object)
#endif

/* Check whether TYPE is a basic DBusType.  */
#define XD_BASIC_DBUS_TYPE(type)					\
  ((type ==  DBUS_TYPE_BYTE)						\
   || (type ==  DBUS_TYPE_BOOLEAN)					\
   || (type ==  DBUS_TYPE_INT16)					\
   || (type ==  DBUS_TYPE_UINT16)					\
   || (type ==  DBUS_TYPE_INT32)					\
   || (type ==  DBUS_TYPE_UINT32)					\
   || (type ==  DBUS_TYPE_INT64)					\
   || (type ==  DBUS_TYPE_UINT64)					\
   || (type ==  DBUS_TYPE_DOUBLE)					\
   || (type ==  DBUS_TYPE_STRING)					\
   || (type ==  DBUS_TYPE_OBJECT_PATH)					\
   || (type ==  DBUS_TYPE_SIGNATURE))

/* Determine the DBusType of a given Lisp symbol.  OBJECT must be one
   of the predefined D-Bus type symbols.  */
#define XD_SYMBOL_TO_DBUS_TYPE(object)					\
  ((EQ (object, QCdbus_type_byte)) ? DBUS_TYPE_BYTE			\
   : (EQ (object, QCdbus_type_boolean)) ? DBUS_TYPE_BOOLEAN		\
   : (EQ (object, QCdbus_type_int16)) ? DBUS_TYPE_INT16			\
   : (EQ (object, QCdbus_type_uint16)) ? DBUS_TYPE_UINT16		\
   : (EQ (object, QCdbus_type_int32)) ? DBUS_TYPE_INT32			\
   : (EQ (object, QCdbus_type_uint32)) ? DBUS_TYPE_UINT32		\
   : (EQ (object, QCdbus_type_int64)) ? DBUS_TYPE_INT64			\
   : (EQ (object, QCdbus_type_uint64)) ? DBUS_TYPE_UINT64		\
   : (EQ (object, QCdbus_type_double)) ? DBUS_TYPE_DOUBLE		\
   : (EQ (object, QCdbus_type_string)) ? DBUS_TYPE_STRING		\
   : (EQ (object, QCdbus_type_object_path)) ? DBUS_TYPE_OBJECT_PATH	\
   : (EQ (object, QCdbus_type_signature)) ? DBUS_TYPE_SIGNATURE		\
   : (EQ (object, QCdbus_type_array)) ? DBUS_TYPE_ARRAY			\
   : (EQ (object, QCdbus_type_variant)) ? DBUS_TYPE_VARIANT		\
   : (EQ (object, QCdbus_type_struct)) ? DBUS_TYPE_STRUCT		\
   : (EQ (object, QCdbus_type_dict_entry)) ? DBUS_TYPE_DICT_ENTRY	\
   : DBUS_TYPE_INVALID)

/* Check whether a Lisp symbol is a predefined D-Bus type symbol.  */
#define XD_DBUS_TYPE_P(object)						\
  (SYMBOLP (object) && ((XD_SYMBOL_TO_DBUS_TYPE (object) != DBUS_TYPE_INVALID)))

/* Determine the DBusType of a given Lisp OBJECT.  It is used to
   convert Lisp objects, being arguments of `dbus-call-method' or
   `dbus-send-signal', into corresponding C values appended as
   arguments to a D-Bus message.  */
#define XD_OBJECT_TO_DBUS_TYPE(object)					\
  ((EQ (object, Qt) || EQ (object, Qnil)) ? DBUS_TYPE_BOOLEAN		\
   : (NATNUMP (object)) ? DBUS_TYPE_UINT32				\
   : (INTEGERP (object)) ? DBUS_TYPE_INT32				\
   : (FLOATP (object)) ? DBUS_TYPE_DOUBLE				\
   : (STRINGP (object)) ? DBUS_TYPE_STRING				\
   : (XD_DBUS_TYPE_P (object)) ? XD_SYMBOL_TO_DBUS_TYPE (object)	\
   : (CONSP (object)) ? ((XD_DBUS_TYPE_P (XCAR (object)))		\
			 ? XD_SYMBOL_TO_DBUS_TYPE (XCAR (object))	\
			 : DBUS_TYPE_ARRAY)				\
   : DBUS_TYPE_INVALID)

/* Return a list pointer which does not have a Lisp symbol as car.  */
#define XD_NEXT_VALUE(object)					\
  ((XD_DBUS_TYPE_P (XCAR (object))) ? XCDR (object) : object)

/* Compute SIGNATURE of OBJECT.  It must have a form that it can be
   used in dbus_message_iter_open_container.  DTYPE is the DBusType
   the object is related to.  It is passed as argument, because it
   cannot be detected in basic type objects, when they are preceded by
   a type symbol.  PARENT_TYPE is the DBusType of a container this
   signature is embedded, or DBUS_TYPE_INVALID.  It is needed for the
   check that DBUS_TYPE_DICT_ENTRY occurs only as array element.  */
void
xd_signature(signature, dtype, parent_type, object)
     char *signature;
     unsigned int dtype, parent_type;
     Lisp_Object object;
{
  unsigned int subtype;
  Lisp_Object elt;
  char x[DBUS_MAXIMUM_SIGNATURE_LENGTH];

  elt = object;

  switch (dtype)
    {
    case DBUS_TYPE_BYTE:
    case DBUS_TYPE_UINT16:
    case DBUS_TYPE_UINT32:
    case DBUS_TYPE_UINT64:
      CHECK_NATNUM (object);
      sprintf (signature, "%c", dtype);
      break;

    case DBUS_TYPE_BOOLEAN:
      if (!EQ (object, Qt) && !EQ (object, Qnil))
	wrong_type_argument (intern ("booleanp"), object);
      sprintf (signature, "%c", dtype);
      break;

    case DBUS_TYPE_INT16:
    case DBUS_TYPE_INT32:
    case DBUS_TYPE_INT64:
      CHECK_NUMBER (object);
      sprintf (signature, "%c", dtype);
      break;

    case DBUS_TYPE_DOUBLE:
      CHECK_FLOAT (object);
      sprintf (signature, "%c", dtype);
      break;

    case DBUS_TYPE_STRING:
    case DBUS_TYPE_OBJECT_PATH:
    case DBUS_TYPE_SIGNATURE:
      CHECK_STRING (object);
      sprintf (signature, "%c", dtype);
      break;

    case DBUS_TYPE_ARRAY:
      /* Check that all list elements have the same D-Bus type.  For
	 complex element types, we just check the container type, not
	 the whole element's signature.  */
      CHECK_CONS (object);

      if (EQ (QCdbus_type_array, XCAR (elt))) /* Type symbol is optional.  */
	elt = XD_NEXT_VALUE (elt);
      subtype = XD_OBJECT_TO_DBUS_TYPE (XCAR (elt));
      xd_signature (x, subtype, dtype, XCAR (XD_NEXT_VALUE (elt)));

      while (!NILP (elt))
	{
	  if (subtype != XD_OBJECT_TO_DBUS_TYPE (XCAR (elt)))
	    wrong_type_argument (intern ("D-Bus"), XCAR (elt));
	  elt = XCDR (XD_NEXT_VALUE (elt));
	}

      sprintf (signature, "%c%s", dtype, x);
      break;

    case DBUS_TYPE_VARIANT:
      /* Check that there is exactly one list element.  */
      CHECK_CONS (object);

      elt = XD_NEXT_VALUE (elt);
      subtype = XD_OBJECT_TO_DBUS_TYPE (XCAR (elt));
      xd_signature (x, subtype, dtype, XCAR (XD_NEXT_VALUE (elt)));

      if (!NILP (XCDR (XD_NEXT_VALUE (elt))))
	wrong_type_argument (intern ("D-Bus"),
			     XCAR (XCDR (XD_NEXT_VALUE (elt))));

      sprintf (signature, "%c%s", dtype, x);
      break;

    case DBUS_TYPE_STRUCT:
      /* A struct list might contain any number of elements with
	 different types.  No further check needed.  */
      CHECK_CONS (object);

      elt = XD_NEXT_VALUE (elt);

      /* Compose the signature from the elements.  It is enclosed by
	 parentheses.  */
      sprintf (signature, "%c", DBUS_STRUCT_BEGIN_CHAR );
      while (!NILP (elt))
	{
	  subtype = XD_OBJECT_TO_DBUS_TYPE (XCAR (elt));
	  xd_signature (x, subtype, dtype, XCAR (XD_NEXT_VALUE (elt)));
	  strcat (signature, x);
	  elt = XCDR (XD_NEXT_VALUE (elt));
	}
      sprintf (signature, "%s%c", signature, DBUS_STRUCT_END_CHAR);
      break;

    case DBUS_TYPE_DICT_ENTRY:
      /* Check that there are exactly two list elements, and the first
	 one is of basic type.  The dictionary entry itself must be an
	 element of an array.  */
      CHECK_CONS (object);

      /* Check the parent object type.  */
      if (parent_type != DBUS_TYPE_ARRAY)
	wrong_type_argument (intern ("D-Bus"), object);

      /* Compose the signature from the elements.  It is enclosed by
	 curly braces.  */
      sprintf (signature, "%c", DBUS_DICT_ENTRY_BEGIN_CHAR);

      /* First element.  */
      elt = XD_NEXT_VALUE (elt);
      subtype = XD_OBJECT_TO_DBUS_TYPE (XCAR (elt));
      xd_signature (x, subtype, dtype, XCAR (XD_NEXT_VALUE (elt)));
      strcat (signature, x);

      if (!XD_BASIC_DBUS_TYPE (subtype))
	wrong_type_argument (intern ("D-Bus"), XCAR (XD_NEXT_VALUE (elt)));

      /* Second element.  */
      elt = XCDR (XD_NEXT_VALUE (elt));
      subtype = XD_OBJECT_TO_DBUS_TYPE (XCAR (elt));
      xd_signature (x, subtype, dtype, XCAR (XD_NEXT_VALUE (elt)));
      strcat (signature, x);

      if (!NILP (XCDR (XD_NEXT_VALUE (elt))))
	wrong_type_argument (intern ("D-Bus"),
			     XCAR (XCDR (XD_NEXT_VALUE (elt))));

      /* Closing signature.  */
      sprintf (signature, "%s%c", signature, DBUS_DICT_ENTRY_END_CHAR);
      break;

    default:
      wrong_type_argument (intern ("D-Bus"), object);
    }

  XD_DEBUG_MESSAGE ("%s", signature);
}

/* Append C value, extracted from Lisp OBJECT, to iteration ITER.
   DTYPE must be a valid DBusType.  It is used to convert Lisp
   objects, being arguments of `dbus-call-method' or
   `dbus-send-signal', into corresponding C values appended as
   arguments to a D-Bus message.  */
void
xd_append_arg (dtype, object, iter)
     unsigned int dtype;
     Lisp_Object object;
     DBusMessageIter *iter;
{
  Lisp_Object elt;
  char signature[DBUS_MAXIMUM_SIGNATURE_LENGTH];
  DBusMessageIter subiter;
  char *value;

  XD_DEBUG_MESSAGE ("%c %s", dtype, SDATA (format2 ("%s", object, Qnil)));

  if (XD_BASIC_DBUS_TYPE (dtype))
    {
      switch (dtype)
	{
	case DBUS_TYPE_BYTE:
	  XD_DEBUG_MESSAGE ("%c %u", dtype, XUINT (object));
	  value = (unsigned char *) XUINT (object);
	  break;

	case DBUS_TYPE_BOOLEAN:
	  XD_DEBUG_MESSAGE ("%c %s", dtype, (NILP (object)) ? "false" : "true");
	  value = (NILP (object))
	    ? (unsigned char *) FALSE : (unsigned char *) TRUE;
	  break;

	case DBUS_TYPE_INT16:
	  XD_DEBUG_MESSAGE ("%c %d", dtype, XINT (object));
	  value = (char *) (dbus_int16_t *) XINT (object);
	  break;

	case DBUS_TYPE_UINT16:
	  XD_DEBUG_MESSAGE ("%c %u", dtype, XUINT (object));
	  value = (char *) (dbus_uint16_t *) XUINT (object);
	  break;

	case DBUS_TYPE_INT32:
	  XD_DEBUG_MESSAGE ("%c %d", dtype, XINT (object));
	  value = (char *) (dbus_int32_t *) XINT (object);
	  break;

	case DBUS_TYPE_UINT32:
	  XD_DEBUG_MESSAGE ("%c %u", dtype, XUINT (object));
	  value = (char *) (dbus_uint32_t *) XUINT (object);
	  break;

	case DBUS_TYPE_INT64:
	  XD_DEBUG_MESSAGE ("%c %d", dtype, XINT (object));
	  value = (char *) (dbus_int64_t *) XINT (object);
	  break;

	case DBUS_TYPE_UINT64:
	  XD_DEBUG_MESSAGE ("%c %u", dtype, XUINT (object));
	  value = (char *) (dbus_int64_t *) XUINT (object);
	  break;

	case DBUS_TYPE_DOUBLE:
	  XD_DEBUG_MESSAGE ("%c %f", dtype, XFLOAT (object));
	  value = (char *) (double *) XFLOAT (object);
	  break;

	case DBUS_TYPE_STRING:
	case DBUS_TYPE_OBJECT_PATH:
	case DBUS_TYPE_SIGNATURE:
	  XD_DEBUG_MESSAGE ("%c %s", dtype, SDATA (object));
	  value = SDATA (object);
	  break;
	}

      if (!dbus_message_iter_append_basic (iter, dtype, &value))
	xsignal2 (Qdbus_error,
		  build_string ("Unable to append argument"), object);
    }

  else /* Compound types.  */
    {

      /* All compound types except array have a type symbol.  For
	 array, it is optional.  Skip it.  */
      if (!XD_BASIC_DBUS_TYPE (XD_OBJECT_TO_DBUS_TYPE (XCAR (object))))
	object = XD_NEXT_VALUE (object);

      /* Open new subiteration.  */
      switch (dtype)
	{
	case DBUS_TYPE_ARRAY:
	case DBUS_TYPE_VARIANT:
	  /* A variant has just one element.  An array has elements of
	     the same type.  Both have been checked already for
	     correct types, it is sufficient to retrieve just the
	     signature of the first element.  */
	  xd_signature (signature, XD_OBJECT_TO_DBUS_TYPE (XCAR (object)),
			dtype, XCAR (XD_NEXT_VALUE (object)));
	  XD_DEBUG_MESSAGE ("%c %s %s", dtype, signature,
			    SDATA (format2 ("%s", object, Qnil)));
	  if (!dbus_message_iter_open_container (iter, dtype,
						 signature, &subiter))
	    xsignal3 (Qdbus_error,
		      build_string ("Cannot open container"),
		      make_number (dtype), build_string (signature));
	  break;

	case DBUS_TYPE_STRUCT:
	case DBUS_TYPE_DICT_ENTRY:
	  /* These containers do not require a signature.  */
	  XD_DEBUG_MESSAGE ("%c %s", dtype,
			    SDATA (format2 ("%s", object, Qnil)));
	  if (!dbus_message_iter_open_container (iter, dtype, NULL, &subiter))
	    xsignal2 (Qdbus_error,
		      build_string ("Cannot open container"),
		      make_number (dtype));
	  break;
	}

      /* Loop over list elements.  */
      while (!NILP (object))
	{
	  dtype = XD_OBJECT_TO_DBUS_TYPE (XCAR (object));
	  object = XD_NEXT_VALUE (object);

	  xd_append_arg (dtype, XCAR (object), &subiter);

	  object = XCDR (object);
	}

      /* Close the subiteration.  */
      if (!dbus_message_iter_close_container (iter, &subiter))
	xsignal2 (Qdbus_error,
		  build_string ("Cannot close container"),
		  make_number (dtype));
    }
}

/* Retrieve C value from a DBusMessageIter structure ITER, and return
   a converted Lisp object.  The type DTYPE of the argument of the
   D-Bus message must be a valid DBusType.  Compound D-Bus types
   result always in a Lisp list.  */
Lisp_Object
xd_retrieve_arg (dtype, iter)
     unsigned int dtype;
     DBusMessageIter *iter;
{

  switch (dtype)
    {
    case DBUS_TYPE_BYTE:
    case DBUS_TYPE_INT16:
    case DBUS_TYPE_UINT16:
      {
	dbus_uint16_t val;
	dbus_message_iter_get_basic (iter, &val);
	XD_DEBUG_MESSAGE ("%c %d", dtype, val);
	return make_number (val);
      }

    case DBUS_TYPE_BOOLEAN:
      {
	dbus_bool_t val;
	dbus_message_iter_get_basic (iter, &val);
	XD_DEBUG_MESSAGE ("%c %s", dtype, (val == FALSE) ? "false" : "true");
	return (val == FALSE) ? Qnil : Qt;
      }

    case DBUS_TYPE_INT32:
    case DBUS_TYPE_UINT32:
      {
	dbus_uint32_t val;
	dbus_message_iter_get_basic (iter, &val);
	if (FIXNUM_OVERFLOW_P (val))
	  XD_DEBUG_MESSAGE ("%c %f", dtype, val)
	else
	  XD_DEBUG_MESSAGE ("%c %d", dtype, val);
	return make_fixnum_or_float (val);
      }

    case DBUS_TYPE_INT64:
    case DBUS_TYPE_UINT64:
      {
	dbus_uint64_t val;
	dbus_message_iter_get_basic (iter, &val);
	if (FIXNUM_OVERFLOW_P (val))
	  XD_DEBUG_MESSAGE ("%c %f", dtype, val)
	else
	  XD_DEBUG_MESSAGE ("%c %d", dtype, val);
	return make_fixnum_or_float (val);
      }

    case DBUS_TYPE_DOUBLE:
      {
	double val;
	dbus_message_iter_get_basic (iter, &val);
	XD_DEBUG_MESSAGE ("%c %f", dtype, val);
	return make_float (val);
      }

    case DBUS_TYPE_STRING:
    case DBUS_TYPE_OBJECT_PATH:
    case DBUS_TYPE_SIGNATURE:
      {
	char *val;
	dbus_message_iter_get_basic (iter, &val);
	XD_DEBUG_MESSAGE ("%c %s", dtype, val);
	return build_string (val);
      }

    case DBUS_TYPE_ARRAY:
    case DBUS_TYPE_VARIANT:
    case DBUS_TYPE_STRUCT:
    case DBUS_TYPE_DICT_ENTRY:
      {
	Lisp_Object result;
	struct gcpro gcpro1;
	result = Qnil;
	GCPRO1 (result);
	DBusMessageIter subiter;
	int subtype;
	dbus_message_iter_recurse (iter, &subiter);
	while ((subtype = dbus_message_iter_get_arg_type (&subiter))
	       != DBUS_TYPE_INVALID)
	  {
	    result = Fcons (xd_retrieve_arg (subtype, &subiter), result);
	    dbus_message_iter_next (&subiter);
	  }
	RETURN_UNGCPRO (Fnreverse (result));
      }

    default:
      XD_DEBUG_MESSAGE ("DBusType '%c' not supported", dtype);
      return Qnil;
    }
}


/* Initialize D-Bus connection.  BUS is a Lisp symbol, either :system
   or :session.  It tells which D-Bus to be initialized.  */
DBusConnection *
xd_initialize (bus)
     Lisp_Object bus;
{
  DBusConnection *connection;
  DBusError derror;

  /* Parameter check.  */
  CHECK_SYMBOL (bus);
  if (!((EQ (bus, QCdbus_system_bus)) || (EQ (bus, QCdbus_session_bus))))
    xsignal2 (Qdbus_error, build_string ("Wrong bus name"), bus);

  /* Open a connection to the bus.  */
  dbus_error_init (&derror);

  if (EQ (bus, QCdbus_system_bus))
    connection = dbus_bus_get (DBUS_BUS_SYSTEM, &derror);
  else
    connection = dbus_bus_get (DBUS_BUS_SESSION, &derror);

  if (dbus_error_is_set (&derror))
    XD_ERROR (derror);

  if (connection == NULL)
    xsignal2 (Qdbus_error, build_string ("No connection"), bus);

  /* Return the result.  */
  return connection;
}

DEFUN ("dbus-get-unique-name", Fdbus_get_unique_name, Sdbus_get_unique_name,
       1, 1, 0,
       doc: /* Return the unique name of Emacs registered at D-Bus BUS as string.  */)
     (bus)
     Lisp_Object bus;
{
  DBusConnection *connection;
  char name[DBUS_MAXIMUM_NAME_LENGTH];

  /* Check parameters.  */
  CHECK_SYMBOL (bus);

  /* Open a connection to the bus.  */
  connection = xd_initialize (bus);

  /* Request the name.  */
  strcpy (name, dbus_bus_get_unique_name (connection));
  if (name == NULL)
    xsignal1 (Qdbus_error, build_string ("No unique name available"));

  /* Return.  */
  return build_string (name);
}

DEFUN ("dbus-call-method", Fdbus_call_method, Sdbus_call_method, 5, MANY, 0,
       doc: /* Call METHOD on the D-Bus BUS.

BUS is either the symbol `:system' or the symbol `:session'.

SERVICE is the D-Bus service name to be used.  PATH is the D-Bus
object path SERVICE is registered at.  INTERFACE is an interface
offered by SERVICE.  It must provide METHOD.

All other arguments ARGS are passed to METHOD as arguments.  They are
converted into D-Bus types via the following rules:

  t and nil => DBUS_TYPE_BOOLEAN
  number    => DBUS_TYPE_UINT32
  integer   => DBUS_TYPE_INT32
  float     => DBUS_TYPE_DOUBLE
  string    => DBUS_TYPE_STRING
  list      => DBUS_TYPE_ARRAY

All arguments can be preceded by a type symbol.  For details about
type symbols, see Info node `(dbus)Type Conversion'.

`dbus-call-method' returns the resulting values of METHOD as a list of
Lisp objects.  The type conversion happens the other direction as for
input arguments.  It follows the mapping rules:

  DBUS_TYPE_BOOLEAN     => t or nil
  DBUS_TYPE_BYTE        => number
  DBUS_TYPE_UINT16      => number
  DBUS_TYPE_INT16       => integer
  DBUS_TYPE_UINT32      => number or float
  DBUS_TYPE_INT32       => integer or float
  DBUS_TYPE_UINT64      => number or float
  DBUS_TYPE_INT64       => integer or float
  DBUS_TYPE_DOUBLE      => float
  DBUS_TYPE_STRING      => string
  DBUS_TYPE_OBJECT_PATH => string
  DBUS_TYPE_SIGNATURE   => string
  DBUS_TYPE_ARRAY       => list
  DBUS_TYPE_VARIANT     => list
  DBUS_TYPE_STRUCT      => list
  DBUS_TYPE_DICT_ENTRY  => list

Example:

\(dbus-call-method
  :session "org.gnome.seahorse" "/org/gnome/seahorse/keys/openpgp"
  "org.gnome.seahorse.Keys" "GetKeyField"
  "openpgp:657984B8C7A966DD" "simple-name")

  => (t ("Philip R. Zimmermann"))

If the result of the METHOD call is just one value, the converted Lisp
object is returned instead of a list containing this single Lisp object.

\(dbus-call-method
  :system "org.freedesktop.Hal" "/org/freedesktop/Hal/devices/computer"
  "org.freedesktop.Hal.Device" "GetPropertyString"
  "system.kernel.machine")

  => "i686"

usage: (dbus-call-method BUS SERVICE PATH INTERFACE METHOD &rest ARGS)  */)
     (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  Lisp_Object bus, service, path, interface, method;
  Lisp_Object result;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  DBusConnection *connection;
  DBusMessage *dmessage;
  DBusMessage *reply;
  DBusMessageIter iter;
  DBusError derror;
  unsigned int dtype;
  int i;
  char signature[DBUS_MAXIMUM_SIGNATURE_LENGTH];

  /* Check parameters.  */
  bus = args[0];
  service = args[1];
  path = args[2];
  interface = args[3];
  method = args[4];

  CHECK_SYMBOL (bus);
  CHECK_STRING (service);
  CHECK_STRING (path);
  CHECK_STRING (interface);
  CHECK_STRING (method);
  GCPRO5 (bus, service, path, interface, method);

  XD_DEBUG_MESSAGE ("%s %s %s %s",
		    SDATA (service),
		    SDATA (path),
		    SDATA (interface),
		    SDATA (method));

  /* Open a connection to the bus.  */
  connection = xd_initialize (bus);

  /* Create the message.  */
  dmessage = dbus_message_new_method_call ((char *) SDATA (service),
					   (char *) SDATA (path),
					   (char *) SDATA (interface),
					   (char *) SDATA (method));
  if (dmessage == NULL)
    {
      UNGCPRO;
      xsignal1 (Qdbus_error, build_string ("Unable to create a new message"));
    }

  UNGCPRO;

  /* Initialize parameter list of message.  */
  dbus_message_iter_init_append (dmessage, &iter);

  /* Append parameters to the message.  */
  for (i = 5; i < nargs; ++i)
    {

      XD_DEBUG_VALID_LISP_OBJECT_P (args[i]);
      XD_DEBUG_MESSAGE ("Parameter%d %s",
			i-4, SDATA (format2 ("%s", args[i], Qnil)));

      dtype = XD_OBJECT_TO_DBUS_TYPE (args[i]);
      if (XD_DBUS_TYPE_P (args[i]))
	++i;

      /* Check for valid signature.  We use DBUS_TYPE_INVALID is
	 indication that there is no parent type.  */
      xd_signature (signature, dtype, DBUS_TYPE_INVALID, args[i]);

      xd_append_arg (dtype, args[i], &iter);
    }

  /* Send the message.  */
  dbus_error_init (&derror);
  reply = dbus_connection_send_with_reply_and_block (connection,
						     dmessage,
						     -1,
						     &derror);

  if (dbus_error_is_set (&derror))
    XD_ERROR (derror);

  if (reply == NULL)
    xsignal1 (Qdbus_error, build_string ("No reply"));

  XD_DEBUG_MESSAGE ("Message sent");

  /* Collect the results.  */
  result = Qnil;
  GCPRO1 (result);

  if (!dbus_message_iter_init (reply, &iter))
    {
      UNGCPRO;
      xsignal1 (Qdbus_error, build_string ("Cannot read reply"));
    }

  /* Loop over the parameters of the D-Bus reply message.  Construct a
     Lisp list, which is returned by `dbus-call-method'.  */
  while ((dtype = dbus_message_iter_get_arg_type (&iter)) != DBUS_TYPE_INVALID)
    {
      result = Fcons (xd_retrieve_arg (dtype, &iter), result);
      dbus_message_iter_next (&iter);
    }

  /* Cleanup.  */
  dbus_message_unref (dmessage);
  dbus_message_unref (reply);

  /* Return the result.  If there is only one single Lisp object,
     return it as-it-is, otherwise return the reversed list.  */
  if (XUINT (Flength (result)) == 1)
    RETURN_UNGCPRO (XCAR (result));
  else
    RETURN_UNGCPRO (Fnreverse (result));
}

DEFUN ("dbus-send-signal", Fdbus_send_signal, Sdbus_send_signal, 5, MANY, 0,
       doc: /* Send signal SIGNAL on the D-Bus BUS.

BUS is either the symbol `:system' or the symbol `:session'.

SERVICE is the D-Bus service name SIGNAL is sent from.  PATH is the
D-Bus object path SERVICE is registered at.  INTERFACE is an interface
offered by SERVICE.  It must provide signal SIGNAL.

All other arguments ARGS are passed to SIGNAL as arguments.  They are
converted into D-Bus types via the following rules:

  t and nil => DBUS_TYPE_BOOLEAN
  number    => DBUS_TYPE_UINT32
  integer   => DBUS_TYPE_INT32
  float     => DBUS_TYPE_DOUBLE
  string    => DBUS_TYPE_STRING
  list      => DBUS_TYPE_ARRAY

All arguments can be preceded by a type symbol.  For details about
type symbols, see Info node `(dbus)Type Conversion'.

Example:

\(dbus-send-signal
  :session "org.gnu.Emacs" "/org/gnu/Emacs"
  "org.gnu.Emacs.FileManager" "FileModified" "/home/albinus/.emacs")

usage: (dbus-send-signal BUS SERVICE PATH INTERFACE SIGNAL &rest ARGS)  */)
     (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  Lisp_Object bus, service, path, interface, signal;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  DBusConnection *connection;
  DBusMessage *dmessage;
  DBusMessageIter iter;
  unsigned int dtype;
  int i;
  char signature[DBUS_MAXIMUM_SIGNATURE_LENGTH];

  /* Check parameters.  */
  bus = args[0];
  service = args[1];
  path = args[2];
  interface = args[3];
  signal = args[4];

  CHECK_SYMBOL (bus);
  CHECK_STRING (service);
  CHECK_STRING (path);
  CHECK_STRING (interface);
  CHECK_STRING (signal);
  GCPRO5 (bus, service, path, interface, signal);

  XD_DEBUG_MESSAGE ("%s %s %s %s",
		    SDATA (service),
		    SDATA (path),
		    SDATA (interface),
		    SDATA (signal));

  /* Open a connection to the bus.  */
  connection = xd_initialize (bus);

  /* Create the message.  */
  dmessage = dbus_message_new_signal ((char *) SDATA (path),
				      (char *) SDATA (interface),
				      (char *) SDATA (signal));
  if (dmessage == NULL)
    {
      UNGCPRO;
      xsignal1 (Qdbus_error, build_string ("Unable to create a new message"));
    }

  UNGCPRO;

  /* Initialize parameter list of message.  */
  dbus_message_iter_init_append (dmessage, &iter);

  /* Append parameters to the message.  */
  for (i = 5; i < nargs; ++i)
    {
      XD_DEBUG_VALID_LISP_OBJECT_P (args[i]);
      XD_DEBUG_MESSAGE ("Parameter%d %s",
			i-4, SDATA (format2 ("%s", args[i], Qnil)));

      dtype = XD_OBJECT_TO_DBUS_TYPE (args[i]);
      if (XD_DBUS_TYPE_P (args[i]))
	++i;

      /* Check for valid signature.  We use DBUS_TYPE_INVALID is
	 indication that there is no parent type.  */
      xd_signature (signature, dtype, DBUS_TYPE_INVALID, args[i]);

      xd_append_arg (dtype, args[i], &iter);
    }

  /* Send the message.  The message is just added to the outgoing
     message queue.  */
  if (!dbus_connection_send (connection, dmessage, NULL))
    xsignal1 (Qdbus_error, build_string ("Cannot send message"));

  /* Flush connection to ensure the message is handled.  */
  dbus_connection_flush (connection);

  XD_DEBUG_MESSAGE ("Signal sent");

  /* Cleanup.  */
  dbus_message_unref (dmessage);

  /* Return.  */
  return Qt;
}

/* Read queued incoming message of the D-Bus BUS.  BUS is a Lisp
   symbol, either :system or :session.  */
Lisp_Object
xd_read_message (bus)
     Lisp_Object bus;
{
  Lisp_Object args, key, value;
  struct gcpro gcpro1;
  static struct input_event event;
  DBusConnection *connection;
  DBusMessage *dmessage;
  DBusMessageIter iter;
  unsigned int dtype;
  char uname[DBUS_MAXIMUM_NAME_LENGTH];
  char path[DBUS_MAXIMUM_MATCH_RULE_LENGTH]; /* Unlimited in D-Bus spec.  */
  char interface[DBUS_MAXIMUM_NAME_LENGTH];
  char member[DBUS_MAXIMUM_NAME_LENGTH];

  /* Open a connection to the bus.  */
  connection = xd_initialize (bus);

  /* Non blocking read of the next available message.  */
  dbus_connection_read_write (connection, 0);
  dmessage = dbus_connection_pop_message (connection);

  /* Return if there is no queued message.  */
  if (dmessage == NULL)
    return;

  XD_DEBUG_MESSAGE ("Event received");

  /* Collect the parameters.  */
  args = Qnil;
  GCPRO1 (args);

  if (!dbus_message_iter_init (dmessage, &iter))
    {
      UNGCPRO;
      XD_DEBUG_MESSAGE ("Cannot read event");
      return;
    }

  /* Loop over the resulting parameters.  Construct a list.  */
  while ((dtype = dbus_message_iter_get_arg_type (&iter)) != DBUS_TYPE_INVALID)
    {
      args = Fcons (xd_retrieve_arg (dtype, &iter), args);
      dbus_message_iter_next (&iter);
    }

  /* The arguments are stored in reverse order.  Reorder them.  */
  args = Fnreverse (args);

  /* Read unique name, object path, interface and member from the
     message.  */
  strcpy (uname,     dbus_message_get_sender (dmessage));
  strcpy (path,      dbus_message_get_path (dmessage));
  strcpy (interface, dbus_message_get_interface (dmessage));
  strcpy (member,    dbus_message_get_member (dmessage));

  /* Search for a registered function of the message.  */
  key = list3 (bus, build_string (interface), build_string (member));
  value = Fgethash (key, Vdbus_registered_functions_table, Qnil);

  /* Loop over the registered functions.  Construct an event.  */
  while (!NILP (value))
    {
      key = XCAR (value);
      /* key has the structure (UNAME SERVICE PATH HANDLER).  */
      if (((uname == NULL)
	   || (NILP (XCAR (key)))
	   || (strcmp (uname, SDATA (XCAR (key))) == 0))
	  && ((path == NULL)
	      || (NILP (XCAR (XCDR (XCDR (key)))))
	      || (strcmp (path, SDATA (XCAR (XCDR (XCDR (key))))) == 0))
	  && (!NILP (XCAR (XCDR (XCDR (XCDR (key)))))))
	{
	  EVENT_INIT (event);
	  event.kind = DBUS_EVENT;
	  event.frame_or_window = Qnil;
	  event.arg = Fcons (XCAR (XCDR (XCDR (XCDR (key)))), args);

	  /* Add uname, path, interface and member to the event.  */
	  event.arg = Fcons ((member == NULL ? Qnil : build_string (member)),
			     event.arg);
	  event.arg = Fcons ((interface == NULL
			      ? Qnil : build_string (interface)),
			     event.arg);
	  event.arg = Fcons ((path == NULL ? Qnil : build_string (path)),
			     event.arg);
	  event.arg = Fcons ((uname == NULL ? Qnil : build_string (uname)),
			     event.arg);

	  /* Add the bus symbol to the event.  */
	  event.arg = Fcons (bus, event.arg);

	  /* Store it into the input event queue.  */
	  kbd_buffer_store_event (&event);
	}
     value = XCDR (value);
    }

  /* Cleanup.  */
  dbus_message_unref (dmessage);
  UNGCPRO;
}

/* Read queued incoming messages from the system and session buses.  */
void
xd_read_queued_messages ()
{

  /* Vdbus_registered_functions_table will be initialized as hash
     table in dbus.el.  When this package isn't loaded yet, it doesn't
     make sense to handle D-Bus messages.  Furthermore, we ignore all
     Lisp errors during the call.  */
  if (HASH_TABLE_P (Vdbus_registered_functions_table))
    {
      internal_condition_case_1 (xd_read_message, QCdbus_system_bus,
				 Qerror, Fidentity);
      internal_condition_case_1 (xd_read_message, QCdbus_session_bus,
				 Qerror, Fidentity);
    }
}

DEFUN ("dbus-register-signal", Fdbus_register_signal, Sdbus_register_signal,
       6, 6, 0,
       doc: /* Register for signal SIGNAL on the D-Bus BUS.

BUS is either the symbol `:system' or the symbol `:session'.

SERVICE is the D-Bus service name used by the sending D-Bus object.
It can be either a known name or the unique name of the D-Bus object
sending the signal.  When SERVICE is nil, related signals from all
D-Bus objects shall be accepted.

PATH is the D-Bus object path SERVICE is registered.  It can also be
nil if the path name of incoming signals shall not be checked.

INTERFACE is an interface offered by SERVICE.  It must provide SIGNAL.
HANDLER is a Lisp function to be called when the signal is received.
It must accept as arguments the values SIGNAL is sending.  INTERFACE,
SIGNAL and HANDLER must not be nil.  Example:

\(defun my-signal-handler (device)
  (message "Device %s added" device))

\(dbus-register-signal
  :system "org.freedesktop.Hal" "/org/freedesktop/Hal/Manager"
  "org.freedesktop.Hal.Manager" "DeviceAdded" 'my-signal-handler)

  => ((:system "org.freedesktop.Hal.Manager" "DeviceAdded")
      ("org.freedesktop.Hal" "/org/freedesktop/Hal/Manager" my-signal-handler))

`dbus-register-signal' returns an object, which can be used in
`dbus-unregister-signal' for removing the registration.  */)
     (bus, service, path, interface, signal, handler)
     Lisp_Object bus, service, path, interface, signal, handler;
{
  Lisp_Object uname, key, value;
  DBusConnection *connection;
  char rule[DBUS_MAXIMUM_MATCH_RULE_LENGTH];
  DBusError derror;

  /* Check parameters.  */
  CHECK_SYMBOL (bus);
  if (!NILP (service)) CHECK_STRING (service);
  if (!NILP (path)) CHECK_STRING (path);
  CHECK_STRING (interface);
  CHECK_STRING (signal);
  FUNCTIONP (handler);

  /* Retrieve unique name of service.  If service is a known name, we
     will register for the corresponding unique name, if any.  Signals
     are sent always with the unique name as sender.  Note: the unique
     name of "org.freedesktop.DBus" is that string itself.  */
  if ((!NILP (service))
      && (strlen (SDATA (service)) > 0)
      && (strcmp (SDATA (service), DBUS_SERVICE_DBUS) != 0)
      && (strncmp (SDATA (service), ":", 1) != 0))
    {
      uname = call2 (intern ("dbus-get-name-owner"), bus, service);
      /* When there is no unique name, we mark it with an empty
	 string.  */
      if (NILP (uname))
	uname = build_string ("");
    }
  else
    uname = service;

  /* Create a matching rule if the unique name exists (when no
     wildcard).  */
  if (NILP (uname) || (strlen (SDATA (uname)) > 0))
    {
      /* Open a connection to the bus.  */
      connection = xd_initialize (bus);

      /* Create a rule to receive related signals.  */
      sprintf (rule,
	       "type='signal',interface='%s',member='%s'",
	       SDATA (interface),
	       SDATA (signal));

      /* Add unique name and path to the rule if they are non-nil.  */
      if (!NILP (uname))
	sprintf (rule, "%s,sender='%s'", rule, SDATA (uname));

      if (!NILP (path))
	sprintf (rule, "%s,path='%s'", rule, SDATA (path));

      /* Add the rule to the bus.  */
      dbus_error_init (&derror);
      dbus_bus_add_match (connection, rule, &derror);
      if (dbus_error_is_set (&derror))
	XD_ERROR (derror);

      XD_DEBUG_MESSAGE ("Matching rule \"%s\" created", rule);
    }

  /* Create a hash table entry.  */
  key = list3 (bus, interface, signal);
  value = Fgethash (key, Vdbus_registered_functions_table, Qnil);

  if (NILP (Fmember (list4 (uname, service, path, handler), value)))
    Fputhash (key,
	      Fcons (list4 (uname, service, path, handler), value),
	      Vdbus_registered_functions_table);

  /* Return object.  */
  return list2 (key, list3 (service, path, handler));
}

DEFUN ("dbus-unregister-signal", Fdbus_unregister_signal, Sdbus_unregister_signal,
       1, 1, 0,
       doc: /* Unregister OBJECT from the D-Bus.
OBJECT must be the result of a preceding `dbus-register-signal' call.  */)
     (object)
     Lisp_Object object;
{
  Lisp_Object value;
  struct gcpro gcpro1;

  /* Check parameter.  */
  CONSP (object) && (!NILP (XCAR (object))) && CONSP (XCDR (object));

  /* Find the corresponding entry in the hash table.  */
  value = Fgethash (XCAR (object), Vdbus_registered_functions_table, Qnil);

  /* Loop over the registered functions.  */
  while (!NILP (value))
    {
      GCPRO1 (value);

      /* (car value) has the structure (UNAME SERVICE PATH HANDLER).
	 (cdr object) has the structure ((SERVICE PATH HANDLER) ...).  */
      if (!NILP (Fequal (XCDR (XCAR (value)), XCAR (XCDR (object)))))
	{
	  /* Compute new hash value.  */
	  value = Fdelete (XCAR (value),
			   Fgethash (XCAR (object),
				     Vdbus_registered_functions_table, Qnil));
	  if (NILP (value))
	    Fremhash (XCAR (object), Vdbus_registered_functions_table);
	  else
	    Fputhash (XCAR (object), value, Vdbus_registered_functions_table);
	  RETURN_UNGCPRO (Qt);
	}
      UNGCPRO;
      value = XCDR (value);
    }

  /* Return.  */
  return Qnil;
}


void
syms_of_dbusbind ()
{

  Qdbus_get_unique_name = intern ("dbus-get-unique-name");
  staticpro (&Qdbus_get_unique_name);
  defsubr (&Sdbus_get_unique_name);

  Qdbus_call_method = intern ("dbus-call-method");
  staticpro (&Qdbus_call_method);
  defsubr (&Sdbus_call_method);

  Qdbus_send_signal = intern ("dbus-send-signal");
  staticpro (&Qdbus_send_signal);
  defsubr (&Sdbus_send_signal);

  Qdbus_register_signal = intern ("dbus-register-signal");
  staticpro (&Qdbus_register_signal);
  defsubr (&Sdbus_register_signal);

  Qdbus_unregister_signal = intern ("dbus-unregister-signal");
  staticpro (&Qdbus_unregister_signal);
  defsubr (&Sdbus_unregister_signal);

  Qdbus_error = intern ("dbus-error");
  staticpro (&Qdbus_error);
  Fput (Qdbus_error, Qerror_conditions,
	list2 (Qdbus_error, Qerror));
  Fput (Qdbus_error, Qerror_message,
	build_string ("D-Bus error"));

  QCdbus_system_bus = intern (":system");
  staticpro (&QCdbus_system_bus);

  QCdbus_session_bus = intern (":session");
  staticpro (&QCdbus_session_bus);

  QCdbus_type_byte = intern (":byte");
  staticpro (&QCdbus_type_byte);

  QCdbus_type_boolean = intern (":boolean");
  staticpro (&QCdbus_type_boolean);

  QCdbus_type_int16 = intern (":int16");
  staticpro (&QCdbus_type_int16);

  QCdbus_type_uint16 = intern (":uint16");
  staticpro (&QCdbus_type_uint16);

  QCdbus_type_int32 = intern (":int32");
  staticpro (&QCdbus_type_int32);

  QCdbus_type_uint32 = intern (":uint32");
  staticpro (&QCdbus_type_uint32);

  QCdbus_type_int64 = intern (":int64");
  staticpro (&QCdbus_type_int64);

  QCdbus_type_uint64 = intern (":uint64");
  staticpro (&QCdbus_type_uint64);

  QCdbus_type_double = intern (":double");
  staticpro (&QCdbus_type_double);

  QCdbus_type_string = intern (":string");
  staticpro (&QCdbus_type_string);

  QCdbus_type_object_path = intern (":object-path");
  staticpro (&QCdbus_type_object_path);

  QCdbus_type_signature = intern (":signature");
  staticpro (&QCdbus_type_signature);

  QCdbus_type_array = intern (":array");
  staticpro (&QCdbus_type_array);

  QCdbus_type_variant = intern (":variant");
  staticpro (&QCdbus_type_variant);

  QCdbus_type_struct = intern (":struct");
  staticpro (&QCdbus_type_struct);

  QCdbus_type_dict_entry = intern (":dict-entry");
  staticpro (&QCdbus_type_dict_entry);

  DEFVAR_LISP ("dbus-registered-functions-table", &Vdbus_registered_functions_table,
    doc: /* Hash table of registered functions for D-Bus.
The key in the hash table is the list (BUS INTERFACE MEMBER).  BUS is
either the symbol `:system' or the symbol `:session'.  INTERFACE is a
string which denotes a D-Bus interface, and MEMBER, also a string, is
either a method or a signal INTERFACE is offering.  All arguments but
BUS must not be nil.

The value in the hash table is a list of quadruple lists
\((UNAME SERVICE PATH HANDLER) (UNAME SERVICE PATH HANDLER) ...).
SERVICE is the service name as registered, UNAME is the corresponding
unique name.  PATH is the object path of the sending object.  All of
them can be nil, which means a wildcard then.  HANDLER is the function
to be called when a D-Bus message, which matches the key criteria,
arrives.  */);
  /* We initialize Vdbus_registered_functions_table in dbus.el,
     because we need to define a hash table function first.  */
  Vdbus_registered_functions_table = Qnil;

  DEFVAR_LISP ("dbus-debug", &Vdbus_debug,
    doc: /* If non-nil, debug messages of D-Bus bindings are raised.  */);
#ifdef DBUS_DEBUG
  Vdbus_debug = Qt;
#else
  Vdbus_debug = Qnil;
#endif

  Fprovide (intern ("dbusbind"), Qnil);

}

#endif /* HAVE_DBUS */

/* arch-tag: 0e828477-b571-4fe4-b559-5c9211bc14b8
   (do not change this comment) */
