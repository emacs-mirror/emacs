/* Elisp bindings for D-Bus.
   Copyright (C) 2007-2018 Free Software Foundation, Inc.

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

#include <config.h>

#ifdef HAVE_DBUS
#include <stdio.h>
#include <stdlib.h>
#include <dbus/dbus.h>

#include "lisp.h"
#include "termhooks.h"
#include "keyboard.h"
#include "process.h"

#ifndef DBUS_NUM_MESSAGE_TYPES
#define DBUS_NUM_MESSAGE_TYPES 5
#endif


/* Some platforms define the symbol "interface", but we want to use it
 * as a variable name below.  */

#ifdef interface
#undef interface
#endif


/* Alist of D-Bus buses we are polling for messages.
   The key is the symbol or string of the bus, and the value is the
   connection address.  */
static Lisp_Object xd_registered_buses;

/* Whether we are reading a D-Bus event.  */
static bool xd_in_read_queued_messages = 0;


/* We use "xd_" and "XD_" as prefix for all internal symbols, because
   we don't want to poison other namespaces with "dbus_".  */

/* Raise a signal.  If we are reading events, we cannot signal; we
   throw to xd_read_queued_messages then.  */
#define XD_SIGNAL1(arg)							\
  do {									\
    if (xd_in_read_queued_messages)					\
      Fthrow (Qdbus_error, Qnil);					\
    else								\
      xsignal1 (Qdbus_error, arg);					\
  } while (0)

#define XD_SIGNAL2(arg1, arg2)						\
  do {									\
    if (xd_in_read_queued_messages)					\
      Fthrow (Qdbus_error, Qnil);					\
    else								\
      xsignal2 (Qdbus_error, arg1, arg2);				\
  } while (0)

#define XD_SIGNAL3(arg1, arg2, arg3)					\
  do {									\
    if (xd_in_read_queued_messages)					\
      Fthrow (Qdbus_error, Qnil);					\
    else								\
      xsignal3 (Qdbus_error, arg1, arg2, arg3);				\
  } while (0)

/* Raise a Lisp error from a D-Bus ERROR.  */
#define XD_ERROR(error)							\
  do {									\
    /* Remove the trailing newline.  */					\
    char const *mess = error.message;					\
    char const *nl = strchr (mess, '\n');				\
    Lisp_Object err = make_string (mess, nl ? nl - mess : strlen (mess)); \
    dbus_error_free (&error);						\
    XD_SIGNAL1 (err);							\
  } while (0)

/* Macros for debugging.  In order to enable them, build with
   "make MYCPPFLAGS='-DDBUS_DEBUG'".  */
#ifdef DBUS_DEBUG
#define XD_DEBUG_MESSAGE(...)						\
  do {									\
    char s[1024];							\
    snprintf (s, sizeof s, __VA_ARGS__);				\
    if (!noninteractive)						\
      printf ("%s: %s\n", __func__, s);					\
    message ("%s: %s", __func__, s);					\
  } while (0)
#define XD_DEBUG_VALID_LISP_OBJECT_P(object)				\
  do {									\
    if (!valid_lisp_object_p (object))					\
      {									\
	XD_DEBUG_MESSAGE ("%d Assertion failure", __LINE__);		\
	XD_SIGNAL1 (build_string ("Assertion failure"));		\
      }									\
  } while (0)

#else /* !DBUS_DEBUG */
# define XD_DEBUG_MESSAGE(...)						\
  do {									\
    if (!NILP (Vdbus_debug))						\
      {									\
	char s[1024];							\
	snprintf (s, sizeof s, __VA_ARGS__);				\
	message ("%s: %s", __func__, s);				\
      }									\
  } while (0)
# define XD_DEBUG_VALID_LISP_OBJECT_P(object)
#endif

/* Check whether TYPE is a basic DBusType.  */
#ifdef HAVE_DBUS_TYPE_IS_VALID
#define XD_BASIC_DBUS_TYPE(type)					\
  (dbus_type_is_valid (type) && dbus_type_is_basic (type))
#else
#ifdef DBUS_TYPE_UNIX_FD
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
   || (type ==  DBUS_TYPE_SIGNATURE)					\
   || (type ==  DBUS_TYPE_UNIX_FD))
#else
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
#endif
#endif

/* This was a macro.  On Solaris 2.11 it was said to compile for
   hours, when optimization is enabled.  So we have transferred it into
   a function.  */
/* Determine the DBusType of a given Lisp symbol.  OBJECT must be one
   of the predefined D-Bus type symbols.  */
static int
xd_symbol_to_dbus_type (Lisp_Object object)
{
  return
    (EQ (object, QCbyte) ? DBUS_TYPE_BYTE
     : EQ (object, QCboolean) ? DBUS_TYPE_BOOLEAN
     : EQ (object, QCint16) ? DBUS_TYPE_INT16
     : EQ (object, QCuint16) ? DBUS_TYPE_UINT16
     : EQ (object, QCint32) ? DBUS_TYPE_INT32
     : EQ (object, QCuint32) ? DBUS_TYPE_UINT32
     : EQ (object, QCint64) ? DBUS_TYPE_INT64
     : EQ (object, QCuint64) ? DBUS_TYPE_UINT64
     : EQ (object, QCdouble) ? DBUS_TYPE_DOUBLE
     : EQ (object, QCstring) ? DBUS_TYPE_STRING
     : EQ (object, QCobject_path) ? DBUS_TYPE_OBJECT_PATH
     : EQ (object, QCsignature) ? DBUS_TYPE_SIGNATURE
#ifdef DBUS_TYPE_UNIX_FD
     : EQ (object, QCunix_fd) ? DBUS_TYPE_UNIX_FD
#endif
     : EQ (object, QCarray) ? DBUS_TYPE_ARRAY
     : EQ (object, QCvariant) ? DBUS_TYPE_VARIANT
     : EQ (object, QCstruct) ? DBUS_TYPE_STRUCT
     : EQ (object, QCdict_entry) ? DBUS_TYPE_DICT_ENTRY
     : DBUS_TYPE_INVALID);
}

/* Check whether a Lisp symbol is a predefined D-Bus type symbol.  */
#define XD_DBUS_TYPE_P(object)						\
  (SYMBOLP (object) && ((xd_symbol_to_dbus_type (object) != DBUS_TYPE_INVALID)))

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
   : (XD_DBUS_TYPE_P (object)) ? xd_symbol_to_dbus_type (object)	\
   : (CONSP (object))							\
   ? ((XD_DBUS_TYPE_P (CAR_SAFE (object)))				\
      ? ((XD_BASIC_DBUS_TYPE (xd_symbol_to_dbus_type (CAR_SAFE (object)))) \
	 ? DBUS_TYPE_ARRAY						\
	 : xd_symbol_to_dbus_type (CAR_SAFE (object)))			\
      : DBUS_TYPE_ARRAY)						\
   : DBUS_TYPE_INVALID)

/* Return a list pointer which does not have a Lisp symbol as car.  */
#define XD_NEXT_VALUE(object)						\
  ((XD_DBUS_TYPE_P (CAR_SAFE (object))) ? CDR_SAFE (object) : object)

/* Transform the message type to its string representation for debug
   messages.  */
#define XD_MESSAGE_TYPE_TO_STRING(mtype)				\
  ((mtype == DBUS_MESSAGE_TYPE_INVALID)					\
  ? "DBUS_MESSAGE_TYPE_INVALID"						\
  : (mtype == DBUS_MESSAGE_TYPE_METHOD_CALL)				\
  ? "DBUS_MESSAGE_TYPE_METHOD_CALL"					\
  : (mtype == DBUS_MESSAGE_TYPE_METHOD_RETURN)				\
  ? "DBUS_MESSAGE_TYPE_METHOD_RETURN"					\
  : (mtype == DBUS_MESSAGE_TYPE_ERROR)					\
   ? "DBUS_MESSAGE_TYPE_ERROR"						\
   : "DBUS_MESSAGE_TYPE_SIGNAL")

/* Transform the object to its string representation for debug
   messages.  */
static char *
XD_OBJECT_TO_STRING (Lisp_Object object)
{
  AUTO_STRING (format, "%s");
  return SSDATA (CALLN (Fformat, format, object));
}

#define XD_DBUS_VALIDATE_BUS_ADDRESS(bus)				\
  do {									\
    char const *session_bus_address = getenv ("DBUS_SESSION_BUS_ADDRESS"); \
    if (STRINGP (bus))							\
      {									\
	DBusAddressEntry **entries;					\
	int len;							\
	DBusError derror;						\
	dbus_error_init (&derror);					\
	if (!dbus_parse_address (SSDATA (bus), &entries, &len, &derror)) \
	  XD_ERROR (derror);						\
	/* Cleanup.  */							\
	dbus_error_free (&derror);					\
	dbus_address_entries_free (entries);				\
	/* Canonicalize session bus address.  */			\
	if ((session_bus_address != NULL)				\
	    && (!NILP (Fstring_equal					\
		       (bus, build_string (session_bus_address)))))	\
	  bus = QCsession;						\
      }									\
									\
    else								\
      {									\
	CHECK_SYMBOL (bus);						\
	if (!(EQ (bus, QCsystem) || EQ (bus, QCsession)))		\
	  XD_SIGNAL2 (build_string ("Wrong bus name"), bus);		\
	/* We do not want to have an autolaunch for the session bus.  */ \
	if (EQ (bus, QCsession) && session_bus_address == NULL)		\
	  XD_SIGNAL2 (build_string ("No connection to bus"), bus);	\
      }									\
  } while (0)

#if (HAVE_DBUS_VALIDATE_BUS_NAME || HAVE_DBUS_VALIDATE_PATH		\
     || HAVE_DBUS_VALIDATE_INTERFACE || HAVE_DBUS_VALIDATE_MEMBER)
#define XD_DBUS_VALIDATE_OBJECT(object, func)				\
  do {									\
    if (!NILP (object))							\
      {									\
	DBusError derror;						\
	CHECK_STRING (object);						\
	dbus_error_init (&derror);					\
	if (!func (SSDATA (object), &derror))				\
	  XD_ERROR (derror);						\
	/* Cleanup.  */							\
	dbus_error_free (&derror);					\
      }									\
  } while (0)
#endif

#if HAVE_DBUS_VALIDATE_BUS_NAME
#define XD_DBUS_VALIDATE_BUS_NAME(bus_name)				\
  XD_DBUS_VALIDATE_OBJECT(bus_name, dbus_validate_bus_name);
#else
#define XD_DBUS_VALIDATE_BUS_NAME(bus_name)				\
  if (!NILP (bus_name)) CHECK_STRING (bus_name);
#endif

#if HAVE_DBUS_VALIDATE_PATH
#define XD_DBUS_VALIDATE_PATH(path)					\
  XD_DBUS_VALIDATE_OBJECT(path, dbus_validate_path);
#else
#define XD_DBUS_VALIDATE_PATH(path)					\
  if (!NILP (path)) CHECK_STRING (path);
#endif

#if HAVE_DBUS_VALIDATE_INTERFACE
#define XD_DBUS_VALIDATE_INTERFACE(interface)				\
  XD_DBUS_VALIDATE_OBJECT(interface, dbus_validate_interface);
#else
#define XD_DBUS_VALIDATE_INTERFACE(interface)				\
  if (!NILP (interface)) CHECK_STRING (interface);
#endif

#if HAVE_DBUS_VALIDATE_MEMBER
#define XD_DBUS_VALIDATE_MEMBER(member)					\
  XD_DBUS_VALIDATE_OBJECT(member, dbus_validate_member);
#else
#define XD_DBUS_VALIDATE_MEMBER(member)					\
  if (!NILP (member)) CHECK_STRING (member);
#endif

/* Append to SIGNATURE a copy of X, making sure SIGNATURE does
   not become too long.  */
static void
xd_signature_cat (char *signature, char const *x)
{
  ptrdiff_t siglen = strlen (signature);
  ptrdiff_t xlen = strlen (x);
  if (DBUS_MAXIMUM_SIGNATURE_LENGTH - xlen <= siglen)
    string_overflow ();
  strcpy (signature + siglen, x);
}

/* Compute SIGNATURE of OBJECT.  It must have a form that it can be
   used in dbus_message_iter_open_container.  DTYPE is the DBusType
   the object is related to.  It is passed as argument, because it
   cannot be detected in basic type objects, when they are preceded by
   a type symbol.  PARENT_TYPE is the DBusType of a container this
   signature is embedded, or DBUS_TYPE_INVALID.  It is needed for the
   check that DBUS_TYPE_DICT_ENTRY occurs only as array element.  */
static void
xd_signature (char *signature, int dtype, int parent_type, Lisp_Object object)
{
  int subtype;
  Lisp_Object elt;
  char const *subsig;
  int subsiglen;
  char x[DBUS_MAXIMUM_SIGNATURE_LENGTH];

  elt = object;

  switch (dtype)
    {
    case DBUS_TYPE_BYTE:
    case DBUS_TYPE_UINT16:
      CHECK_NATNUM (object);
      sprintf (signature, "%c", dtype);
      break;

    case DBUS_TYPE_BOOLEAN:
      if (!EQ (object, Qt) && !EQ (object, Qnil))
	wrong_type_argument (intern ("booleanp"), object);
      sprintf (signature, "%c", dtype);
      break;

    case DBUS_TYPE_INT16:
      CHECK_NUMBER (object);
      sprintf (signature, "%c", dtype);
      break;

    case DBUS_TYPE_UINT32:
    case DBUS_TYPE_UINT64:
#ifdef DBUS_TYPE_UNIX_FD
    case DBUS_TYPE_UNIX_FD:
#endif
    case DBUS_TYPE_INT32:
    case DBUS_TYPE_INT64:
    case DBUS_TYPE_DOUBLE:
      CHECK_NUMBER_OR_FLOAT (object);
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

      /* Type symbol is optional.  */
      if (EQ (QCarray, CAR_SAFE (elt)))
	elt = XD_NEXT_VALUE (elt);

      /* If the array is empty, DBUS_TYPE_STRING is the default
	 element type.  */
      if (NILP (elt))
	{
	  subtype = DBUS_TYPE_STRING;
	  subsig = DBUS_TYPE_STRING_AS_STRING;
	}
      else
	{
	  subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
	  xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));
	  subsig = x;
	}

      /* If the element type is DBUS_TYPE_SIGNATURE, and this is the
	 only element, the value of this element is used as the
	 array's element signature.  */
      if ((subtype == DBUS_TYPE_SIGNATURE)
	  && STRINGP (CAR_SAFE (XD_NEXT_VALUE (elt)))
	  && NILP (CDR_SAFE (XD_NEXT_VALUE (elt))))
	subsig = SSDATA (CAR_SAFE (XD_NEXT_VALUE (elt)));

      while (!NILP (elt))
	{
	  if (subtype != XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt)))
	    wrong_type_argument (intern ("D-Bus"), CAR_SAFE (elt));
	  elt = CDR_SAFE (XD_NEXT_VALUE (elt));
	}

      subsiglen = snprintf (signature, DBUS_MAXIMUM_SIGNATURE_LENGTH,
			    "%c%s", dtype, subsig);
      if (! (0 <= subsiglen && subsiglen < DBUS_MAXIMUM_SIGNATURE_LENGTH))
	string_overflow ();
      break;

    case DBUS_TYPE_VARIANT:
      /* Check that there is exactly one list element.  */
      CHECK_CONS (object);

      elt = XD_NEXT_VALUE (elt);
      subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
      xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));

      if (!NILP (CDR_SAFE (XD_NEXT_VALUE (elt))))
	wrong_type_argument (intern ("D-Bus"),
			     CAR_SAFE (CDR_SAFE (XD_NEXT_VALUE (elt))));

      sprintf (signature, "%c", dtype);
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
	  subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
	  xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));
	  xd_signature_cat (signature, x);
	  elt = CDR_SAFE (XD_NEXT_VALUE (elt));
	}
      xd_signature_cat (signature, DBUS_STRUCT_END_CHAR_AS_STRING);
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
      subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
      xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));
      xd_signature_cat (signature, x);

      if (!XD_BASIC_DBUS_TYPE (subtype))
	wrong_type_argument (intern ("D-Bus"), CAR_SAFE (XD_NEXT_VALUE (elt)));

      /* Second element.  */
      elt = CDR_SAFE (XD_NEXT_VALUE (elt));
      subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
      xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));
      xd_signature_cat (signature, x);

      if (!NILP (CDR_SAFE (XD_NEXT_VALUE (elt))))
	wrong_type_argument (intern ("D-Bus"),
			     CAR_SAFE (CDR_SAFE (XD_NEXT_VALUE (elt))));

      /* Closing signature.  */
      xd_signature_cat (signature, DBUS_DICT_ENTRY_END_CHAR_AS_STRING);
      break;

    default:
      wrong_type_argument (intern ("D-Bus"), object);
    }

  XD_DEBUG_MESSAGE ("%s", signature);
}

/* Convert X to a signed integer with bounds LO and HI.  */
static intmax_t
xd_extract_signed (Lisp_Object x, intmax_t lo, intmax_t hi)
{
  CHECK_NUMBER_OR_FLOAT (x);
  if (INTEGERP (x))
    {
      if (lo <= XINT (x) && XINT (x) <= hi)
	return XINT (x);
    }
  else
    {
      double d = XFLOAT_DATA (x);
      if (lo <= d && d < 1.0 + hi)
	{
	  intmax_t n = d;
	  if (n == d)
	    return n;
	}
    }
  if (xd_in_read_queued_messages)
    Fthrow (Qdbus_error, Qnil);
  else
    args_out_of_range_3 (x,
			 make_fixnum_or_float (lo),
			 make_fixnum_or_float (hi));
}

/* Convert X to an unsigned integer with bounds 0 and HI.  */
static uintmax_t
xd_extract_unsigned (Lisp_Object x, uintmax_t hi)
{
  CHECK_NUMBER_OR_FLOAT (x);
  if (INTEGERP (x))
    {
      if (0 <= XINT (x) && XINT (x) <= hi)
	return XINT (x);
    }
  else
    {
      double d = XFLOAT_DATA (x);
      if (0 <= d && d < 1.0 + hi)
	{
	  uintmax_t n = d;
	  if (n == d)
	    return n;
	}
    }
  if (xd_in_read_queued_messages)
    Fthrow (Qdbus_error, Qnil);
  else
    args_out_of_range_3 (x, make_number (0), make_fixnum_or_float (hi));
}

/* Append C value, extracted from Lisp OBJECT, to iteration ITER.
   DTYPE must be a valid DBusType.  It is used to convert Lisp
   objects, being arguments of `dbus-call-method' or
   `dbus-send-signal', into corresponding C values appended as
   arguments to a D-Bus message.  */
static void
xd_append_arg (int dtype, Lisp_Object object, DBusMessageIter *iter)
{
  char signature[DBUS_MAXIMUM_SIGNATURE_LENGTH];
  DBusMessageIter subiter;

  if (XD_BASIC_DBUS_TYPE (dtype))
    switch (dtype)
      {
      case DBUS_TYPE_BYTE:
	CHECK_NATNUM (object);
	{
	  unsigned char val = XFASTINT (object) & 0xFF;
	  XD_DEBUG_MESSAGE ("%c %u", dtype, val);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_BOOLEAN:
	{
	  dbus_bool_t val = (NILP (object)) ? FALSE : TRUE;
	  XD_DEBUG_MESSAGE ("%c %s", dtype, (val == FALSE) ? "false" : "true");
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_INT16:
	{
	  dbus_int16_t val =
	    xd_extract_signed (object,
			       TYPE_MINIMUM (dbus_int16_t),
			       TYPE_MAXIMUM (dbus_int16_t));
	  int pval = val;
	  XD_DEBUG_MESSAGE ("%c %d", dtype, pval);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_UINT16:
	{
	  dbus_uint16_t val =
	    xd_extract_unsigned (object,
				 TYPE_MAXIMUM (dbus_uint16_t));
	  unsigned int pval = val;
	  XD_DEBUG_MESSAGE ("%c %u", dtype, pval);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_INT32:
	{
	  dbus_int32_t val =
	    xd_extract_signed (object,
			       TYPE_MINIMUM (dbus_int32_t),
			       TYPE_MAXIMUM (dbus_int32_t));
	  int pval = val;
	  XD_DEBUG_MESSAGE ("%c %d", dtype, pval);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_UINT32:
#ifdef DBUS_TYPE_UNIX_FD
      case DBUS_TYPE_UNIX_FD:
#endif
	{
	  dbus_uint32_t val =
	    xd_extract_unsigned (object,
				 TYPE_MAXIMUM (dbus_uint32_t));
	  unsigned int pval = val;
	  XD_DEBUG_MESSAGE ("%c %u", dtype, pval);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_INT64:
	{
	  dbus_int64_t val =
	    xd_extract_signed (object,
			       TYPE_MINIMUM (dbus_int64_t),
			       TYPE_MAXIMUM (dbus_int64_t));
	  printmax_t pval = val;
	  XD_DEBUG_MESSAGE ("%c %"pMd, dtype, pval);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_UINT64:
	{
	  dbus_uint64_t val =
	    xd_extract_unsigned (object,
				 TYPE_MAXIMUM (dbus_uint64_t));
	  uprintmax_t pval = val;
	  XD_DEBUG_MESSAGE ("%c %"pMu, dtype, pval);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_DOUBLE:
	{
	  double val = extract_float (object);
	  XD_DEBUG_MESSAGE ("%c %f", dtype, val);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_STRING:
      case DBUS_TYPE_OBJECT_PATH:
      case DBUS_TYPE_SIGNATURE:
	CHECK_STRING (object);
	{
	  /* We need to send a valid UTF-8 string.  We could encode `object'
	     but by not encoding it, we guarantee it's valid utf-8, even if
	     it contains eight-bit-bytes.  Of course, you can still send
	     manually-crafted junk by passing a unibyte string.  */
	  char *val = SSDATA (object);
	  XD_DEBUG_MESSAGE ("%c %s", dtype, val);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}
      }

  else /* Compound types.  */
    {

      /* All compound types except array have a type symbol.  For
	 array, it is optional.  Skip it.  */
      if (!XD_BASIC_DBUS_TYPE (XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (object))))
	object = XD_NEXT_VALUE (object);

      /* Open new subiteration.  */
      switch (dtype)
	{
	case DBUS_TYPE_ARRAY:
	  /* An array has only elements of the same type.  So it is
	     sufficient to check the first element's signature
	     only.  */

	  if (NILP (object))
	    /* If the array is empty, DBUS_TYPE_STRING is the default
	       element type.  */
	    strcpy (signature, DBUS_TYPE_STRING_AS_STRING);

	  else
	    /* If the element type is DBUS_TYPE_SIGNATURE, and this is
	       the only element, the value of this element is used as
	       the array's element signature.  */
	    if ((XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (object))
		 == DBUS_TYPE_SIGNATURE)
		&& STRINGP (CAR_SAFE (XD_NEXT_VALUE (object)))
		&& NILP (CDR_SAFE (XD_NEXT_VALUE (object))))
	      {
		lispstpcpy (signature, CAR_SAFE (XD_NEXT_VALUE (object)));
		object = CDR_SAFE (XD_NEXT_VALUE (object));
	      }

	    else
	      xd_signature (signature,
			    XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (object)),
			    dtype, CAR_SAFE (XD_NEXT_VALUE (object)));

	  XD_DEBUG_MESSAGE ("%c %s %s", dtype, signature,
			    XD_OBJECT_TO_STRING (object));
	  if (!dbus_message_iter_open_container (iter, dtype,
						 signature, &subiter))
	    XD_SIGNAL3 (build_string ("Cannot open container"),
			make_number (dtype), build_string (signature));
	  break;

	case DBUS_TYPE_VARIANT:
	  /* A variant has just one element.  */
	  xd_signature (signature, XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (object)),
			dtype, CAR_SAFE (XD_NEXT_VALUE (object)));

	  XD_DEBUG_MESSAGE ("%c %s %s", dtype, signature,
			    XD_OBJECT_TO_STRING (object));
	  if (!dbus_message_iter_open_container (iter, dtype,
						 signature, &subiter))
	    XD_SIGNAL3 (build_string ("Cannot open container"),
			make_number (dtype), build_string (signature));
	  break;

	case DBUS_TYPE_STRUCT:
	case DBUS_TYPE_DICT_ENTRY:
	  /* These containers do not require a signature.  */
	  XD_DEBUG_MESSAGE ("%c %s", dtype, XD_OBJECT_TO_STRING (object));
	  if (!dbus_message_iter_open_container (iter, dtype, NULL, &subiter))
	    XD_SIGNAL2 (build_string ("Cannot open container"),
			make_number (dtype));
	  break;
	}

      /* Loop over list elements.  */
      while (!NILP (object))
	{
	  dtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (object));
	  object = XD_NEXT_VALUE (object);

	  xd_append_arg (dtype, CAR_SAFE (object), &subiter);

	  object = CDR_SAFE (object);
	}

      /* Close the subiteration.  */
      if (!dbus_message_iter_close_container (iter, &subiter))
	XD_SIGNAL2 (build_string ("Cannot close container"),
		    make_number (dtype));
    }
}

/* Retrieve C value from a DBusMessageIter structure ITER, and return
   a converted Lisp object.  The type DTYPE of the argument of the
   D-Bus message must be a valid DBusType.  Compound D-Bus types
   result always in a Lisp list.  */
static Lisp_Object
xd_retrieve_arg (int dtype, DBusMessageIter *iter)
{

  switch (dtype)
    {
    case DBUS_TYPE_BYTE:
      {
	unsigned int val;
	dbus_message_iter_get_basic (iter, &val);
	val = val & 0xFF;
	XD_DEBUG_MESSAGE ("%c %u", dtype, val);
	return make_number (val);
      }

    case DBUS_TYPE_BOOLEAN:
      {
	dbus_bool_t val;
	dbus_message_iter_get_basic (iter, &val);
	XD_DEBUG_MESSAGE ("%c %s", dtype, (val == FALSE) ? "false" : "true");
	return (val == FALSE) ? Qnil : Qt;
      }

    case DBUS_TYPE_INT16:
      {
	dbus_int16_t val;
	int pval;
	dbus_message_iter_get_basic (iter, &val);
	pval = val;
	XD_DEBUG_MESSAGE ("%c %d", dtype, pval);
	return make_number (val);
      }

    case DBUS_TYPE_UINT16:
      {
	dbus_uint16_t val;
	int pval;
	dbus_message_iter_get_basic (iter, &val);
	pval = val;
	XD_DEBUG_MESSAGE ("%c %d", dtype, pval);
	return make_number (val);
      }

    case DBUS_TYPE_INT32:
      {
	dbus_int32_t val;
	int pval;
	dbus_message_iter_get_basic (iter, &val);
	pval = val;
	XD_DEBUG_MESSAGE ("%c %d", dtype, pval);
	return make_fixnum_or_float (val);
      }

    case DBUS_TYPE_UINT32:
#ifdef DBUS_TYPE_UNIX_FD
    case DBUS_TYPE_UNIX_FD:
#endif
      {
	dbus_uint32_t val;
	unsigned int pval;
	dbus_message_iter_get_basic (iter, &val);
	pval = val;
	XD_DEBUG_MESSAGE ("%c %u", dtype, pval);
	return make_fixnum_or_float (val);
      }

    case DBUS_TYPE_INT64:
      {
	dbus_int64_t val;
	printmax_t pval;
	dbus_message_iter_get_basic (iter, &val);
	pval = val;
	XD_DEBUG_MESSAGE ("%c %"pMd, dtype, pval);
	return make_fixnum_or_float (val);
      }

    case DBUS_TYPE_UINT64:
      {
	dbus_uint64_t val;
	uprintmax_t pval;
	dbus_message_iter_get_basic (iter, &val);
	pval = val;
	XD_DEBUG_MESSAGE ("%c %"pMu, dtype, pval);
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
	DBusMessageIter subiter;
	int subtype;
	result = Qnil;
	dbus_message_iter_recurse (iter, &subiter);
	while ((subtype = dbus_message_iter_get_arg_type (&subiter))
	       != DBUS_TYPE_INVALID)
	  {
	    result = Fcons (xd_retrieve_arg (subtype, &subiter), result);
	    dbus_message_iter_next (&subiter);
	  }
	XD_DEBUG_MESSAGE ("%c %s", dtype, XD_OBJECT_TO_STRING (result));
	return Fnreverse (result);
      }

    default:
      XD_DEBUG_MESSAGE ("DBusType '%c' not supported", dtype);
      return Qnil;
    }
}

/* Return the number of references of the shared CONNECTION.  */
static ptrdiff_t
xd_get_connection_references (DBusConnection *connection)
{
  ptrdiff_t *refcount;

  /* We cannot access the DBusConnection structure, it is not public.
     But we know, that the reference counter is the first field in
     that structure.  */
  refcount = (void *) &connection;
  refcount =  (void *) *refcount;
  return *refcount;
}

/* Convert a Lisp D-Bus object to a pointer.  */
static DBusConnection *
xd_lisp_dbus_to_dbus (Lisp_Object bus)
{
  return (DBusConnection *) XSAVE_POINTER (bus, 0);
}

/* Return D-Bus connection address.  BUS is either a Lisp symbol,
   :system or :session, or a string denoting the bus address.  */
static DBusConnection *
xd_get_connection_address (Lisp_Object bus)
{
  DBusConnection *connection;
  Lisp_Object val;

  val = CDR_SAFE (Fassoc (bus, xd_registered_buses, Qnil));
  if (NILP (val))
    XD_SIGNAL2 (build_string ("No connection to bus"), bus);
  else
    connection = xd_lisp_dbus_to_dbus (val);

  if (!dbus_connection_get_is_connected (connection))
    XD_SIGNAL2 (build_string ("No connection to bus"), bus);

  return connection;
}

/* Return the file descriptor for WATCH, -1 if not found.  */
static int
xd_find_watch_fd (DBusWatch *watch)
{
#if HAVE_DBUS_WATCH_GET_UNIX_FD
  /* TODO: Reverse these on w32, which prefers the opposite.  */
  int fd = dbus_watch_get_unix_fd (watch);
  if (fd == -1)
    fd = dbus_watch_get_socket (watch);
#else
  int fd = dbus_watch_get_fd (watch);
#endif
  return fd;
}

/* Prototype.  */
static void xd_read_queued_messages (int fd, void *data);

/* Start monitoring WATCH for possible I/O.  */
static dbus_bool_t
xd_add_watch (DBusWatch *watch, void *data)
{
  unsigned int flags = dbus_watch_get_flags (watch);
  int fd = xd_find_watch_fd (watch);

  XD_DEBUG_MESSAGE ("fd %d, write %u, enabled %u",
		    fd, flags & DBUS_WATCH_WRITABLE,
		    dbus_watch_get_enabled (watch));

  if (fd == -1)
    return FALSE;

  if (dbus_watch_get_enabled (watch))
    {
      if (flags & DBUS_WATCH_WRITABLE)
        add_write_fd (fd, xd_read_queued_messages, data);
      if (flags & DBUS_WATCH_READABLE)
        add_read_fd (fd, xd_read_queued_messages, data);
    }
  return TRUE;
}

/* Stop monitoring WATCH for possible I/O.
   DATA is the used bus, either a string or QCsystem or QCsession.  */
static void
xd_remove_watch (DBusWatch *watch, void *data)
{
  unsigned int flags = dbus_watch_get_flags (watch);
  int fd = xd_find_watch_fd (watch);

  XD_DEBUG_MESSAGE ("fd %d", fd);

  if (fd == -1)
    return;

  /* Unset session environment.  */
#if 0
  /* This is buggy, since unsetenv is not thread-safe.  */
  if (XSYMBOL (QCsession) == data)
    {
      XD_DEBUG_MESSAGE ("unsetenv DBUS_SESSION_BUS_ADDRESS");
      unsetenv ("DBUS_SESSION_BUS_ADDRESS");
    }
#endif

  if (flags & DBUS_WATCH_WRITABLE)
    delete_write_fd (fd);
  if (flags & DBUS_WATCH_READABLE)
    delete_read_fd (fd);
}

/* Toggle monitoring WATCH for possible I/O.  */
static void
xd_toggle_watch (DBusWatch *watch, void *data)
{
  if (dbus_watch_get_enabled (watch))
    xd_add_watch (watch, data);
  else
    xd_remove_watch (watch, data);
}

/* Close connection to D-Bus BUS.  */
static void
xd_close_bus (Lisp_Object bus)
{
  DBusConnection *connection;
  Lisp_Object val;
  Lisp_Object busobj;

  /* Check whether we are connected.  */
  val = Fassoc (bus, xd_registered_buses, Qnil);
  if (NILP (val))
    return;

  busobj = CDR_SAFE (val);
  if (NILP (busobj)) {
    xd_registered_buses = Fdelete (val, xd_registered_buses);
    return;
  }

  /* Retrieve bus address.  */
  connection = xd_lisp_dbus_to_dbus (busobj);

  if (xd_get_connection_references (connection) == 1)
    {
      /* Close connection, if there isn't another shared application.  */
      XD_DEBUG_MESSAGE ("Close connection to bus %s",
			XD_OBJECT_TO_STRING (bus));
      dbus_connection_close (connection);

      xd_registered_buses = Fdelete (val, xd_registered_buses);
    }

  else
    /* Decrement reference count.  */
    dbus_connection_unref (connection);

  /* Return.  */
  return;
}

DEFUN ("dbus--init-bus", Fdbus__init_bus, Sdbus__init_bus, 1, 2, 0,
       doc: /* Establish the connection to D-Bus BUS.

This function is dbus internal.  You almost certainly want to use
`dbus-init-bus'.

BUS can be either the symbol `:system' or the symbol `:session', or it
can be a string denoting the address of the corresponding bus.  For
the system and session buses, this function is called when loading
`dbus.el', there is no need to call it again.

The function returns a number, which counts the connections this Emacs
session has established to the BUS under the same unique name (see
`dbus-get-unique-name').  It depends on the libraries Emacs is linked
with, and on the environment Emacs is running.  For example, if Emacs
is linked with the gtk toolkit, and it runs in a GTK-aware environment
like Gnome, another connection might already be established.

When PRIVATE is non-nil, a new connection is established instead of
reusing an existing one.  It results in a new unique name at the bus.
This can be used, if it is necessary to distinguish from another
connection used in the same Emacs process, like the one established by
GTK+.  It should be used with care for at least the `:system' and
`:session' buses, because other Emacs Lisp packages might already use
this connection to those buses.  */)
  (Lisp_Object bus, Lisp_Object private)
{
  DBusConnection *connection;
  DBusError derror;
  Lisp_Object val;
  ptrdiff_t refcount;

  /* Check parameter.  */
  XD_DBUS_VALIDATE_BUS_ADDRESS (bus);

  /* Close bus if it is already open.  */
  xd_close_bus (bus);

  /* Check, whether we are still connected.  */
  val = Fassoc (bus, xd_registered_buses, Qnil);
  if (!NILP (val))
    {
      connection = xd_get_connection_address (bus);
      dbus_connection_ref (connection);
    }

  else
    {
      /* Initialize.  */
      dbus_error_init (&derror);

      /* Open the connection.  */
      if (STRINGP (bus))
	if (NILP (private))
	  connection = dbus_connection_open (SSDATA (bus), &derror);
	else
	  connection = dbus_connection_open_private (SSDATA (bus), &derror);

      else
	{
	  DBusBusType bustype = (EQ (bus, QCsystem)
				 ? DBUS_BUS_SYSTEM : DBUS_BUS_SESSION);
	  if (NILP (private))
	    connection = dbus_bus_get (bustype, &derror);
	  else
	    connection = dbus_bus_get_private (bustype, &derror);
	}

      if (dbus_error_is_set (&derror))
	XD_ERROR (derror);

      if (connection == NULL)
	XD_SIGNAL2 (build_string ("No connection to bus"), bus);

      /* If it is not the system or session bus, we must register
	 ourselves.  Otherwise, we have called dbus_bus_get, which has
	 configured us to exit if the connection closes - we undo this
	 setting.  */
      if (STRINGP (bus))
	dbus_bus_register (connection, &derror);
      else
	dbus_connection_set_exit_on_disconnect (connection, FALSE);

      if (dbus_error_is_set (&derror))
	XD_ERROR (derror);

      /* Add the watch functions.  We pass also the bus as data, in
	 order to distinguish between the buses in xd_remove_watch.  */
      if (!dbus_connection_set_watch_functions (connection,
						xd_add_watch,
						xd_remove_watch,
						xd_toggle_watch,
						SYMBOLP (bus)
						? (void *) XSYMBOL (bus)
						: (void *) XSTRING (bus),
						NULL))
	XD_SIGNAL1 (build_string ("Cannot add watch functions"));

      /* Add bus to list of registered buses.  */
      val = make_save_ptr (connection);
      xd_registered_buses = Fcons (Fcons (bus, val), xd_registered_buses);

      /* Cleanup.  */
      dbus_error_free (&derror);
    }

  /* Return reference counter.  */
  refcount = xd_get_connection_references (connection);
  XD_DEBUG_MESSAGE ("Bus %s, Reference counter %"pD"d",
		    XD_OBJECT_TO_STRING (bus), refcount);
  return make_number (refcount);
}

DEFUN ("dbus-get-unique-name", Fdbus_get_unique_name, Sdbus_get_unique_name,
       1, 1, 0,
       doc: /* Return the unique name of Emacs registered at D-Bus BUS.  */)
  (Lisp_Object bus)
{
  DBusConnection *connection;
  const char *name;

  /* Check parameter.  */
  XD_DBUS_VALIDATE_BUS_ADDRESS (bus);

  /* Retrieve bus address.  */
  connection = xd_get_connection_address (bus);

  /* Request the name.  */
  name = dbus_bus_get_unique_name (connection);
  if (name == NULL)
    XD_SIGNAL1 (build_string ("No unique name available"));

  /* Return.  */
  return build_string (name);
}

DEFUN ("dbus-message-internal", Fdbus_message_internal, Sdbus_message_internal,
       4, MANY, 0,
       doc: /* Send a D-Bus message.
This is an internal function, it shall not be used outside dbus.el.

The following usages are expected:

`dbus-call-method', `dbus-call-method-asynchronously':
  (dbus-message-internal
    dbus-message-type-method-call BUS SERVICE PATH INTERFACE METHOD HANDLER
    &optional :timeout TIMEOUT &rest ARGS)

`dbus-send-signal':
  (dbus-message-internal
    dbus-message-type-signal BUS SERVICE PATH INTERFACE SIGNAL &rest ARGS)

`dbus-method-return-internal':
  (dbus-message-internal
    dbus-message-type-method-return BUS SERVICE SERIAL &rest ARGS)

`dbus-method-error-internal':
  (dbus-message-internal
    dbus-message-type-error BUS SERVICE SERIAL &rest ARGS)

usage: (dbus-message-internal &rest REST)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object message_type, bus, service, handler;
  Lisp_Object path = Qnil;
  Lisp_Object interface = Qnil;
  Lisp_Object member = Qnil;
  Lisp_Object result;
  DBusConnection *connection;
  DBusMessage *dmessage;
  DBusMessageIter iter;
  int dtype;
  int mtype;
  dbus_uint32_t serial = 0;
  unsigned int ui_serial;
  int timeout = -1;
  ptrdiff_t count;
  char signature[DBUS_MAXIMUM_SIGNATURE_LENGTH];

  /* Initialize parameters.  */
  message_type = args[0];
  bus = args[1];
  service = args[2];
  handler = Qnil;

  CHECK_NATNUM (message_type);
  if (! (DBUS_MESSAGE_TYPE_INVALID < XFASTINT (message_type)
	 && XFASTINT (message_type) < DBUS_NUM_MESSAGE_TYPES))
    XD_SIGNAL2 (build_string ("Invalid message type"), message_type);
  mtype = XFASTINT (message_type);

  if ((mtype == DBUS_MESSAGE_TYPE_METHOD_CALL)
      || (mtype == DBUS_MESSAGE_TYPE_SIGNAL))
    {
      path = args[3];
      interface = args[4];
      member = args[5];
      if (mtype == DBUS_MESSAGE_TYPE_METHOD_CALL)
	handler = args[6];
      count = (mtype == DBUS_MESSAGE_TYPE_METHOD_CALL) ? 7 : 6;
    }
  else /* DBUS_MESSAGE_TYPE_METHOD_RETURN, DBUS_MESSAGE_TYPE_ERROR  */
    {
      serial = xd_extract_unsigned (args[3], TYPE_MAXIMUM (dbus_uint32_t));
      count = 4;
    }

  /* Check parameters.  */
  XD_DBUS_VALIDATE_BUS_ADDRESS (bus);
  XD_DBUS_VALIDATE_BUS_NAME (service);
  if (nargs < count)
    xsignal2 (Qwrong_number_of_arguments,
	      Qdbus_message_internal,
	      make_number (nargs));

  if ((mtype == DBUS_MESSAGE_TYPE_METHOD_CALL)
      || (mtype == DBUS_MESSAGE_TYPE_SIGNAL))
    {
      XD_DBUS_VALIDATE_PATH (path);
      XD_DBUS_VALIDATE_INTERFACE (interface);
      XD_DBUS_VALIDATE_MEMBER (member);
      if (!NILP (handler) && !FUNCTIONP (handler))
	wrong_type_argument (Qinvalid_function, handler);
    }

  /* Trace parameters.  */
  switch (mtype)
    {
    case DBUS_MESSAGE_TYPE_METHOD_CALL:
      XD_DEBUG_MESSAGE ("%s %s %s %s %s %s %s",
			XD_MESSAGE_TYPE_TO_STRING (mtype),
			XD_OBJECT_TO_STRING (bus),
			XD_OBJECT_TO_STRING (service),
			XD_OBJECT_TO_STRING (path),
			XD_OBJECT_TO_STRING (interface),
			XD_OBJECT_TO_STRING (member),
			XD_OBJECT_TO_STRING (handler));
      break;
    case DBUS_MESSAGE_TYPE_SIGNAL:
      XD_DEBUG_MESSAGE ("%s %s %s %s %s %s",
			XD_MESSAGE_TYPE_TO_STRING (mtype),
			XD_OBJECT_TO_STRING (bus),
			XD_OBJECT_TO_STRING (service),
			XD_OBJECT_TO_STRING (path),
			XD_OBJECT_TO_STRING (interface),
			XD_OBJECT_TO_STRING (member));
      break;
    default: /* DBUS_MESSAGE_TYPE_METHOD_RETURN, DBUS_MESSAGE_TYPE_ERROR  */
      ui_serial = serial;
      XD_DEBUG_MESSAGE ("%s %s %s %u",
			XD_MESSAGE_TYPE_TO_STRING (mtype),
			XD_OBJECT_TO_STRING (bus),
			XD_OBJECT_TO_STRING (service),
			ui_serial);
    }

  /* Retrieve bus address.  */
  connection = xd_get_connection_address (bus);

  /* Create the D-Bus message.  */
  dmessage = dbus_message_new (mtype);
  if (dmessage == NULL)
    XD_SIGNAL1 (build_string ("Unable to create a new message"));

  if (STRINGP (service))
    {
      if (mtype != DBUS_MESSAGE_TYPE_SIGNAL)
	/* Set destination.  */
	{
	  if (!dbus_message_set_destination (dmessage, SSDATA (service)))
	    XD_SIGNAL2 (build_string ("Unable to set the destination"),
			service);
	}

      else
	/* Set destination for unicast signals.  */
	{
	  Lisp_Object uname;

	  /* If it is the same unique name as we are registered at the
	     bus or an unknown name, we regard it as broadcast message
	     due to backward compatibility.  */
	  if (dbus_bus_name_has_owner (connection, SSDATA (service), NULL))
	    uname = call2 (intern ("dbus-get-name-owner"), bus, service);
	  else
	    uname = Qnil;

	  if (STRINGP (uname)
	      && (strcmp (dbus_bus_get_unique_name (connection), SSDATA (uname))
		  != 0)
	      && (!dbus_message_set_destination (dmessage, SSDATA (service))))
	    XD_SIGNAL2 (build_string ("Unable to set signal destination"),
			service);
	}
    }

  /* Set message parameters.  */
  if ((mtype == DBUS_MESSAGE_TYPE_METHOD_CALL)
      || (mtype == DBUS_MESSAGE_TYPE_SIGNAL))
    {
      if ((!dbus_message_set_path (dmessage, SSDATA (path)))
	  || (!dbus_message_set_interface (dmessage, SSDATA (interface)))
	  || (!dbus_message_set_member (dmessage, SSDATA (member))))
	XD_SIGNAL1 (build_string ("Unable to set the message parameter"));
    }

  else /* DBUS_MESSAGE_TYPE_METHOD_RETURN, DBUS_MESSAGE_TYPE_ERROR  */
    {
      if (!dbus_message_set_reply_serial (dmessage, serial))
	XD_SIGNAL1 (build_string ("Unable to create a return message"));

      if ((mtype == DBUS_MESSAGE_TYPE_ERROR)
	  && (!dbus_message_set_error_name (dmessage, DBUS_ERROR_FAILED)))
	XD_SIGNAL1 (build_string ("Unable to create a error message"));
    }

  /* Check for timeout parameter.  */
  if ((count + 2 <= nargs) && EQ (args[count], QCtimeout))
    {
      CHECK_NATNUM (args[count+1]);
      timeout = min (XFASTINT (args[count+1]), INT_MAX);
      count = count+2;
    }

  /* Initialize parameter list of message.  */
  dbus_message_iter_init_append (dmessage, &iter);

  /* Append parameters to the message.  */
  for (; count < nargs; ++count)
    {
      dtype = XD_OBJECT_TO_DBUS_TYPE (args[count]);
      if (XD_DBUS_TYPE_P (args[count]))
	{
	  XD_DEBUG_VALID_LISP_OBJECT_P (args[count]);
	  XD_DEBUG_VALID_LISP_OBJECT_P (args[count+1]);
	  XD_DEBUG_MESSAGE ("Parameter%"pD"d %s %s", count - 4,
			    XD_OBJECT_TO_STRING (args[count]),
			    XD_OBJECT_TO_STRING (args[count+1]));
	  ++count;
	}
      else
	{
	  XD_DEBUG_VALID_LISP_OBJECT_P (args[count]);
	  XD_DEBUG_MESSAGE ("Parameter%"pD"d %s", count - 4,
			    XD_OBJECT_TO_STRING (args[count]));
	}

      /* Check for valid signature.  We use DBUS_TYPE_INVALID as
	 indication that there is no parent type.  */
      xd_signature (signature, dtype, DBUS_TYPE_INVALID, args[count]);

      xd_append_arg (dtype, args[count], &iter);
    }

  if (!NILP (handler))
    {
      /* Send the message.  The message is just added to the outgoing
	 message queue.  */
      if (!dbus_connection_send_with_reply (connection, dmessage,
					    NULL, timeout))
	XD_SIGNAL1 (build_string ("Cannot send message"));

      /* The result is the key in Vdbus_registered_objects_table.  */
      serial = dbus_message_get_serial (dmessage);
      result = list3 (QCserial, bus, make_fixnum_or_float (serial));

      /* Create a hash table entry.  */
      Fputhash (result, handler, Vdbus_registered_objects_table);
    }
  else
    {
      /* Send the message.  The message is just added to the outgoing
	 message queue.  */
      if (!dbus_connection_send (connection, dmessage, NULL))
	XD_SIGNAL1 (build_string ("Cannot send message"));

      result = Qnil;
    }

  XD_DEBUG_MESSAGE ("Message sent: %s", XD_OBJECT_TO_STRING (result));

  /* Cleanup.  */
  dbus_message_unref (dmessage);

  /* Return the result.  */
  return result;
}

/* Read one queued incoming message of the D-Bus BUS.
   BUS is either a Lisp symbol, :system or :session, or a string denoting
   the bus address.  */
static void
xd_read_message_1 (DBusConnection *connection, Lisp_Object bus)
{
  Lisp_Object args, key, value;
  struct input_event event;
  DBusMessage *dmessage;
  DBusMessageIter iter;
  int dtype;
  int mtype;
  dbus_uint32_t serial;
  unsigned int ui_serial;
  const char *uname, *path, *interface, *member;

  dmessage = dbus_connection_pop_message (connection);

  /* Return if there is no queued message.  */
  if (dmessage == NULL)
    return;

  /* Collect the parameters.  */
  args = Qnil;

  /* Loop over the resulting parameters.  Construct a list.  */
  if (dbus_message_iter_init (dmessage, &iter))
    {
      while ((dtype = dbus_message_iter_get_arg_type (&iter))
	     != DBUS_TYPE_INVALID)
	{
	  args = Fcons (xd_retrieve_arg (dtype, &iter), args);
	  dbus_message_iter_next (&iter);
	}
      /* The arguments are stored in reverse order.  Reorder them.  */
      args = Fnreverse (args);
    }

  /* Read message type, message serial, unique name, object path,
     interface and member from the message.  */
  mtype = dbus_message_get_type (dmessage);
  ui_serial = serial =
    ((mtype == DBUS_MESSAGE_TYPE_METHOD_RETURN)
     || (mtype == DBUS_MESSAGE_TYPE_ERROR))
    ? dbus_message_get_reply_serial (dmessage)
    : dbus_message_get_serial (dmessage);
  uname = dbus_message_get_sender (dmessage);
  path = dbus_message_get_path (dmessage);
  interface = dbus_message_get_interface (dmessage);
  member = dbus_message_get_member (dmessage);

  XD_DEBUG_MESSAGE ("Event received: %s %u %s %s %s %s %s",
		    XD_MESSAGE_TYPE_TO_STRING (mtype),
		    ui_serial, uname, path, interface, member,
		    XD_OBJECT_TO_STRING (args));

  if (mtype == DBUS_MESSAGE_TYPE_INVALID)
    goto cleanup;

  else if ((mtype == DBUS_MESSAGE_TYPE_METHOD_RETURN)
	   || (mtype == DBUS_MESSAGE_TYPE_ERROR))
    {
      /* Search for a registered function of the message.  */
      key = list3 (QCserial, bus, make_fixnum_or_float (serial));
      value = Fgethash (key, Vdbus_registered_objects_table, Qnil);

      /* There shall be exactly one entry.  Construct an event.  */
      if (NILP (value))
	goto cleanup;

      /* Remove the entry.  */
      Fremhash (key, Vdbus_registered_objects_table);

      /* Construct an event.  */
      EVENT_INIT (event);
      event.kind = DBUS_EVENT;
      event.frame_or_window = Qnil;
      event.arg = Fcons (value, args);
    }

  else /* DBUS_MESSAGE_TYPE_METHOD_CALL, DBUS_MESSAGE_TYPE_SIGNAL.  */
    {
      /* Vdbus_registered_objects_table requires non-nil interface and
	 member.  */
      if ((interface == NULL) || (member == NULL))
	goto cleanup;

      /* Search for a registered function of the message.  */
      key = list4 (mtype == DBUS_MESSAGE_TYPE_METHOD_CALL ? QCmethod : QCsignal,
		   bus, build_string (interface), build_string (member));
      value = Fgethash (key, Vdbus_registered_objects_table, Qnil);

      /* Loop over the registered functions.  Construct an event.  */
      while (!NILP (value))
	{
	  key = CAR_SAFE (value);
	  /* key has the structure (UNAME SERVICE PATH HANDLER).  */
	  if (((uname == NULL)
	       || (NILP (CAR_SAFE (key)))
	       || (strcmp (uname, SSDATA (CAR_SAFE (key))) == 0))
	      && ((path == NULL)
		  || (NILP (CAR_SAFE (CDR_SAFE (CDR_SAFE (key)))))
		  || (strcmp (path,
			      SSDATA (CAR_SAFE (CDR_SAFE (CDR_SAFE (key)))))
		      == 0))
	      && (!NILP (CAR_SAFE (CDR_SAFE (CDR_SAFE (CDR_SAFE (key)))))))
	    {
	      EVENT_INIT (event);
	      event.kind = DBUS_EVENT;
	      event.frame_or_window = Qnil;
	      event.arg
		= Fcons (CAR_SAFE (CDR_SAFE (CDR_SAFE (CDR_SAFE (key)))), args);
	      break;
	    }
	  value = CDR_SAFE (value);
	}

      if (NILP (value))
	goto cleanup;
    }

  /* Add type, serial, uname, path, interface and member to the event.  */
  event.arg = Fcons ((member == NULL ? Qnil : build_string (member)),
		     event.arg);
  event.arg = Fcons ((interface == NULL ? Qnil : build_string (interface)),
		     event.arg);
  event.arg = Fcons ((path == NULL ? Qnil : build_string (path)),
		     event.arg);
  event.arg = Fcons ((uname == NULL ? Qnil : build_string (uname)),
		     event.arg);
  event.arg = Fcons (make_fixnum_or_float (serial), event.arg);
  event.arg = Fcons (make_number (mtype), event.arg);

  /* Add the bus symbol to the event.  */
  event.arg = Fcons (bus, event.arg);

  /* Store it into the input event queue.  */
  kbd_buffer_store_event (&event);

  XD_DEBUG_MESSAGE ("Event stored: %s", XD_OBJECT_TO_STRING (event.arg));

  /* Cleanup.  */
 cleanup:
  dbus_message_unref (dmessage);
}

/* Read queued incoming messages of the D-Bus BUS.
   BUS is either a Lisp symbol, :system or :session, or a string denoting
   the bus address.  */
static Lisp_Object
xd_read_message (Lisp_Object bus)
{
  /* Retrieve bus address.  */
  DBusConnection *connection = xd_get_connection_address (bus);

  /* Non blocking read of the next available message.  */
  dbus_connection_read_write (connection, 0);

  while (dbus_connection_get_dispatch_status (connection)
         != DBUS_DISPATCH_COMPLETE)
    xd_read_message_1 (connection, bus);
  return Qnil;
}

/* Callback called when something is ready to read or write.  */
static void
xd_read_queued_messages (int fd, void *data)
{
  Lisp_Object busp = xd_registered_buses;
  Lisp_Object bus = Qnil;
  Lisp_Object key;

  /* Find bus related to fd.  */
  if (data != NULL)
    while (!NILP (busp))
      {
	key = CAR_SAFE (CAR_SAFE (busp));
	if ((SYMBOLP (key) && XSYMBOL (key) == data)
	    || (STRINGP (key) && XSTRING (key) == data))
	  bus = key;
	busp = CDR_SAFE (busp);
      }

  if (NILP (bus))
    return;

  /* We ignore all Lisp errors during the call.  */
  xd_in_read_queued_messages = 1;
  internal_catch (Qdbus_error, xd_read_message, bus);
  xd_in_read_queued_messages = 0;
}


void
init_dbusbind (void)
{
  /* We do not want to abort.  */
  xputenv ("DBUS_FATAL_WARNINGS=0");
}

void
syms_of_dbusbind (void)
{
  defsubr (&Sdbus__init_bus);
  defsubr (&Sdbus_get_unique_name);

  DEFSYM (Qdbus_message_internal, "dbus-message-internal");
  defsubr (&Sdbus_message_internal);

  /* D-Bus error symbol.  */
  DEFSYM (Qdbus_error, "dbus-error");
  Fput (Qdbus_error, Qerror_conditions,
	list2 (Qdbus_error, Qerror));
  Fput (Qdbus_error, Qerror_message,
	build_pure_c_string ("D-Bus error"));

  /* Lisp symbols of the system and session buses.  */
  DEFSYM (QCsystem, ":system");
  DEFSYM (QCsession, ":session");

  /* Lisp symbol for method call timeout.  */
  DEFSYM (QCtimeout, ":timeout");

  /* Lisp symbols of D-Bus types.  */
  DEFSYM (QCbyte, ":byte");
  DEFSYM (QCboolean, ":boolean");
  DEFSYM (QCint16, ":int16");
  DEFSYM (QCuint16, ":uint16");
  DEFSYM (QCint32, ":int32");
  DEFSYM (QCuint32, ":uint32");
  DEFSYM (QCint64, ":int64");
  DEFSYM (QCuint64, ":uint64");
  DEFSYM (QCdouble, ":double");
  DEFSYM (QCstring, ":string");
  DEFSYM (QCobject_path, ":object-path");
  DEFSYM (QCsignature, ":signature");
#ifdef DBUS_TYPE_UNIX_FD
  DEFSYM (QCunix_fd, ":unix-fd");
#endif
  DEFSYM (QCarray, ":array");
  DEFSYM (QCvariant, ":variant");
  DEFSYM (QCstruct, ":struct");
  DEFSYM (QCdict_entry, ":dict-entry");

  /* Lisp symbols of objects in `dbus-registered-objects-table'.  */
  DEFSYM (QCserial, ":serial");
  DEFSYM (QCmethod, ":method");
  DEFSYM (QCsignal, ":signal");

  DEFVAR_LISP ("dbus-compiled-version",
	       Vdbus_compiled_version,
    doc: /* The version of D-Bus Emacs is compiled against.  */);
#ifdef DBUS_VERSION_STRING
  Vdbus_compiled_version = build_pure_c_string (DBUS_VERSION_STRING);
#else
  Vdbus_compiled_version = Qnil;
#endif

  DEFVAR_LISP ("dbus-runtime-version",
	       Vdbus_runtime_version,
    doc: /* The version of D-Bus Emacs runs with.  */);
  {
#ifdef DBUS_VERSION
    int major, minor, micro;
    char s[sizeof ".." + 3 * INT_STRLEN_BOUND (int)];
    dbus_get_version (&major, &minor, &micro);
    Vdbus_runtime_version
      = make_formatted_string (s, "%d.%d.%d", major, minor, micro);
#else
    Vdbus_runtime_version = Qnil;
#endif
  }

  DEFVAR_LISP ("dbus-message-type-invalid",
	       Vdbus_message_type_invalid,
    doc: /* This value is never a valid message type.  */);
  Vdbus_message_type_invalid = make_number (DBUS_MESSAGE_TYPE_INVALID);

  DEFVAR_LISP ("dbus-message-type-method-call",
	       Vdbus_message_type_method_call,
    doc: /* Message type of a method call message.  */);
  Vdbus_message_type_method_call = make_number (DBUS_MESSAGE_TYPE_METHOD_CALL);

  DEFVAR_LISP ("dbus-message-type-method-return",
	       Vdbus_message_type_method_return,
    doc: /* Message type of a method return message.  */);
  Vdbus_message_type_method_return
    = make_number (DBUS_MESSAGE_TYPE_METHOD_RETURN);

  DEFVAR_LISP ("dbus-message-type-error",
	       Vdbus_message_type_error,
    doc: /* Message type of an error reply message.  */);
  Vdbus_message_type_error = make_number (DBUS_MESSAGE_TYPE_ERROR);

  DEFVAR_LISP ("dbus-message-type-signal",
	       Vdbus_message_type_signal,
    doc: /* Message type of a signal message.  */);
  Vdbus_message_type_signal = make_number (DBUS_MESSAGE_TYPE_SIGNAL);

  DEFVAR_LISP ("dbus-registered-objects-table",
	       Vdbus_registered_objects_table,
    doc: /* Hash table of registered functions for D-Bus.

There are two different uses of the hash table: for accessing
registered interfaces properties, targeted by signals or method calls,
and for calling handlers in case of non-blocking method call returns.

In the first case, the key in the hash table is the list (TYPE BUS
INTERFACE MEMBER).  TYPE is one of the Lisp symbols `:method',
`:signal' or `:property'.  BUS is either a Lisp symbol, `:system' or
`:session', or a string denoting the bus address.  INTERFACE is a
string which denotes a D-Bus interface, and MEMBER, also a string, is
either a method, a signal or a property INTERFACE is offering.  All
arguments but BUS must not be nil.

The value in the hash table is a list of quadruple lists ((UNAME
SERVICE PATH OBJECT [RULE]) ...).  SERVICE is the service name as
registered, UNAME is the corresponding unique name.  In case of
registered methods and properties, UNAME is nil.  PATH is the object
path of the sending object.  All of them can be nil, which means a
wildcard then.  OBJECT is either the handler to be called when a D-Bus
message, which matches the key criteria, arrives (TYPE `:method' and
`:signal'), or a cons cell containing the value of the property (TYPE
`:property').

For entries of type `:signal', there is also a fifth element RULE,
which keeps the match string the signal is registered with.

In the second case, the key in the hash table is the list (:serial BUS
SERIAL).  BUS is either a Lisp symbol, `:system' or `:session', or a
string denoting the bus address.  SERIAL is the serial number of the
non-blocking method call, a reply is expected.  Both arguments must
not be nil.  The value in the hash table is HANDLER, the function to
be called when the D-Bus reply message arrives.  */);
  Vdbus_registered_objects_table = CALLN (Fmake_hash_table, QCtest, Qequal);

  DEFVAR_LISP ("dbus-debug", Vdbus_debug,
    doc: /* If non-nil, debug messages of D-Bus bindings are raised.  */);
#ifdef DBUS_DEBUG
  Vdbus_debug = Qt;
  /* We can also set environment variable DBUS_VERBOSE=1 in order to
     see more traces.  This requires libdbus-1 to be configured with
     --enable-verbose-mode.  */
#else
  Vdbus_debug = Qnil;
#endif

  /* Initialize internal objects.  */
  xd_registered_buses = Qnil;
  staticpro (&xd_registered_buses);

  Fprovide (intern_c_string ("dbusbind"), Qnil);

}

#endif /* HAVE_DBUS */
