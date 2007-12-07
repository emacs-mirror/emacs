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

/* Hash table which keeps function definitions.  */
Lisp_Object Vdbus_registered_functions_table;

/* Whether to debug D-Bus.  */
Lisp_Object Vdbus_debug;


/* We use "xd_" and "XD_" as prefix for all internal symbols, because
   we don't want to poison other namespaces with "dbus_".  */

/* Raise a Lisp error from a D-Bus error.  */
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

/* Determine the DBusType of a given Lisp object.  It is used to
   convert Lisp objects, being arguments of `dbus-call-method' or
   `dbus-send-signal', into corresponding C values appended as
   arguments to a D-Bus message.  */
#define XD_LISP_OBJECT_TO_DBUS_TYPE(object)				\
  (EQ (object, Qt) || EQ (object, Qnil)) ? DBUS_TYPE_BOOLEAN :		\
  (NATNUMP (object)) ? DBUS_TYPE_UINT32 :				\
  (INTEGERP (object)) ? DBUS_TYPE_INT32 :				\
  (FLOATP (object)) ? DBUS_TYPE_DOUBLE :				\
  (STRINGP (object)) ? DBUS_TYPE_STRING :				\
  DBUS_TYPE_INVALID

/* Extract C value from Lisp OBJECT.  DTYPE must be a valid DBusType,
   as detected by XD_LISP_OBJECT_TO_DBUS_TYPE.  Compound types are not
   supported (yet).  It is used to convert Lisp objects, being
   arguments of `dbus-call-method' or `dbus-send-signal', into
   corresponding C values appended as arguments to a D-Bus
   message.  */
char *
xd_retrieve_value (dtype, object)
     uint dtype;
     Lisp_Object object;
{

  XD_DEBUG_VALID_LISP_OBJECT_P (object);
  switch (dtype)
    {
    case DBUS_TYPE_BOOLEAN:
      XD_DEBUG_MESSAGE ("%d %s", dtype, (NILP (object)) ? "false" : "true");
      return (NILP (object)) ? (char *) FALSE : (char *) TRUE;
    case DBUS_TYPE_UINT32:
      XD_DEBUG_MESSAGE ("%d %d", dtype, XUINT (object));
      return (char *) XUINT (object);
    case DBUS_TYPE_INT32:
      XD_DEBUG_MESSAGE ("%d %d", dtype, XINT (object));
      return (char *) XINT (object);
    case DBUS_TYPE_DOUBLE:
      XD_DEBUG_MESSAGE ("%d %d", dtype, XFLOAT (object));
      return (char *) XFLOAT (object);
    case DBUS_TYPE_STRING:
      XD_DEBUG_MESSAGE ("%d %s", dtype, SDATA (object));
      return SDATA (object);
    default:
      XD_DEBUG_MESSAGE ("DBus-Type %d not supported", dtype);
      return NULL;
    }
}

/* Retrieve C value from a DBusMessageIter structure ITER, and return
   a converted Lisp object.  The type DTYPE of the argument of the
   D-Bus message must be a valid DBusType.  Compound D-Bus types are
   partly supported; they result always in a Lisp list.  */
Lisp_Object
xd_retrieve_arg (dtype, iter)
     uint dtype;
     DBusMessageIter *iter;
{

  switch (dtype)
    {
    case DBUS_TYPE_BOOLEAN:
      {
	dbus_bool_t val;
	dbus_message_iter_get_basic (iter, &val);
	XD_DEBUG_MESSAGE ("%d %s", dtype, (val == FALSE) ? "false" : "true");
	return (val == FALSE) ? Qnil : Qt;
      }
    case DBUS_TYPE_INT32:
    case DBUS_TYPE_UINT32:
      {
	dbus_uint32_t val;
	dbus_message_iter_get_basic (iter, &val);
	XD_DEBUG_MESSAGE ("%d %d", dtype, val);
	return make_number (val);
      }
    case DBUS_TYPE_STRING:
    case DBUS_TYPE_OBJECT_PATH:
      {
	char *val;
	dbus_message_iter_get_basic (iter, &val);
	XD_DEBUG_MESSAGE ("%d %s", dtype, val);
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
      XD_DEBUG_MESSAGE ("DBusType %d not supported", dtype);
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

Other Lisp objects are not supported as input arguments of METHOD.

`dbus-call-method' returns the resulting values of METHOD as a list of
Lisp objects.  The type conversion happens the other direction as for
input arguments.  Additionally to the types supported for input
arguments, the D-Bus compound types DBUS_TYPE_ARRAY, DBUS_TYPE_VARIANT,
DBUS_TYPE_STRUCT and DBUS_TYPE_DICT_ENTRY are accepted.  All of them
are converted into a list of Lisp objects which correspond to the
elements of the D-Bus container.  Example:

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
  uint dtype;
  int i;
  char *value;

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
  dmessage = dbus_message_new_method_call (SDATA (service),
					   SDATA (path),
					   SDATA (interface),
					   SDATA (method));
  if (dmessage == NULL)
    {
      UNGCPRO;
      xsignal1 (Qdbus_error, build_string ("Unable to create a new message"));
    }

  UNGCPRO;

  /* Append parameters to the message.  */
  for (i = 5; i < nargs; ++i)
    {

      XD_DEBUG_VALID_LISP_OBJECT_P (args[i]);
      XD_DEBUG_MESSAGE ("Parameter%d %s",
			i-4,
			SDATA (format2 ("%s", args[i], Qnil)));

      dtype = XD_LISP_OBJECT_TO_DBUS_TYPE (args[i]);
      if (dtype == DBUS_TYPE_INVALID)
	xsignal2 (Qdbus_error, build_string ("Not a valid argument"), args[i]);

      value = (char *) xd_retrieve_value (dtype, args[i]);

      if (!dbus_message_append_args (dmessage,
				     dtype,
				     &value,
				     DBUS_TYPE_INVALID))
	xsignal2 (Qdbus_error,
		  build_string ("Unable to append argument"), args[i]);
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

Other Lisp objects are not supported as arguments of SIGNAL.

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
  uint dtype;
  int i;
  char *value;

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
  dmessage = dbus_message_new_signal (SDATA (path),
				      SDATA (interface),
				      SDATA (signal));
  if (dmessage == NULL)
    {
      UNGCPRO;
      xsignal1 (Qdbus_error, build_string ("Unable to create a new message"));
    }

  UNGCPRO;

  /* Append parameters to the message.  */
  for (i = 5; i < nargs; ++i)
    {
      XD_DEBUG_VALID_LISP_OBJECT_P (args[i]);
      XD_DEBUG_MESSAGE ("Parameter%d %s",
			i-4,
			SDATA (format2 ("%s", args[i], Qnil)));

      dtype = XD_LISP_OBJECT_TO_DBUS_TYPE (args[i]);
      if (dtype == DBUS_TYPE_INVALID)
	xsignal2 (Qdbus_error, build_string ("Not a valid argument"), args[i]);

      value = (char *) xd_retrieve_value (dtype, args[i]);

      if (!dbus_message_append_args (dmessage,
				     dtype,
				     &value,
				     DBUS_TYPE_INVALID))
	xsignal2 (Qdbus_error,
		  build_string ("Unable to append argument"), args[i]);
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
  Lisp_Object key;
  struct gcpro gcpro1;
  static struct input_event event;
  DBusConnection *connection;
  DBusMessage *dmessage;
  DBusMessageIter iter;
  uint dtype;
  char service[DBUS_MAXIMUM_NAME_LENGTH];
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

  /* There is a message in the queue.  Construct the D-Bus event.  */
  XD_DEBUG_MESSAGE ("Event received");
  EVENT_INIT (event);

  event.kind = DBUS_EVENT;
  event.frame_or_window = Qnil;

  /* Collect the parameters.  */
  event.arg = Qnil;
  GCPRO1 (event.arg);

  if (!dbus_message_iter_init (dmessage, &iter))
    {
      UNGCPRO;
      XD_DEBUG_MESSAGE ("Cannot read event");
      return;
    }

  /* Loop over the resulting parameters.  Construct a list.  */
  while ((dtype = dbus_message_iter_get_arg_type (&iter)) != DBUS_TYPE_INVALID)
    {
      event.arg = Fcons (xd_retrieve_arg (dtype, &iter), event.arg);
      dbus_message_iter_next (&iter);
    }

  /* The arguments are stored in reverse order.  Reorder them.  */
  event.arg = Fnreverse (event.arg);

  /* Read service, object path, interface and member from the message.
     The service is always the unique name of the sending object.  */
  strcpy (service,   dbus_message_get_sender (dmessage));
  strcpy (path,      dbus_message_get_path (dmessage));
  strcpy (interface, dbus_message_get_interface (dmessage));
  strcpy (member,    dbus_message_get_member (dmessage));

  /* Add the registered function of the message.  */
  key = list5 (bus,
	       (service == NULL ? Qnil : build_string (service)),
	       (path == NULL ? Qnil : build_string (path)),
	       (interface == NULL ? Qnil : build_string (interface)),
	       (member == NULL ? Qnil : build_string (member)));
  event.arg = Fcons (Fgethash (key, Vdbus_registered_functions_table, Qnil),
		     event.arg);

  /* Add service, path, interface and member to the event.  */
  event.arg = Fcons ((member == NULL ? Qnil : build_string (member)),
		     event.arg);
  event.arg = Fcons ((interface == NULL ? Qnil : build_string (interface)),
		     event.arg);
  event.arg = Fcons ((path == NULL ? Qnil : build_string (path)),
		     event.arg);
  event.arg = Fcons ((service == NULL ? Qnil : build_string (service)),
		     event.arg);

  /* Add the bus symbol to the event.  */
  event.arg = Fcons (bus, event.arg);

  /* Store it into the input event queue.  */
  kbd_buffer_store_event (&event);

  /* Cleanup.  */
  dbus_message_unref (dmessage);
  UNGCPRO;
}

/* Read queued incoming messages from the system and session buses.  */
void
xd_read_queued_messages ()
{

  /* Vdbus_registered_functions_table will be made as hash table in
     dbus.el.  When it isn't loaded yet, it doesn't make sense to
     handle D-Bus messages.  Furthermore, we ignore all Lisp errors
     during the call.  */
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

  => (:system ":1.3" "/org/freedesktop/Hal/Manager"
      "org.freedesktop.Hal.Manager" "DeviceAdded")

`dbus-register-signal' returns an object, which can be used in
`dbus-unregister-signal' for removing the registration.  */)
     (bus, service, path, interface, signal, handler)
     Lisp_Object bus, service, path, interface, signal, handler;
{
  Lisp_Object unique_name, key;
  DBusConnection *connection;
  char rule[DBUS_MAXIMUM_MATCH_RULE_LENGTH];
  DBusError derror;

  /* Check parameters.  */
  CHECK_SYMBOL (bus);
  if (!NILP (service)) CHECK_STRING (service);
  if (!NILP (path)) CHECK_STRING (path);
  CHECK_STRING (interface);
  CHECK_STRING (signal);
  CHECK_SYMBOL (handler);

  /* Retrieve unique name of service.  If service is a known name, we
     will register for the corresponding unique name, if any.  Signals
     are sent always with the unique name as sender.  Note: the unique
     name of "org.freedesktop.DBus" is that string itself.  */
  if ((!NILP (service)) &&
      (strcmp (SDATA (service), DBUS_SERVICE_DBUS) != 0) &&
      (strncmp (SDATA (service), ":", 1) != 0))
    unique_name = call2 (intern ("dbus-get-name-owner"), bus, service);
  else
    unique_name = service;

  /* Open a connection to the bus.  */
  connection = xd_initialize (bus);

  /* Create a rule to receive related signals.  */
  sprintf (rule,
	   "type='signal',interface='%s',member=%s%",
	   SDATA (interface),
	   SDATA (signal));

  /* Add unique name and path to the rule if they are non-nil.  */
  if (!NILP (unique_name))
    sprintf (rule, "%s,sender='%s'%", rule, SDATA (unique_name));

  if (!NILP (path))
    sprintf (rule, "%s,path='%s'", rule, SDATA (path));

  /* Add the rule to the bus.  */
  dbus_error_init (&derror);
  dbus_bus_add_match (connection, rule, &derror);
  if (dbus_error_is_set (&derror))
    XD_ERROR (derror);

  XD_DEBUG_MESSAGE ("Matching rule \"%s\" created", rule);

  /* Create a hash table entry.  */
  key = list5 (bus, unique_name, path, interface, signal);
  Fputhash (key, handler, Vdbus_registered_functions_table);
  XD_DEBUG_MESSAGE ("\"%s\" registered with handler \"%s\"",
		    SDATA (format2 ("%s", key, Qnil)),
		    SDATA (format2 ("%s", handler, Qnil)));

  /* Return key.  */
  return key;
}

DEFUN ("dbus-unregister-signal", Fdbus_unregister_signal, Sdbus_unregister_signal,
       1, 1, 0,
       doc: /* Unregister OBJECT from the D-Bus.
OBJECT must be the result of a preceding `dbus-register-signal' call.  */)
     (object)
     Lisp_Object object;
{

  /* Unintern the signal symbol.  */
  Fremhash (object, Vdbus_registered_functions_table);

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

  DEFVAR_LISP ("dbus-registered-functions-table", &Vdbus_registered_functions_table,
    doc: /* Hash table of registered functions for D-Bus.
The key in the hash table is the list (BUS SERVICE PATH MEMBER INTERFACE).
BUS is either the symbol `:system' or the symbol `:session'.  SERVICE
and PATH are the unique name and the object path of the sending object.
INTERFACE is a string which denotes a D-Bus interface, and MEMBER,
also a string, is either a method or a signal INTERFACE is offering.
All arguments but BUS can be nil, which means a wild card then.

The value in the hash table a the function to be called when a D-Bus
message, which matches the key criteria, arrives.  */);
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
