/* Selection processing for Emacs on Mac OS.
   Copyright (C) 2005 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include <config.h>

#include "lisp.h"
#include "macterm.h"
#include "blockinput.h"
#include "keymap.h"

#if !TARGET_API_MAC_CARBON
#include <Endian.h>
typedef int ScrapRef;
typedef ResType ScrapFlavorType;
#endif /* !TARGET_API_MAC_CARBON */

static OSErr get_scrap_from_symbol P_ ((Lisp_Object, int, ScrapRef *));
static ScrapFlavorType get_flavor_type_from_symbol P_ ((Lisp_Object));
static int valid_scrap_target_type_p P_ ((Lisp_Object));
static OSErr clear_scrap P_ ((ScrapRef *));
static OSErr put_scrap_string P_ ((ScrapRef, Lisp_Object, Lisp_Object));
static OSErr put_scrap_private_timestamp P_ ((ScrapRef, unsigned long));
static ScrapFlavorType scrap_has_target_type P_ ((ScrapRef, Lisp_Object));
static Lisp_Object get_scrap_string P_ ((ScrapRef, Lisp_Object));
static OSErr get_scrap_private_timestamp P_ ((ScrapRef, unsigned long *));
static Lisp_Object get_scrap_target_type_list P_ ((ScrapRef));
static void x_own_selection P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object x_get_local_selection P_ ((Lisp_Object, Lisp_Object, int));
static Lisp_Object x_get_foreign_selection P_ ((Lisp_Object,
                                                Lisp_Object,
                                                Lisp_Object));
EXFUN (Fx_selection_owner_p, 1);
#ifdef MAC_OSX
static OSStatus mac_handle_service_event P_ ((EventHandlerCallRef,
					      EventRef, void *));
void init_service_handler P_ ((void));
#endif

Lisp_Object QPRIMARY, QSECONDARY, QTIMESTAMP, QTARGETS;

static Lisp_Object Vx_lost_selection_functions;
/* Coding system for communicating with other programs via scrap.  */
static Lisp_Object Vselection_coding_system;

/* Coding system for the next communicating with other programs.  */
static Lisp_Object Vnext_selection_coding_system;

static Lisp_Object Qforeign_selection;

/* The timestamp of the last input event Emacs received from the
   window server.  */
/* Defined in keyboard.c.  */
extern unsigned long last_event_timestamp;

/* This is an association list whose elements are of the form
     ( SELECTION-NAME SELECTION-VALUE SELECTION-TIMESTAMP FRAME)
   SELECTION-NAME is a lisp symbol.
   SELECTION-VALUE is the value that emacs owns for that selection.
     It may be any kind of Lisp object.
   SELECTION-TIMESTAMP is the time at which emacs began owning this selection,
     as a cons of two 16-bit numbers (making a 32 bit time.)
   FRAME is the frame for which we made the selection.
   If there is an entry in this alist, and the data for the flavor
     type SCRAP_FLAVOR_TYPE_EMACS_TIMESTAMP in the corresponding scrap
     (if exists) coincides with SELECTION-TIMESTAMP, then it can be
     assumed that Emacs owns that selection.
   The only (eq) parts of this list that are visible from Lisp are the
    selection-values.  */
static Lisp_Object Vselection_alist;

#define SCRAP_FLAVOR_TYPE_EMACS_TIMESTAMP 'Etsp'

/* This is an alist whose CARs are selection-types and whose CDRs are
   the names of Lisp functions to call to convert the given Emacs
   selection value to a string representing the given selection type.
   This is for Lisp-level extension of the emacs selection
   handling.  */
static Lisp_Object Vselection_converter_alist;

/* A selection name (represented as a Lisp symbol) can be associated
   with a named scrap via `mac-scrap-name' property.  Likewise for a
   selection type with a scrap flavor type via `mac-ostype'.  */
static Lisp_Object Qmac_scrap_name, Qmac_ostype;

#ifdef MAC_OSX
/* Selection name for communication via Services menu.  */
static Lisp_Object Vmac_services_selection;
#endif

/* Get a reference to the scrap corresponding to the symbol SYM.  The
   reference is set to *SCRAP, and it becomes NULL if there's no
   corresponding scrap.  Clear the scrap if CLEAR_P is non-zero.  */

static OSErr
get_scrap_from_symbol (sym, clear_p, scrap)
     Lisp_Object sym;
     int clear_p;
     ScrapRef *scrap;
{
  OSErr err = noErr;
  Lisp_Object str = Fget (sym, Qmac_scrap_name);

  if (!STRINGP (str))
    *scrap = NULL;
  else
    {
#if TARGET_API_MAC_CARBON
#ifdef MAC_OSX
      CFStringRef scrap_name = cfstring_create_with_string (str);
      OptionBits options = (clear_p ? kScrapClearNamedScrap
			    : kScrapGetNamedScrap);

      err = GetScrapByName (scrap_name, options, scrap);
      CFRelease (scrap_name);
#else	/* !MAC_OSX */
      if (clear_p)
	err = ClearCurrentScrap ();
      if (err == noErr)
	err = GetCurrentScrap (scrap);
#endif	/* !MAC_OSX */
#else	/* !TARGET_API_MAC_CARBON */
      if (clear_p)
	err = ZeroScrap ();
      if (err == noErr)
	*scrap = 1;
#endif	/* !TARGET_API_MAC_CARBON */
    }

  return err;
}

/* Get a scrap flavor type from the symbol SYM.  Return 0 if no
   corresponding flavor type.  */

static ScrapFlavorType
get_flavor_type_from_symbol (sym)
     Lisp_Object sym;
{
  ScrapFlavorType val;
  Lisp_Object str = Fget (sym, Qmac_ostype);

  if (STRINGP (str) && SBYTES (str) == 4)
    return EndianU32_BtoN (*((UInt32 *) SDATA (str)));

  return 0;
}

/* Check if the symbol SYM has a corresponding scrap flavor type.  */

static int
valid_scrap_target_type_p (sym)
     Lisp_Object sym;
{
  return get_flavor_type_from_symbol (sym) != 0;
}

/* Clear the scrap whose reference is *SCRAP. */

static INLINE OSErr
clear_scrap (scrap)
     ScrapRef *scrap;
{
#if TARGET_API_MAC_CARBON
#ifdef MAC_OSX
  return ClearScrap (scrap);
#else
  return ClearCurrentScrap ();
#endif
#else  /* !TARGET_API_MAC_CARBON */
  return ZeroScrap ();
#endif	/* !TARGET_API_MAC_CARBON */
}

/* Put Lisp String STR to the scrap SCRAP.  The target type is
   specified by TYPE. */

static OSErr
put_scrap_string (scrap, type, str)
     ScrapRef scrap;
     Lisp_Object type, str;
{
  ScrapFlavorType flavor_type = get_flavor_type_from_symbol (type);

  if (flavor_type == 0)
    return noTypeErr;

#if TARGET_API_MAC_CARBON
  return PutScrapFlavor (scrap, flavor_type, kScrapFlavorMaskNone,
			 SBYTES (str), SDATA (str));
#else  /* !TARGET_API_MAC_CARBON */
  return PutScrap (SBYTES (str), flavor_type, SDATA (str));
#endif	/* !TARGET_API_MAC_CARBON */
}

/* Put TIMESTAMP to the scrap SCRAP.  The timestamp is used for
   checking if the scrap is owned by the process.  */

static INLINE OSErr
put_scrap_private_timestamp (scrap, timestamp)
     ScrapRef scrap;
     unsigned long timestamp;
{
#if TARGET_API_MAC_CARBON
  return PutScrapFlavor (scrap, SCRAP_FLAVOR_TYPE_EMACS_TIMESTAMP,
			 kScrapFlavorMaskSenderOnly,
			 sizeof (timestamp), &timestamp);
#else  /* !TARGET_API_MAC_CARBON */
  return PutScrap (sizeof (timestamp), SCRAP_FLAVOR_TYPE_EMACS_TIMESTAMP,
		   &timestamp);
#endif	/* !TARGET_API_MAC_CARBON */
}

/* Check if data for the target type TYPE is available in SCRAP.  */

static ScrapFlavorType
scrap_has_target_type (scrap, type)
     ScrapRef scrap;
     Lisp_Object type;
{
  OSErr err;
  ScrapFlavorType flavor_type = get_flavor_type_from_symbol (type);

  if (flavor_type)
    {
#if TARGET_API_MAC_CARBON
      ScrapFlavorFlags flags;

      err = GetScrapFlavorFlags (scrap, flavor_type, &flags);
      if (err != noErr)
	flavor_type = 0;
#else  /* !TARGET_API_MAC_CARBON */
      SInt32 size, offset;

      size = GetScrap (NULL, flavor_type, &offset);
      if (size < 0)
	flavor_type = 0;
#endif	/* !TARGET_API_MAC_CARBON */
    }

  return flavor_type;
}

/* Get data for the target type TYPE from SCRAP and create a Lisp
   string.  Return nil if failed to get data.  */

static Lisp_Object
get_scrap_string (scrap, type)
     ScrapRef scrap;
     Lisp_Object type;
{
  OSErr err;
  Lisp_Object result = Qnil;
  ScrapFlavorType flavor_type = get_flavor_type_from_symbol (type);
#if TARGET_API_MAC_CARBON
  Size size;

  if (flavor_type)
    {
      err = GetScrapFlavorSize (scrap, flavor_type, &size);
      if (err == noErr)
	{
	  do
	    {
	      result = make_uninit_string (size);
	      err = GetScrapFlavorData (scrap, flavor_type,
					&size, SDATA (result));
	      if (err != noErr)
		result = Qnil;
	      else if (size < SBYTES (result))
		result = make_unibyte_string (SDATA (result), size);
	    }
	  while (STRINGP (result) && size > SBYTES (result));
	}
    }
#else
  Handle handle;
  SInt32 size, offset;

  if (flavor_type)
    size = GetScrap (NULL, flavor_type, &offset);
  if (size >= 0)
    {
      handle = NewHandle (size);
      HLock (handle);
      size = GetScrap (handle, flavor_type, &offset);
      if (size >= 0)
	result = make_unibyte_string (*handle, size);
      DisposeHandle (handle);
    }
#endif

  return result;
}

/* Get timestamp from the scrap SCRAP and set to *TIMPSTAMP.  */

static OSErr
get_scrap_private_timestamp (scrap, timestamp)
     ScrapRef scrap;
     unsigned long *timestamp;
{
  OSErr err = noErr;
#if TARGET_API_MAC_CARBON
  ScrapFlavorFlags flags;

  err = GetScrapFlavorFlags (scrap, SCRAP_FLAVOR_TYPE_EMACS_TIMESTAMP, &flags);
  if (err == noErr)
    if (!(flags & kScrapFlavorMaskSenderOnly))
      err = noTypeErr;
    else
      {
	Size size = sizeof (*timestamp);

	err = GetScrapFlavorData (scrap, SCRAP_FLAVOR_TYPE_EMACS_TIMESTAMP,
				  &size, timestamp);
	if (err == noErr && size != sizeof (*timestamp))
	  err = noTypeErr;
      }
#else  /* !TARGET_API_MAC_CARBON */
  Handle handle;
  SInt32 size, offset;

  size = GetScrap (NULL, SCRAP_FLAVOR_TYPE_EMACS_TIMESTAMP, &offset);
  if (size == sizeof (*timestamp))
    {
      handle = NewHandle (size);
      HLock (handle);
      size = GetScrap (handle, SCRAP_FLAVOR_TYPE_EMACS_TIMESTAMP, &offset);
      if (size == sizeof (*timestamp))
	*timestamp = *((unsigned long *) *handle);
      DisposeHandle (handle);
    }
  if (size != sizeof (*timestamp))
    err = noTypeErr;
#endif	/* !TARGET_API_MAC_CARBON */

  return err;
}

/* Get the list of target types in SCRAP.  The return value is a list
   of target type symbols possibly followed by scrap flavor type
   strings.  */

static Lisp_Object
get_scrap_target_type_list (scrap)
     ScrapRef scrap;
{
  Lisp_Object result = Qnil, rest, target_type;
#if TARGET_API_MAC_CARBON
  OSErr err;
  UInt32 count, i, type;
  ScrapFlavorInfo *flavor_info = NULL;
  Lisp_Object strings = Qnil;

  err = GetScrapFlavorCount (scrap, &count);
  if (err == noErr)
    flavor_info = xmalloc (sizeof (ScrapFlavorInfo) * count);
  if (flavor_info)
    {
      err = GetScrapFlavorInfoList (scrap, &count, flavor_info);
      if (err != noErr)
	{
	  xfree (flavor_info);
	  flavor_info = NULL;
	}
    }
  if (flavor_info == NULL)
    count = 0;
#endif
  for (rest = Vselection_converter_alist; CONSP (rest); rest = XCDR (rest))
    {
      ScrapFlavorType flavor_type = 0;

      if (CONSP (XCAR (rest)) && SYMBOLP (target_type = XCAR (XCAR (rest)))
	  && (flavor_type = scrap_has_target_type (scrap, target_type)))
	{
	  result = Fcons (target_type, result);
#if TARGET_API_MAC_CARBON
	  for (i = 0; i < count; i++)
	    if (flavor_info[i].flavorType == flavor_type)
	      {
		flavor_info[i].flavorType = 0;
		break;
	      }
#endif
	}
    }
#if TARGET_API_MAC_CARBON
  if (flavor_info)
    {
      for (i = 0; i < count; i++)
	if (flavor_info[i].flavorType)
	  {
	    type = EndianU32_NtoB (flavor_info[i].flavorType);
	    strings = Fcons (make_unibyte_string ((char *) &type, 4), strings);
	  }
      result = nconc2 (result, strings);
      xfree (flavor_info);
    }
#endif

  return result;
}

/* Do protocol to assert ourself as a selection owner.
   Update the Vselection_alist so that we can reply to later requests for
   our selection.  */

static void
x_own_selection (selection_name, selection_value)
     Lisp_Object selection_name, selection_value;
{
  OSErr err;
  ScrapRef scrap;
  struct gcpro gcpro1, gcpro2;
  Lisp_Object rest, handler_fn, value, type;
  int count;

  CHECK_SYMBOL (selection_name);

  GCPRO2 (selection_name, selection_value);

  BLOCK_INPUT;

  err = get_scrap_from_symbol (selection_name, 1, &scrap);
  if (err == noErr && scrap)
    {
      /* Don't allow a quit within the converter.
	 When the user types C-g, he would be surprised
	 if by luck it came during a converter.  */
      count = SPECPDL_INDEX ();
      specbind (Qinhibit_quit, Qt);

      for (rest = Vselection_converter_alist; CONSP (rest); rest = XCDR (rest))
	{
	  if (!(CONSP (XCAR (rest))
		&& SYMBOLP (type = XCAR (XCAR (rest)))
		&& valid_scrap_target_type_p (type)
		&& SYMBOLP (handler_fn = XCDR (XCAR (rest)))))
	    continue;

	  if (!NILP (handler_fn))
	    value = call3 (handler_fn, selection_name,
			   type, selection_value);

	  if (STRINGP (value))
	    err = put_scrap_string (scrap, type, value);
	  else if (CONSP (value)
		   && EQ (XCAR (value), type)
		   && STRINGP (XCDR (value)))
	    err = put_scrap_string (scrap, type, XCDR (value));
	}

      unbind_to (count, Qnil);

      if (err == noErr)
	err = put_scrap_private_timestamp (scrap, last_event_timestamp);
    }

  UNBLOCK_INPUT;

  UNGCPRO;

  if (scrap && err != noErr)
    error ("Can't set selection");

  /* Now update the local cache */
  {
    Lisp_Object selection_time;
    Lisp_Object selection_data;
    Lisp_Object prev_value;

    selection_time = long_to_cons (last_event_timestamp);
    selection_data = Fcons (selection_name,
			    Fcons (selection_value,
				   Fcons (selection_time,
					  Fcons (selected_frame, Qnil))));
    prev_value = assq_no_quit (selection_name, Vselection_alist);

    Vselection_alist = Fcons (selection_data, Vselection_alist);

    /* If we already owned the selection, remove the old selection data.
       Perhaps we should destructively modify it instead.
       Don't use Fdelq as that may QUIT.  */
    if (!NILP (prev_value))
      {
	Lisp_Object rest;	/* we know it's not the CAR, so it's easy.  */
	for (rest = Vselection_alist; !NILP (rest); rest = Fcdr (rest))
	  if (EQ (prev_value, Fcar (XCDR (rest))))
	    {
	      XSETCDR (rest, Fcdr (XCDR (rest)));
	      break;
	    }
      }
  }
}

/* Given a selection-name and desired type, look up our local copy of
   the selection value and convert it to the type.
   The value is nil or a string.
   This function is used both for remote requests (LOCAL_REQUEST is zero)
   and for local x-get-selection-internal (LOCAL_REQUEST is nonzero).

   This calls random Lisp code, and may signal or gc.  */

static Lisp_Object
x_get_local_selection (selection_symbol, target_type, local_request)
     Lisp_Object selection_symbol, target_type;
     int local_request;
{
  Lisp_Object local_value;
  Lisp_Object handler_fn, value, type, check;
  int count;

  if (NILP (Fx_selection_owner_p (selection_symbol)))
    return Qnil;

  local_value = assq_no_quit (selection_symbol, Vselection_alist);

  /* TIMESTAMP is a special case 'cause that's easiest.  */
  if (EQ (target_type, QTIMESTAMP))
    {
      handler_fn = Qnil;
      value = XCAR (XCDR (XCDR (local_value)));
    }
#if 0
  else if (EQ (target_type, QDELETE))
    {
      handler_fn = Qnil;
      Fx_disown_selection_internal
	(selection_symbol,
	 XCAR (XCDR (XCDR (local_value))));
      value = QNULL;
    }
#endif
  else
    {
      /* Don't allow a quit within the converter.
	 When the user types C-g, he would be surprised
	 if by luck it came during a converter.  */
      count = SPECPDL_INDEX ();
      specbind (Qinhibit_quit, Qt);

      CHECK_SYMBOL (target_type);
      handler_fn = Fcdr (Fassq (target_type, Vselection_converter_alist));
      /* gcpro is not needed here since nothing but HANDLER_FN
	 is live, and that ought to be a symbol.  */

      if (!NILP (handler_fn))
	value = call3 (handler_fn,
		       selection_symbol, (local_request ? Qnil : target_type),
		       XCAR (XCDR (local_value)));
      else
	value = Qnil;
      unbind_to (count, Qnil);
    }

  /* Make sure this value is of a type that we could transmit
     to another X client.  */

  check = value;
  if (CONSP (value)
      && SYMBOLP (XCAR (value)))
    type = XCAR (value),
    check = XCDR (value);

  if (STRINGP (check)
      || VECTORP (check)
      || SYMBOLP (check)
      || INTEGERP (check)
      || NILP (value))
    return value;
  /* Check for a value that cons_to_long could handle.  */
  else if (CONSP (check)
	   && INTEGERP (XCAR (check))
	   && (INTEGERP (XCDR (check))
	       ||
	       (CONSP (XCDR (check))
		&& INTEGERP (XCAR (XCDR (check)))
		&& NILP (XCDR (XCDR (check))))))
    return value;
  else
    return
      Fsignal (Qerror,
	       Fcons (build_string ("invalid data returned by selection-conversion function"),
		      Fcons (handler_fn, Fcons (value, Qnil))));
}


/* Clear all selections that were made from frame F.
   We do this when about to delete a frame.  */

void
x_clear_frame_selections (f)
     FRAME_PTR f;
{
  Lisp_Object frame;
  Lisp_Object rest;

  XSETFRAME (frame, f);

  /* Otherwise, we're really honest and truly being told to drop it.
     Don't use Fdelq as that may QUIT;.  */

  /* Delete elements from the beginning of Vselection_alist.  */
  while (!NILP (Vselection_alist)
	 && EQ (frame, Fcar (Fcdr (Fcdr (Fcdr (Fcar (Vselection_alist)))))))
    {
      /* Let random Lisp code notice that the selection has been stolen.  */
      Lisp_Object hooks, selection_symbol;

      hooks = Vx_lost_selection_functions;
      selection_symbol = Fcar (Fcar (Vselection_alist));

      if (!EQ (hooks, Qunbound)
	  && !NILP (Fx_selection_owner_p (selection_symbol)))
	{
	  for (; CONSP (hooks); hooks = Fcdr (hooks))
	    call1 (Fcar (hooks), selection_symbol);
#if 0 /* This can crash when deleting a frame
	 from x_connection_closed.  Anyway, it seems unnecessary;
	 something else should cause a redisplay.  */
	  redisplay_preserve_echo_area (21);
#endif
	}

      Vselection_alist = Fcdr (Vselection_alist);
    }

  /* Delete elements after the beginning of Vselection_alist.  */
  for (rest = Vselection_alist; !NILP (rest); rest = Fcdr (rest))
    if (EQ (frame, Fcar (Fcdr (Fcdr (Fcdr (Fcar (XCDR (rest))))))))
      {
	/* Let random Lisp code notice that the selection has been stolen.  */
	Lisp_Object hooks, selection_symbol;

	hooks = Vx_lost_selection_functions;
	selection_symbol = Fcar (Fcar (XCDR (rest)));

	if (!EQ (hooks, Qunbound)
	  && !NILP (Fx_selection_owner_p (selection_symbol)))
	  {
	    for (; CONSP (hooks); hooks = Fcdr (hooks))
	      call1 (Fcar (hooks), selection_symbol);
#if 0 /* See above */
	    redisplay_preserve_echo_area (22);
#endif
	  }
	XSETCDR (rest, Fcdr (XCDR (rest)));
	break;
      }
}

/* Do protocol to read selection-data from the server.
   Converts this to Lisp data and returns it.  */

static Lisp_Object
x_get_foreign_selection (selection_symbol, target_type, time_stamp)
     Lisp_Object selection_symbol, target_type, time_stamp;
{
  OSErr err;
  ScrapRef scrap;
  Lisp_Object result = Qnil;

  BLOCK_INPUT;

  err = get_scrap_from_symbol (selection_symbol, 0, &scrap);
  if (err == noErr && scrap)
    if (EQ (target_type, QTARGETS))
      {
	result = get_scrap_target_type_list (scrap);
	result = Fvconcat (1, &result);
      }
    else
      {
	result = get_scrap_string (scrap, target_type);
	if (STRINGP (result))
	  Fput_text_property (make_number (0), make_number (SBYTES (result)),
			      Qforeign_selection, target_type, result);
      }

  UNBLOCK_INPUT;

  return result;
}


DEFUN ("x-own-selection-internal", Fx_own_selection_internal,
       Sx_own_selection_internal, 2, 2, 0,
       doc: /* Assert a selection of the given TYPE with the given VALUE.
TYPE is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
VALUE is typically a string, or a cons of two markers, but may be
anything that the functions on `selection-converter-alist' know about.  */)
     (selection_name, selection_value)
     Lisp_Object selection_name, selection_value;
{
  check_mac ();
  CHECK_SYMBOL (selection_name);
  if (NILP (selection_value)) error ("SELECTION-VALUE may not be nil");
  x_own_selection (selection_name, selection_value);
  return selection_value;
}


/* Request the selection value from the owner.  If we are the owner,
   simply return our selection value.  If we are not the owner, this
   will block until all of the data has arrived.  */

DEFUN ("x-get-selection-internal", Fx_get_selection_internal,
       Sx_get_selection_internal, 2, 3, 0,
       doc: /* Return text selected from some Mac application.
SELECTION is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
TYPE is the type of data desired, typically `STRING'.
TIME_STAMP is ignored on Mac.  */)
     (selection_symbol, target_type, time_stamp)
     Lisp_Object selection_symbol, target_type, time_stamp;
{
  Lisp_Object val = Qnil;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (target_type, val); /* we store newly consed data into these */
  check_mac ();
  CHECK_SYMBOL (selection_symbol);
  CHECK_SYMBOL (target_type);

  val = x_get_local_selection (selection_symbol, target_type, 1);

  if (NILP (val))
    {
      val = x_get_foreign_selection (selection_symbol, target_type, time_stamp);
      goto DONE;
    }

  if (CONSP (val)
      && SYMBOLP (XCAR (val)))
    {
      val = XCDR (val);
      if (CONSP (val) && NILP (XCDR (val)))
	val = XCAR (val);
    }
 DONE:
  UNGCPRO;
  return val;
}

DEFUN ("x-disown-selection-internal", Fx_disown_selection_internal,
       Sx_disown_selection_internal, 1, 2, 0,
       doc: /* If we own the selection SELECTION, disown it.
Disowning it means there is no such selection.  */)
     (selection, time)
     Lisp_Object selection;
     Lisp_Object time;
{
  OSErr err;
  ScrapRef scrap;
  Lisp_Object local_selection_data;

  check_mac ();
  CHECK_SYMBOL (selection);

  if (NILP (Fx_selection_owner_p (selection)))
    return Qnil;  /* Don't disown the selection when we're not the owner.  */

  local_selection_data = assq_no_quit (selection, Vselection_alist);

  /* Don't use Fdelq as that may QUIT;.  */

  if (EQ (local_selection_data, Fcar (Vselection_alist)))
    Vselection_alist = Fcdr (Vselection_alist);
  else
    {
      Lisp_Object rest;
      for (rest = Vselection_alist; !NILP (rest); rest = Fcdr (rest))
	if (EQ (local_selection_data, Fcar (XCDR (rest))))
	  {
	    XSETCDR (rest, Fcdr (XCDR (rest)));
	    break;
	  }
    }

  /* Let random lisp code notice that the selection has been stolen.  */

  {
    Lisp_Object rest;
    rest = Vx_lost_selection_functions;
    if (!EQ (rest, Qunbound))
      {
	for (; CONSP (rest); rest = Fcdr (rest))
	  call1 (Fcar (rest), selection);
	prepare_menu_bars ();
	redisplay_preserve_echo_area (20);
      }
  }

  BLOCK_INPUT;

  err = get_scrap_from_symbol (selection, 0, &scrap);
  if (err == noErr && scrap)
    clear_scrap (&scrap);

  UNBLOCK_INPUT;

  return Qt;
}


DEFUN ("x-selection-owner-p", Fx_selection_owner_p, Sx_selection_owner_p,
       0, 1, 0,
       doc: /* Whether the current Emacs process owns the given SELECTION.
The arg should be the name of the selection in question, typically one of
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.
For convenience, the symbol nil is the same as `PRIMARY',
and t is the same as `SECONDARY'.  */)
     (selection)
     Lisp_Object selection;
{
  OSErr err;
  ScrapRef scrap;
  Lisp_Object result = Qnil, local_selection_data;

  check_mac ();
  CHECK_SYMBOL (selection);
  if (EQ (selection, Qnil)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;

  local_selection_data = assq_no_quit (selection, Vselection_alist);

  if (NILP (local_selection_data))
    return Qnil;

  BLOCK_INPUT;

  err = get_scrap_from_symbol (selection, 0, &scrap);
  if (err == noErr && scrap)
    {
      unsigned long timestamp;

      err = get_scrap_private_timestamp (scrap, &timestamp);
      if (err == noErr
	  && (timestamp
	      == cons_to_long (XCAR (XCDR (XCDR (local_selection_data))))))
	result = Qt;
    }
  else
    result = Qt;

  UNBLOCK_INPUT;

  return result;
}

DEFUN ("x-selection-exists-p", Fx_selection_exists_p, Sx_selection_exists_p,
       0, 1, 0,
       doc: /* Whether there is an owner for the given SELECTION.
The arg should be the name of the selection in question, typically one of
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.
For convenience, the symbol nil is the same as `PRIMARY',
and t is the same as `SECONDARY'.  */)
     (selection)
     Lisp_Object selection;
{
  OSErr err;
  ScrapRef scrap;
  Lisp_Object result = Qnil, rest;

  /* It should be safe to call this before we have an Mac frame.  */
  if (! FRAME_MAC_P (SELECTED_FRAME ()))
    return Qnil;

  CHECK_SYMBOL (selection);
  if (!NILP (Fx_selection_owner_p (selection)))
    return Qt;
  if (EQ (selection, Qnil)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;

  BLOCK_INPUT;

  err = get_scrap_from_symbol (selection, 0, &scrap);
  if (err == noErr && scrap)
    for (rest = Vselection_converter_alist; CONSP (rest); rest = XCDR (rest))
      {
	if (CONSP (XCAR (rest)) && SYMBOLP (XCAR (XCAR (rest)))
	    && scrap_has_target_type (scrap, XCAR (XCAR (rest))))
	  {
	    result = Qt;
	    break;
	  }
      }

  UNBLOCK_INPUT;

  return result;
}


int mac_ready_for_apple_events = 0;
static Lisp_Object Vmac_apple_event_map;
static Lisp_Object Qmac_apple_event_class, Qmac_apple_event_id;
static struct
{
  AppleEvent *buf;
  int size, count;
} deferred_apple_events;
extern Lisp_Object Qundefined;
extern OSErr mac_store_apple_event P_ ((Lisp_Object, Lisp_Object,
					const AEDesc *));

struct apple_event_binding
{
  UInt32 code;			/* Apple event class or ID.  */
  Lisp_Object key, binding;
};

static void
find_event_binding_fun (key, binding, args, data)
     Lisp_Object key, binding, args;
     void *data;
{
  struct apple_event_binding *event_binding =
    (struct apple_event_binding *)data;
  Lisp_Object code_string;

  if (!SYMBOLP (key))
    return;
  code_string = Fget (key, args);
  if (STRINGP (code_string) && SBYTES (code_string) == 4
      && (EndianU32_BtoN (*((UInt32 *) SDATA (code_string)))
	  == event_binding->code))
    {
      event_binding->key = key;
      event_binding->binding = binding;
    }
}

static void
find_event_binding (keymap, event_binding, class_p)
     Lisp_Object keymap;
     struct apple_event_binding *event_binding;
     int class_p;
{
  if (event_binding->code == 0)
    event_binding->binding =
      access_keymap (keymap, event_binding->key, 0, 1, 0);
  else
    {
      event_binding->binding = Qnil;
      map_keymap (keymap, find_event_binding_fun,
		  class_p ? Qmac_apple_event_class : Qmac_apple_event_id,
		  event_binding, 0);
    }
}

void
mac_find_apple_event_spec (class, id, class_key, id_key, binding)
     AEEventClass class;
     AEEventID id;
     Lisp_Object *class_key, *id_key, *binding;
{
  struct apple_event_binding event_binding;
  Lisp_Object keymap;

  *binding = Qnil;

  keymap = get_keymap (Vmac_apple_event_map, 0, 0);
  if (NILP (keymap))
    return;

  event_binding.code = class;
  event_binding.key = *class_key;
  event_binding.binding = Qnil;
  find_event_binding (keymap, &event_binding, 1);
  *class_key = event_binding.key;
  keymap = get_keymap (event_binding.binding, 0, 0);
  if (NILP (keymap))
    return;

  event_binding.code = id;
  event_binding.key = *id_key;
  event_binding.binding = Qnil;
  find_event_binding (keymap, &event_binding, 0);
  *id_key = event_binding.key;
  *binding = event_binding.binding;
}

static OSErr
defer_apple_events (apple_event, reply)
     const AppleEvent *apple_event, *reply;
{
  OSErr err;

  err = AESuspendTheCurrentEvent (apple_event);

  /* Mac OS 10.3 Xcode manual says AESuspendTheCurrentEvent makes
     copies of the Apple event and the reply, but Mac OS 10.4 Xcode
     manual says it doesn't.  Anyway we create copies of them and save
     it in `deferred_apple_events'.  */
  if (err == noErr)
    {
      if (deferred_apple_events.buf == NULL)
	{
	  deferred_apple_events.size = 16;
	  deferred_apple_events.count = 0;
	  deferred_apple_events.buf =
	    xmalloc (sizeof (AppleEvent) * deferred_apple_events.size);
	  if (deferred_apple_events.buf == NULL)
	    err = memFullErr;
	}
      else if (deferred_apple_events.count == deferred_apple_events.size)
	{
	  AppleEvent *newbuf;

	  deferred_apple_events.size *= 2;
	  newbuf = xrealloc (deferred_apple_events.buf,
			     sizeof (AppleEvent) * deferred_apple_events.size);
	  if (newbuf)
	    deferred_apple_events.buf = newbuf;
	  else
	    err = memFullErr;
	}
    }

  if (err == noErr)
    {
      int count = deferred_apple_events.count;

      AEDuplicateDesc (apple_event, deferred_apple_events.buf + count);
      AEDuplicateDesc (reply, deferred_apple_events.buf + count + 1);
      deferred_apple_events.count += 2;
    }

  return err;
}

static pascal OSErr
mac_handle_apple_event (apple_event, reply, refcon)
     const AppleEvent *apple_event;
     AppleEvent *reply;
     SInt32 refcon;
{
  OSErr err;
  AEEventClass event_class;
  AEEventID event_id;
  Lisp_Object class_key, id_key, binding;

  /* We can't handle an Apple event that requests a reply, but this
     seems to be too restrictive.  */
#if 0
  if (reply->descriptorType != typeNull)
    return errAEEventNotHandled;
#endif

  if (!mac_ready_for_apple_events)
    {
      err = defer_apple_events (apple_event, reply);
      if (err != noErr)
	return errAEEventNotHandled;
      return noErr;
    }

  err = AEGetAttributePtr (apple_event, keyEventClassAttr, typeType, NULL,
			   &event_class, sizeof (AEEventClass), NULL);
  if (err == noErr)
    err = AEGetAttributePtr (apple_event, keyEventIDAttr, typeType, NULL,
			     &event_id, sizeof (AEEventID), NULL);
  if (err == noErr)
    {
      mac_find_apple_event_spec (event_class, event_id,
				 &class_key, &id_key, &binding);
      if (!NILP (binding) && !EQ (binding, Qundefined))
	{
	  if (INTEGERP (binding))
	    return XINT (binding);
	  err = mac_store_apple_event (class_key, id_key, apple_event);
	  if (err == noErr)
	    return noErr;
	}
    }
  return errAEEventNotHandled;
}

void
init_apple_event_handler ()
{
  OSErr err;
  long result;

  /* Make sure we have Apple events before starting.  */
  err = Gestalt (gestaltAppleEventsAttr, &result);
  if (err != noErr)
    abort ();

  if (!(result & (1 << gestaltAppleEventsPresent)))
    abort ();

  err = AEInstallEventHandler (typeWildCard, typeWildCard,
#if TARGET_API_MAC_CARBON
			       NewAEEventHandlerUPP (mac_handle_apple_event),
#else
			       NewAEEventHandlerProc (mac_handle_apple_event),
#endif
			       0L, false);
  if (err != noErr)
    abort ();
}

DEFUN ("mac-process-deferred-apple-events", Fmac_process_deferred_apple_events, Smac_process_deferred_apple_events, 0, 0, 0,
       doc: /* Process Apple events that are deferred at the startup time.  */)
  ()
{
  OSErr err;
  Lisp_Object result = Qnil;
  long i, count;
  AppleEvent apple_event, reply;
  AEKeyword keyword;

  if (mac_ready_for_apple_events)
    return Qnil;

  BLOCK_INPUT;
  mac_ready_for_apple_events = 1;
  if (deferred_apple_events.buf)
    {
      for (i = 0; i < deferred_apple_events.count; i += 2)
	{
	  AEResumeTheCurrentEvent (deferred_apple_events.buf + i,
				   deferred_apple_events.buf + i + 1,
				   ((AEEventHandlerUPP)
				    kAEUseStandardDispatch), 0);
	  AEDisposeDesc (deferred_apple_events.buf + i);
	  AEDisposeDesc (deferred_apple_events.buf + i + 1);
	}
      xfree (deferred_apple_events.buf);
      bzero (&deferred_apple_events, sizeof (deferred_apple_events));

      result = Qt;
    }
  UNBLOCK_INPUT;

  return result;
}


#ifdef MAC_OSX
void
init_service_handler ()
{
  EventTypeSpec specs[] = {{kEventClassService, kEventServiceGetTypes},
			   {kEventClassService, kEventServiceCopy},
			   {kEventClassService, kEventServicePaste},
			   {kEventClassService, kEventServicePerform}};
  InstallApplicationEventHandler (NewEventHandlerUPP (mac_handle_service_event),
				  GetEventTypeCount (specs), specs, NULL, NULL);
}

extern OSErr mac_store_services_event P_ ((EventRef));

static OSStatus
copy_scrap_flavor_data (from_scrap, to_scrap, flavor_type)
     ScrapRef from_scrap, to_scrap;
     ScrapFlavorType flavor_type;
{
  OSStatus err;
  Size size, size_allocated;
  char *buf = NULL;

  err = GetScrapFlavorSize (from_scrap, flavor_type, &size);
  if (err == noErr)
    buf = xmalloc (size);
  while (buf)
    {
      size_allocated = size;
      err = GetScrapFlavorData (from_scrap, flavor_type, &size, buf);
      if (err != noErr)
	{
	  xfree (buf);
	  buf = NULL;
	}
      else if (size_allocated < size)
	{
	  char *newbuf = xrealloc (buf, size);

	  if (newbuf)
	    buf = newbuf;
	  else
	    {
	      xfree (buf);
	      buf = NULL;
	    }
	}
      else
	break;
    }
  if (err == noErr)
    if (buf == NULL)
      err = memFullErr;
    else
      {
	err = PutScrapFlavor (to_scrap, flavor_type, kScrapFlavorMaskNone,
			      size, buf);
	xfree (buf);
      }

  return err;
}

static OSStatus
mac_handle_service_event (call_ref, event, data)
     EventHandlerCallRef call_ref;
     EventRef event;
     void *data;
{
  OSStatus err = noErr;
  ScrapRef cur_scrap, specific_scrap;
  UInt32 event_kind = GetEventKind (event);
  CFMutableArrayRef copy_types, paste_types;
  CFStringRef type;
  Lisp_Object rest;
  ScrapFlavorType flavor_type;

  /* Check if Vmac_services_selection is a valid selection that has a
     corresponding scrap.  */
  if (!SYMBOLP (Vmac_services_selection))
    err = eventNotHandledErr;
  else
    err = get_scrap_from_symbol (Vmac_services_selection, 0, &cur_scrap);
  if (!(err == noErr && cur_scrap))
    return eventNotHandledErr;

  switch (event_kind)
    {
    case kEventServiceGetTypes:
      /* Set paste types. */
      err = GetEventParameter (event, kEventParamServicePasteTypes,
			       typeCFMutableArrayRef, NULL,
			       sizeof (CFMutableArrayRef), NULL,
			       &paste_types);
      if (err != noErr)
	break;

      for (rest = Vselection_converter_alist; CONSP (rest);
	   rest = XCDR (rest))
	if (CONSP (XCAR (rest)) && SYMBOLP (XCAR (XCAR (rest)))
	    && (flavor_type =
		get_flavor_type_from_symbol (XCAR (XCAR (rest)))))
	  {
	    type = CreateTypeStringWithOSType (flavor_type);
	    if (type)
	      {
		CFArrayAppendValue (paste_types, type);
		CFRelease (type);
	      }
	  }

      /* Set copy types.  */
      err = GetEventParameter (event, kEventParamServiceCopyTypes,
			       typeCFMutableArrayRef, NULL,
			       sizeof (CFMutableArrayRef), NULL,
			       &copy_types);
      if (err != noErr)
	break;

      if (NILP (Fx_selection_owner_p (Vmac_services_selection)))
	break;
      else
	goto copy_all_flavors;

    case kEventServiceCopy:
      err = GetEventParameter (event, kEventParamScrapRef,
			       typeScrapRef, NULL,
			       sizeof (ScrapRef), NULL, &specific_scrap);
      if (err != noErr
	  || NILP (Fx_selection_owner_p (Vmac_services_selection)))
	{
	  err = eventNotHandledErr;
	  break;
	}

    copy_all_flavors:
      {
	UInt32 count, i;
	ScrapFlavorInfo *flavor_info = NULL;
	ScrapFlavorFlags flags;

	err = GetScrapFlavorCount (cur_scrap, &count);
	if (err == noErr)
	  flavor_info = xmalloc (sizeof (ScrapFlavorInfo) * count);
	if (flavor_info)
	  {
	    err = GetScrapFlavorInfoList (cur_scrap, &count, flavor_info);
	    if (err != noErr)
	      {
		xfree (flavor_info);
		flavor_info = NULL;
	      }
	  }
	if (flavor_info == NULL)
	  break;

	for (i = 0; i < count; i++)
	  {
	    flavor_type = flavor_info[i].flavorType;
	    err = GetScrapFlavorFlags (cur_scrap, flavor_type, &flags);
	    if (err == noErr && !(flags & kScrapFlavorMaskSenderOnly))
	      {
		if (event_kind == kEventServiceCopy)
		  err = copy_scrap_flavor_data (cur_scrap, specific_scrap,
						flavor_type);
		else	     /* event_kind == kEventServiceGetTypes */
		  {
		    type = CreateTypeStringWithOSType (flavor_type);
		    if (type)
		      {
			CFArrayAppendValue (copy_types, type);
			CFRelease (type);
		      }
		  }
	      }
	  }
	xfree (flavor_info);
      }
      break;

    case kEventServicePaste:
    case kEventServicePerform:
      {
	int data_exists_p = 0;

        err = GetEventParameter (event, kEventParamScrapRef, typeScrapRef,
				 NULL, sizeof (ScrapRef), NULL,
				 &specific_scrap);
	if (err == noErr)
	  err = clear_scrap (&cur_scrap);
	if (err == noErr)
	  for (rest = Vselection_converter_alist; CONSP (rest);
	       rest = XCDR (rest))
	    {
	      if (! (CONSP (XCAR (rest)) && SYMBOLP (XCAR (XCAR (rest)))))
		continue;
	      flavor_type = get_flavor_type_from_symbol (XCAR (XCAR (rest)));
	      if (flavor_type == 0)
		continue;
	      err = copy_scrap_flavor_data (specific_scrap, cur_scrap,
					    flavor_type);
	      if (err == noErr)
		data_exists_p = 1;
	    }
	if (!data_exists_p)
	  err = eventNotHandledErr;
	else
	  err = mac_store_services_event (event);
      }
      break;
    }

  if (err != noErr)
    err = eventNotHandledErr;
  return err;
}
#endif


void
syms_of_macselect ()
{
  defsubr (&Sx_get_selection_internal);
  defsubr (&Sx_own_selection_internal);
  defsubr (&Sx_disown_selection_internal);
  defsubr (&Sx_selection_owner_p);
  defsubr (&Sx_selection_exists_p);
  defsubr (&Smac_process_deferred_apple_events);

  Vselection_alist = Qnil;
  staticpro (&Vselection_alist);

  DEFVAR_LISP ("selection-converter-alist", &Vselection_converter_alist,
	       doc: /* An alist associating selection-types with functions.
These functions are called to convert the selection, with three args:
the name of the selection (typically `PRIMARY', `SECONDARY', or `CLIPBOARD');
a desired type to which the selection should be converted;
and the local selection value (whatever was given to `x-own-selection').

The function should return the value to send to the Scrap Manager
\(must be a string).  A return value of nil
means that the conversion could not be done.
A return value which is the symbol `NULL'
means that a side-effect was executed,
and there is no meaningful selection value.  */);
  Vselection_converter_alist = Qnil;

  DEFVAR_LISP ("x-lost-selection-functions", &Vx_lost_selection_functions,
	       doc: /* A list of functions to be called when Emacs loses a selection.
\(This happens when a Lisp program explicitly clears the selection.)
The functions are called with one argument, the selection type
\(a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD').  */);
  Vx_lost_selection_functions = Qnil;

  DEFVAR_LISP ("selection-coding-system", &Vselection_coding_system,
	       doc: /* Coding system for communicating with other programs.
When sending or receiving text via cut_buffer, selection, and clipboard,
the text is encoded or decoded by this coding system.
The default value is determined by the system script code.  */);
  Vselection_coding_system = Qnil;

  DEFVAR_LISP ("next-selection-coding-system", &Vnext_selection_coding_system,
	       doc: /* Coding system for the next communication with other programs.
Usually, `selection-coding-system' is used for communicating with
other programs.  But, if this variable is set, it is used for the
next communication only.  After the communication, this variable is
set to nil.  */);
  Vnext_selection_coding_system = Qnil;

  DEFVAR_LISP ("mac-apple-event-map", &Vmac_apple_event_map,
	       doc: /* Keymap for Apple events handled by Emacs.  */);
  Vmac_apple_event_map = Qnil;

#ifdef MAC_OSX
  DEFVAR_LISP ("mac-services-selection", &Vmac_services_selection,
	       doc: /* Selection name for communication via Services menu.  */);
  Vmac_services_selection = intern ("PRIMARY");
#endif

  QPRIMARY   = intern ("PRIMARY");	staticpro (&QPRIMARY);
  QSECONDARY = intern ("SECONDARY");	staticpro (&QSECONDARY);
  QTIMESTAMP = intern ("TIMESTAMP");	staticpro (&QTIMESTAMP);
  QTARGETS   = intern ("TARGETS");	staticpro (&QTARGETS);

  Qforeign_selection = intern ("foreign-selection");
  staticpro (&Qforeign_selection);

  Qmac_scrap_name = intern ("mac-scrap-name");
  staticpro (&Qmac_scrap_name);

  Qmac_ostype = intern ("mac-ostype");
  staticpro (&Qmac_ostype);

  Qmac_apple_event_class = intern ("mac-apple-event-class");
  staticpro (&Qmac_apple_event_class);

  Qmac_apple_event_id = intern ("mac-apple-event-id");
  staticpro (&Qmac_apple_event_id);
}

/* arch-tag: f3c91ad8-99e0-4bd6-9eef-251b2f848732
   (do not change this comment) */
