/* Selection processing for Emacs on Mac OS.
   Copyright (C) 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

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

#include <config.h>

#include "lisp.h"
#include "macterm.h"
#include "blockinput.h"
#include "keymap.h"

#if TARGET_API_MAC_CARBON
typedef ScrapRef Selection;
#else  /* !TARGET_API_MAC_CARBON */
#include <Scrap.h>
#include <Endian.h>
typedef int Selection;
#endif /* !TARGET_API_MAC_CARBON */

static OSStatus mac_get_selection_from_symbol P_ ((Lisp_Object, int,
						   Selection *));
static ScrapFlavorType get_flavor_type_from_symbol P_ ((Lisp_Object,
							Selection));
static int mac_valid_selection_target_p P_ ((Lisp_Object));
static OSStatus mac_clear_selection P_ ((Selection *));
static Lisp_Object mac_get_selection_ownership_info P_ ((Selection));
static int mac_valid_selection_value_p P_ ((Lisp_Object, Lisp_Object));
static OSStatus mac_put_selection_value P_ ((Selection, Lisp_Object,
					     Lisp_Object));
static int mac_selection_has_target_p P_ ((Selection, Lisp_Object));
static Lisp_Object mac_get_selection_value P_ ((Selection, Lisp_Object));
static Lisp_Object mac_get_selection_target_list P_ ((Selection));
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
/* Coding system for communicating with other programs via selections.  */
static Lisp_Object Vselection_coding_system;

/* Coding system for the next communicating with other programs.  */
static Lisp_Object Vnext_selection_coding_system;

static Lisp_Object Qforeign_selection;

/* The timestamp of the last input event Emacs received from the
   window server.  */
/* Defined in keyboard.c.  */
extern unsigned long last_event_timestamp;

/* This is an association list whose elements are of the form
     ( SELECTION-NAME SELECTION-VALUE SELECTION-TIMESTAMP FRAME OWNERSHIP-INFO)
   SELECTION-NAME is a lisp symbol.
   SELECTION-VALUE is the value that emacs owns for that selection.
     It may be any kind of Lisp object.
   SELECTION-TIMESTAMP is the time at which emacs began owning this selection,
     as a cons of two 16-bit numbers (making a 32 bit time.)
   FRAME is the frame for which we made the selection.
   OWNERSHIP-INFO is a value saved when emacs owns for that selection.
     If another application takes the ownership of that selection
     later, then newly examined ownership info value should be
     different from the saved one.
   If there is an entry in this alist, the current ownership info for
    the selection coincides with OWNERSHIP-INFO, then it can be
    assumed that Emacs owns that selection.
   The only (eq) parts of this list that are visible from Lisp are the
    selection-values.  */
static Lisp_Object Vselection_alist;

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
static Lisp_Object Vmac_service_selection;
#endif

/* Get a reference to the selection corresponding to the symbol SYM.
   The reference is set to *SEL, and it becomes NULL if there's no
   corresponding selection.  Clear the selection if CLEAR_P is
   non-zero.  */

static OSStatus
mac_get_selection_from_symbol (sym, clear_p, sel)
     Lisp_Object sym;
     int clear_p;
     Selection *sel;
{
  OSStatus err = noErr;
  Lisp_Object str = Fget (sym, Qmac_scrap_name);

  if (!STRINGP (str))
    *sel = NULL;
  else
    {
#if TARGET_API_MAC_CARBON
#ifdef MAC_OSX
      CFStringRef scrap_name = cfstring_create_with_string (str);
      OptionBits options = (clear_p ? kScrapClearNamedScrap
			    : kScrapGetNamedScrap);

      err = GetScrapByName (scrap_name, options, sel);
      CFRelease (scrap_name);
#else	/* !MAC_OSX */
      if (clear_p)
	err = ClearCurrentScrap ();
      if (err == noErr)
	err = GetCurrentScrap (sel);
#endif	/* !MAC_OSX */
#else	/* !TARGET_API_MAC_CARBON */
      if (clear_p)
	err = ZeroScrap ();
      if (err == noErr)
	*sel = 1;
#endif	/* !TARGET_API_MAC_CARBON */
    }

  return err;
}

/* Get a scrap flavor type from the symbol SYM.  Return 0 if no
   corresponding flavor type.  If SEL is non-zero, the return value is
   non-zero only when the SEL has the flavor type.  */

static ScrapFlavorType
get_flavor_type_from_symbol (sym, sel)
     Lisp_Object sym;
     Selection sel;
{
  Lisp_Object str = Fget (sym, Qmac_ostype);
  ScrapFlavorType flavor_type;

  if (STRINGP (str) && SBYTES (str) == 4)
    flavor_type = EndianU32_BtoN (*((UInt32 *) SDATA (str)));
  else
    flavor_type = 0;

  if (flavor_type && sel)
    {
#if TARGET_API_MAC_CARBON
      OSStatus err;
      ScrapFlavorFlags flags;

      err = GetScrapFlavorFlags (sel, flavor_type, &flags);
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

/* Check if the symbol SYM has a corresponding selection target type.  */

static int
mac_valid_selection_target_p (sym)
     Lisp_Object sym;
{
  return get_flavor_type_from_symbol (sym, 0) != 0;
}

/* Clear the selection whose reference is *SEL.  */

static OSStatus
mac_clear_selection (sel)
     Selection *sel;
{
#if TARGET_API_MAC_CARBON
#ifdef MAC_OSX
  return ClearScrap (sel);
#else
  OSStatus err;

  err = ClearCurrentScrap ();
  if (err == noErr)
    err = GetCurrentScrap (sel);
  return err;
#endif
#else  /* !TARGET_API_MAC_CARBON */
  return ZeroScrap ();
#endif	/* !TARGET_API_MAC_CARBON */
}

/* Get ownership information for SEL.  Emacs can detect a change of
   the ownership by comparing saved and current values of the
   ownership information.  */

static Lisp_Object
mac_get_selection_ownership_info (sel)
     Selection sel;
{
#if TARGET_API_MAC_CARBON
  return long_to_cons ((unsigned long) sel);
#else  /* !TARGET_API_MAC_CARBON */
  ScrapStuffPtr scrap_info = InfoScrap ();

  return make_number (scrap_info->scrapCount);
#endif	/* !TARGET_API_MAC_CARBON */
}

/* Return non-zero if VALUE is a valid selection value for TARGET.  */

static int
mac_valid_selection_value_p (value, target)
     Lisp_Object value, target;
{
  return STRINGP (value);
}

/* Put Lisp Object VALUE to the selection SEL.  The target type is
   specified by TARGET. */

static OSStatus
mac_put_selection_value (sel, target, value)
     Selection sel;
     Lisp_Object target, value;
{
  ScrapFlavorType flavor_type = get_flavor_type_from_symbol (target, 0);

  if (flavor_type == 0 || !STRINGP (value))
    return noTypeErr;

#if TARGET_API_MAC_CARBON
  return PutScrapFlavor (sel, flavor_type, kScrapFlavorMaskNone,
			 SBYTES (value), SDATA (value));
#else  /* !TARGET_API_MAC_CARBON */
  return PutScrap (SBYTES (value), flavor_type, SDATA (value));
#endif	/* !TARGET_API_MAC_CARBON */
}

/* Check if data for the target type TARGET is available in SEL.  */

static int
mac_selection_has_target_p (sel, target)
     Selection sel;
     Lisp_Object target;
{
  return get_flavor_type_from_symbol (target, sel) != 0;
}

/* Get data for the target type TARGET from SEL and create a Lisp
   string.  Return nil if failed to get data.  */

static Lisp_Object
mac_get_selection_value (sel, target)
     Selection sel;
     Lisp_Object target;
{
  OSStatus err;
  Lisp_Object result = Qnil;
  ScrapFlavorType flavor_type = get_flavor_type_from_symbol (target, sel);
#if TARGET_API_MAC_CARBON
  Size size;

  if (flavor_type)
    {
      err = GetScrapFlavorSize (sel, flavor_type, &size);
      if (err == noErr)
	{
	  do
	    {
	      result = make_uninit_string (size);
	      err = GetScrapFlavorData (sel, flavor_type,
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

/* Get the list of target types in SEL.  The return value is a list of
   target type symbols possibly followed by scrap flavor type
   strings.  */

static Lisp_Object
mac_get_selection_target_list (sel)
     Selection sel;
{
  Lisp_Object result = Qnil, rest, target;
#if TARGET_API_MAC_CARBON
  OSStatus err;
  UInt32 count, i, type;
  ScrapFlavorInfo *flavor_info = NULL;
  Lisp_Object strings = Qnil;

  err = GetScrapFlavorCount (sel, &count);
  if (err == noErr)
    flavor_info = xmalloc (sizeof (ScrapFlavorInfo) * count);
  err = GetScrapFlavorInfoList (sel, &count, flavor_info);
  if (err != noErr)
    {
      xfree (flavor_info);
      flavor_info = NULL;
    }
  if (flavor_info == NULL)
    count = 0;
#endif
  for (rest = Vselection_converter_alist; CONSP (rest); rest = XCDR (rest))
    {
      ScrapFlavorType flavor_type = 0;

      if (CONSP (XCAR (rest))
	  && (target = XCAR (XCAR (rest)),
	      SYMBOLP (target))
	  && (flavor_type = get_flavor_type_from_symbol (target, sel)))
	{
	  result = Fcons (target, result);
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
  OSStatus err;
  Selection sel;
  struct gcpro gcpro1, gcpro2;
  Lisp_Object rest, handler_fn, value, target_type;
  int count;

  CHECK_SYMBOL (selection_name);

  GCPRO2 (selection_name, selection_value);

  BLOCK_INPUT;

  err = mac_get_selection_from_symbol (selection_name, 1, &sel);
  if (err == noErr && sel)
    {
      /* Don't allow a quit within the converter.
	 When the user types C-g, he would be surprised
	 if by luck it came during a converter.  */
      count = SPECPDL_INDEX ();
      specbind (Qinhibit_quit, Qt);

      for (rest = Vselection_converter_alist; CONSP (rest); rest = XCDR (rest))
	{
	  if (!(CONSP (XCAR (rest))
		&& (target_type = XCAR (XCAR (rest)),
		    SYMBOLP (target_type))
		&& mac_valid_selection_target_p (target_type)
		&& (handler_fn = XCDR (XCAR (rest)),
		    SYMBOLP (handler_fn))))
	    continue;

	  if (!NILP (handler_fn))
	    value = call3 (handler_fn, selection_name,
			   target_type, selection_value);

	  if (NILP (value))
	    continue;

	  if (mac_valid_selection_value_p (value, target_type))
	    err = mac_put_selection_value (sel, target_type, value);
	  else if (CONSP (value)
		   && EQ (XCAR (value), target_type)
		   && mac_valid_selection_value_p (XCDR (value), target_type))
	    err = mac_put_selection_value (sel, target_type, XCDR (value));
	}

      unbind_to (count, Qnil);
    }

  UNBLOCK_INPUT;

  UNGCPRO;

  if (sel && err != noErr)
    error ("Can't set selection");

  /* Now update the local cache */
  {
    Lisp_Object selection_time;
    Lisp_Object selection_data;
    Lisp_Object ownership_info;
    Lisp_Object prev_value;

    selection_time = long_to_cons (last_event_timestamp);
    if (sel)
      {
	BLOCK_INPUT;
	ownership_info = mac_get_selection_ownership_info (sel);
	UNBLOCK_INPUT;
      }
    else
      ownership_info = Qnil; 	/* dummy value for local-only selection */
    selection_data = Fcons (selection_name,
			    Fcons (selection_value,
				   Fcons (selection_time,
					  Fcons (selected_frame,
						 Fcons (ownership_info,
							Qnil)))));
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

  if (local_request)
    return value;

  /* Make sure this value is of a type that we could transmit
     to another application.  */

  type = target_type;
  check = value;
  if (CONSP (value)
      && SYMBOLP (XCAR (value)))
    type = XCAR (value),
    check = XCDR (value);

  if (NILP (value) || mac_valid_selection_value_p (check, type))
    return value;

  signal_error ("Invalid data returned by selection-conversion function",
		list2 (handler_fn, value));
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
  OSStatus err;
  Selection sel;
  Lisp_Object result = Qnil;

  BLOCK_INPUT;

  err = mac_get_selection_from_symbol (selection_symbol, 0, &sel);
  if (err == noErr && sel)
    {
      if (EQ (target_type, QTARGETS))
	{
	  result = mac_get_selection_target_list (sel);
	  result = Fvconcat (1, &result);
	}
      else
	{
	  result = mac_get_selection_value (sel, target_type);
	  if (STRINGP (result))
	    Fput_text_property (make_number (0), make_number (SBYTES (result)),
				Qforeign_selection, target_type, result);
	}
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
  OSStatus err;
  Selection sel;
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

  err = mac_get_selection_from_symbol (selection, 0, &sel);
  if (err == noErr && sel)
    mac_clear_selection (&sel);

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
  OSStatus err;
  Selection sel;
  Lisp_Object result = Qnil, local_selection_data;

  check_mac ();
  CHECK_SYMBOL (selection);
  if (EQ (selection, Qnil)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;

  local_selection_data = assq_no_quit (selection, Vselection_alist);

  if (NILP (local_selection_data))
    return Qnil;

  BLOCK_INPUT;

  err = mac_get_selection_from_symbol (selection, 0, &sel);
  if (err == noErr && sel)
    {
      Lisp_Object ownership_info;

      ownership_info = XCAR (XCDR (XCDR (XCDR (XCDR (local_selection_data)))));
      if (!NILP (Fequal (ownership_info,
			 mac_get_selection_ownership_info (sel))))
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
  OSStatus err;
  Selection sel;
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

  err = mac_get_selection_from_symbol (selection, 0, &sel);
  if (err == noErr && sel)
    for (rest = Vselection_converter_alist; CONSP (rest); rest = XCDR (rest))
      {
	if (CONSP (XCAR (rest)) && SYMBOLP (XCAR (XCAR (rest)))
	    && mac_selection_has_target_p (sel, XCAR (XCAR (rest))))
	  {
	    result = Qt;
	    break;
	  }
      }

  UNBLOCK_INPUT;

  return result;
}


/***********************************************************************
			 Apple event support
***********************************************************************/
int mac_ready_for_apple_events = 0;
static Lisp_Object Vmac_apple_event_map;
static Lisp_Object Qmac_apple_event_class, Qmac_apple_event_id;
static Lisp_Object Qemacs_suspension_id;
extern Lisp_Object Qundefined;
extern void mac_store_apple_event P_ ((Lisp_Object, Lisp_Object,
				       const AEDesc *));

struct apple_event_binding
{
  UInt32 code;			/* Apple event class or ID.  */
  Lisp_Object key, binding;
};

struct suspended_ae_info
{
  UInt32 expiration_tick, suspension_id;
  AppleEvent apple_event, reply;
  struct suspended_ae_info *next;
};

/* List of apple events deferred at the startup time.  */
static struct suspended_ae_info *deferred_apple_events = NULL;

/* List of suspended apple events, in order of expiration_tick.  */
static struct suspended_ae_info *suspended_apple_events = NULL;

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
  struct suspended_ae_info *new;

  new = xmalloc (sizeof (struct suspended_ae_info));
  bzero (new, sizeof (struct suspended_ae_info));
  new->apple_event.descriptorType = typeNull;
  new->reply.descriptorType = typeNull;

  err = AESuspendTheCurrentEvent (apple_event);

  /* Mac OS 10.3 Xcode manual says AESuspendTheCurrentEvent makes
     copies of the Apple event and the reply, but Mac OS 10.4 Xcode
     manual says it doesn't.  Anyway we create copies of them and save
     them in `deferred_apple_events'.  */
  if (err == noErr)
    err = AEDuplicateDesc (apple_event, &new->apple_event);
  if (err == noErr)
    err = AEDuplicateDesc (reply, &new->reply);
  if (err == noErr)
    {
      new->next = deferred_apple_events;
      deferred_apple_events = new;
    }
  else
    {
      AEDisposeDesc (&new->apple_event);
      AEDisposeDesc (&new->reply);
      xfree (new);
    }

  return err;
}

static OSErr
mac_handle_apple_event_1 (class, id, apple_event, reply)
     Lisp_Object class, id;
     const AppleEvent *apple_event;
     AppleEvent *reply;
{
  OSErr err;
  static UInt32 suspension_id = 0;
  struct suspended_ae_info *new;

  new = xmalloc (sizeof (struct suspended_ae_info));
  bzero (new, sizeof (struct suspended_ae_info));
  new->apple_event.descriptorType = typeNull;
  new->reply.descriptorType = typeNull;

  err = AESuspendTheCurrentEvent (apple_event);
  if (err == noErr)
    err = AEDuplicateDesc (apple_event, &new->apple_event);
  if (err == noErr)
    err = AEDuplicateDesc (reply, &new->reply);
  if (err == noErr)
    err = AEPutAttributePtr (&new->apple_event, KEY_EMACS_SUSPENSION_ID_ATTR,
			     typeUInt32, &suspension_id, sizeof (UInt32));
  if (err == noErr)
    {
      OSErr err1;
      SInt32 reply_requested;

      err1 = AEGetAttributePtr (&new->apple_event, keyReplyRequestedAttr,
				typeSInt32, NULL, &reply_requested,
				sizeof (SInt32), NULL);
      if (err1 != noErr)
	{
	  /* Emulate keyReplyRequestedAttr in older versions.  */
	  reply_requested = reply->descriptorType != typeNull;
	  err = AEPutAttributePtr (&new->apple_event, keyReplyRequestedAttr,
				   typeSInt32, &reply_requested,
				   sizeof (SInt32));
	}
    }
  if (err == noErr)
    {
      SInt32 timeout = 0;
      struct suspended_ae_info **p;

      new->suspension_id = suspension_id;
      suspension_id++;
      err = AEGetAttributePtr (apple_event, keyTimeoutAttr, typeSInt32,
			       NULL, &timeout, sizeof (SInt32), NULL);
      new->expiration_tick = TickCount () + timeout;

      for (p = &suspended_apple_events; *p; p = &(*p)->next)
	if ((*p)->expiration_tick >= new->expiration_tick)
	  break;
      new->next = *p;
      *p = new;

      mac_store_apple_event (class, id, &new->apple_event);
    }
  else
    {
      AEDisposeDesc (&new->reply);
      AEDisposeDesc (&new->apple_event);
      xfree (new);
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
  UInt32 suspension_id;
  AEEventClass event_class;
  AEEventID event_id;
  Lisp_Object class_key, id_key, binding;

  if (!mac_ready_for_apple_events)
    {
      err = defer_apple_events (apple_event, reply);
      if (err != noErr)
	return errAEEventNotHandled;
      return noErr;
    }

  err = AEGetAttributePtr (apple_event, KEY_EMACS_SUSPENSION_ID_ATTR,
			   typeUInt32, NULL,
			   &suspension_id, sizeof (UInt32), NULL);
  if (err == noErr)
    /* Previously suspended event.  Pass it to the next handler.  */
    return errAEEventNotHandled;

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
	  err = mac_handle_apple_event_1 (class_key, id_key,
					  apple_event, reply);
	}
      else
	err = errAEEventNotHandled;
    }
  if (err == noErr)
    return noErr;
  else
    return errAEEventNotHandled;
}

static int
cleanup_suspended_apple_events (head, all_p)
     struct suspended_ae_info **head;
     int all_p;
{
  UInt32 current_tick = TickCount (), nresumed = 0;
  struct suspended_ae_info *p, *next;

  for (p = *head; p; p = next)
    {
      if (!all_p && p->expiration_tick > current_tick)
	break;
      AESetTheCurrentEvent (&p->apple_event);
      AEResumeTheCurrentEvent (&p->apple_event, &p->reply,
			       (AEEventHandlerUPP) kAENoDispatch, 0);
      AEDisposeDesc (&p->reply);
      AEDisposeDesc (&p->apple_event);
      nresumed++;
      next = p->next;
      xfree (p);
    }
  *head = p;

  return nresumed;
}

static void
cleanup_all_suspended_apple_events ()
{
  cleanup_suspended_apple_events (&deferred_apple_events, 1);
  cleanup_suspended_apple_events (&suspended_apple_events, 1);
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

  atexit (cleanup_all_suspended_apple_events);
}

static UInt32
get_suspension_id (apple_event)
     Lisp_Object apple_event;
{
  Lisp_Object tem;

  CHECK_CONS (apple_event);
  CHECK_STRING_CAR (apple_event);
  if (SBYTES (XCAR (apple_event)) != 4
      || strcmp (SDATA (XCAR (apple_event)), "aevt") != 0)
    error ("Not an apple event");

  tem = assq_no_quit (Qemacs_suspension_id, XCDR (apple_event));
  if (NILP (tem))
    error ("Suspension ID not available");

  tem = XCDR (tem);
  if (!(CONSP (tem)
	&& STRINGP (XCAR (tem)) && SBYTES (XCAR (tem)) == 4
	&& strcmp (SDATA (XCAR (tem)), "magn") == 0
	&& STRINGP (XCDR (tem)) && SBYTES (XCDR (tem)) == 4))
    error ("Bad suspension ID format");

  return *((UInt32 *) SDATA (XCDR (tem)));
}


DEFUN ("mac-process-deferred-apple-events", Fmac_process_deferred_apple_events, Smac_process_deferred_apple_events, 0, 0, 0,
       doc: /* Process Apple events that are deferred at the startup time.  */)
  ()
{
  if (mac_ready_for_apple_events)
    return Qnil;

  BLOCK_INPUT;
  mac_ready_for_apple_events = 1;
  if (deferred_apple_events)
    {
      struct suspended_ae_info *prev, *tail, *next;

      /* `nreverse' deferred_apple_events.  */
      prev = NULL;
      for (tail = deferred_apple_events; tail; tail = next)
	{
	  next = tail->next;
	  tail->next = prev;
	  prev = tail;
	}

      /* Now `prev' points to the first cell.  */
      for (tail = prev; tail; tail = next)
	{
	  next = tail->next;
	  AEResumeTheCurrentEvent (&tail->apple_event, &tail->reply,
				   ((AEEventHandlerUPP)
				    kAEUseStandardDispatch), 0);
	  AEDisposeDesc (&tail->reply);
	  AEDisposeDesc (&tail->apple_event);
	  xfree (tail);
	}

      deferred_apple_events = NULL;
    }
  UNBLOCK_INPUT;

  return Qt;
}

DEFUN ("mac-cleanup-expired-apple-events", Fmac_cleanup_expired_apple_events, Smac_cleanup_expired_apple_events, 0, 0, 0,
       doc: /* Clean up expired Apple events.
Return the number of expired events.   */)
     ()
{
  int nexpired;

  BLOCK_INPUT;
  nexpired = cleanup_suspended_apple_events (&suspended_apple_events, 0);
  UNBLOCK_INPUT;

  return make_number (nexpired);
}

DEFUN ("mac-ae-set-reply-parameter", Fmac_ae_set_reply_parameter, Smac_ae_set_reply_parameter, 3, 3, 0,
       doc: /* Set parameter KEYWORD to DESCRIPTOR on reply of APPLE-EVENT.
KEYWORD is a 4-byte string.  DESCRIPTOR is a Lisp representation of an
Apple event descriptor.  It has the form of (TYPE . DATA), where TYPE
is a 4-byte string.  Valid format of DATA is as follows:

  * If TYPE is "null", then DATA is nil.
  * If TYPE is "list", then DATA is a list (DESCRIPTOR1 ... DESCRIPTORn).
  * If TYPE is "reco", then DATA is a list ((KEYWORD1 . DESCRIPTOR1)
    ... (KEYWORDn . DESCRIPTORn)).
  * If TYPE is "aevt", then DATA is ignored and the descriptor is
    treated as null.
  * Otherwise, DATA is a string.

If a (sub-)descriptor is in an invalid format, it is silently treated
as null.

Return t if the parameter is successfully set.  Otherwise return nil.  */)
     (apple_event, keyword, descriptor)
     Lisp_Object apple_event, keyword, descriptor;
{
  Lisp_Object result = Qnil;
  UInt32 suspension_id;
  struct suspended_ae_info *p;

  suspension_id = get_suspension_id (apple_event);

  CHECK_STRING (keyword);
  if (SBYTES (keyword) != 4)
    error ("Apple event keyword must be a 4-byte string: %s",
	   SDATA (keyword));

  BLOCK_INPUT;
  for (p = suspended_apple_events; p; p = p->next)
    if (p->suspension_id == suspension_id)
      break;
  if (p && p->reply.descriptorType != typeNull)
    {
      OSErr err;

      err = mac_ae_put_lisp (&p->reply,
			     EndianU32_BtoN (*((UInt32 *) SDATA (keyword))),
			     descriptor);
      if (err == noErr)
	result = Qt;
    }
  UNBLOCK_INPUT;

  return result;
}

DEFUN ("mac-resume-apple-event", Fmac_resume_apple_event, Smac_resume_apple_event, 1, 2, 0,
       doc: /* Resume handling of APPLE-EVENT.
Every Apple event handled by the Lisp interpreter is suspended first.
This function resumes such a suspended event either to complete Apple
event handling to give a reply, or to redispatch it to other handlers.

If optional ERROR-CODE is an integer, it specifies the error number
that is set in the reply.  If ERROR-CODE is t, the resumed event is
handled with the standard dispatching mechanism, but it is not handled
by Emacs again, thus it is redispatched to other handlers.

Return t if APPLE-EVENT is successfully resumed.  Otherwise return
nil, which means the event is already resumed or expired.  */)
     (apple_event, error_code)
     Lisp_Object apple_event, error_code;
{
  Lisp_Object result = Qnil;
  UInt32 suspension_id;
  struct suspended_ae_info **p, *ae;

  suspension_id = get_suspension_id (apple_event);

  BLOCK_INPUT;
  for (p = &suspended_apple_events; *p; p = &(*p)->next)
    if ((*p)->suspension_id == suspension_id)
      break;
  if (*p)
    {
      ae = *p;
      *p = (*p)->next;
      if (INTEGERP (error_code)
	  && ae->reply.descriptorType != typeNull)
	{
	  SInt32 errn = XINT (error_code);

	  AEPutParamPtr (&ae->reply, keyErrorNumber, typeSInt32,
			 &errn, sizeof (SInt32));
	}
      AESetTheCurrentEvent (&ae->apple_event);
      AEResumeTheCurrentEvent (&ae->apple_event, &ae->reply,
			       ((AEEventHandlerUPP)
				(EQ (error_code, Qt) ?
				 kAEUseStandardDispatch : kAENoDispatch)),
			       0);
      AEDisposeDesc (&ae->reply);
      AEDisposeDesc (&ae->apple_event);
      xfree (ae);
      result = Qt;
    }
  UNBLOCK_INPUT;

  return result;
}


/***********************************************************************
                      Drag and drop support
***********************************************************************/
#if TARGET_API_MAC_CARBON
static Lisp_Object Vmac_dnd_known_types;
static pascal OSErr mac_do_track_drag P_ ((DragTrackingMessage, WindowRef,
					   void *, DragRef));
static pascal OSErr mac_do_receive_drag P_ ((WindowRef, void *, DragRef));
static DragTrackingHandlerUPP mac_do_track_dragUPP = NULL;
static DragReceiveHandlerUPP mac_do_receive_dragUPP = NULL;

extern void mac_store_drag_event P_ ((WindowRef, Point, SInt16,
				      const AEDesc *));

static pascal OSErr
mac_do_track_drag (message, window, refcon, drag)
     DragTrackingMessage message;
     WindowRef window;
     void *refcon;
     DragRef drag;
{
  OSErr err = noErr;
  static int can_accept;
  UInt16 num_items, index;

  if (GetFrontWindowOfClass (kMovableModalWindowClass, false))
    return dragNotAcceptedErr;

  switch (message)
    {
    case kDragTrackingEnterHandler:
      err = CountDragItems (drag, &num_items);
      if (err != noErr)
	break;
      can_accept = 0;
      for (index = 1; index <= num_items; index++)
	{
	  ItemReference item;
	  FlavorFlags flags;
	  Lisp_Object rest;

	  err = GetDragItemReferenceNumber (drag, index, &item);
	  if (err != noErr)
	    continue;
	  for (rest = Vmac_dnd_known_types; CONSP (rest); rest = XCDR (rest))
	    {
	      Lisp_Object str;
	      FlavorType type;

	      str = XCAR (rest);
	      if (!(STRINGP (str) && SBYTES (str) == 4))
		continue;
	      type = EndianU32_BtoN (*((UInt32 *) SDATA (str)));

	      err = GetFlavorFlags (drag, item, type, &flags);
	      if (err == noErr)
		{
		  can_accept = 1;
		  break;
		}
	    }
	}
      break;

    case kDragTrackingEnterWindow:
      if (can_accept)
	{
	  RgnHandle hilite_rgn = NewRgn ();

	  if (hilite_rgn)
	    {
	      Rect r;

	      GetWindowPortBounds (window, &r);
	      OffsetRect (&r, -r.left, -r.top);
	      RectRgn (hilite_rgn, &r);
	      ShowDragHilite (drag, hilite_rgn, true);
	      DisposeRgn (hilite_rgn);
	    }
	  SetThemeCursor (kThemeCopyArrowCursor);
	}
      break;

    case kDragTrackingInWindow:
      break;

    case kDragTrackingLeaveWindow:
      if (can_accept)
	{
	  HideDragHilite (drag);
	  SetThemeCursor (kThemeArrowCursor);
	}
      break;

    case kDragTrackingLeaveHandler:
      break;
    }

  if (err != noErr)
    return dragNotAcceptedErr;
  return noErr;
}

static pascal OSErr
mac_do_receive_drag (window, refcon, drag)
     WindowRef window;
     void *refcon;
     DragRef drag;
{
  OSErr err;
  int num_types, i;
  Lisp_Object rest, str;
  FlavorType *types;
  AppleEvent apple_event;
  Point mouse_pos;
  SInt16 modifiers;

  if (GetFrontWindowOfClass (kMovableModalWindowClass, false))
    return dragNotAcceptedErr;

  num_types = 0;
  for (rest = Vmac_dnd_known_types; CONSP (rest); rest = XCDR (rest))
    {
      str = XCAR (rest);
      if (STRINGP (str) && SBYTES (str) == 4)
	num_types++;
    }

  types = xmalloc (sizeof (FlavorType) * num_types);
  i = 0;
  for (rest = Vmac_dnd_known_types; CONSP (rest); rest = XCDR (rest))
    {
      str = XCAR (rest);
      if (STRINGP (str) && SBYTES (str) == 4)
	types[i++] = EndianU32_BtoN (*((UInt32 *) SDATA (str)));
    }

  err = create_apple_event_from_drag_ref (drag, num_types, types,
					  &apple_event);
  xfree (types);

  if (err == noErr)
    err = GetDragMouse (drag, &mouse_pos, NULL);
  if (err == noErr)
    {
      GlobalToLocal (&mouse_pos);
      err = GetDragModifiers (drag, NULL, NULL, &modifiers);
    }
  if (err == noErr)
    {
      UInt32 key_modifiers = modifiers;

      err = AEPutParamPtr (&apple_event, kEventParamKeyModifiers,
			   typeUInt32, &key_modifiers, sizeof (UInt32));
    }

  if (err == noErr)
    {
      mac_store_drag_event (window, mouse_pos, 0, &apple_event);
      AEDisposeDesc (&apple_event);
      mac_wakeup_from_rne ();
      return noErr;
    }
  else
    return dragNotAcceptedErr;
}
#endif	/* TARGET_API_MAC_CARBON */

OSErr
install_drag_handler (window)
     WindowRef window;
{
  OSErr err = noErr;

#if TARGET_API_MAC_CARBON
  if (mac_do_track_dragUPP == NULL)
    mac_do_track_dragUPP = NewDragTrackingHandlerUPP (mac_do_track_drag);
  if (mac_do_receive_dragUPP == NULL)
    mac_do_receive_dragUPP = NewDragReceiveHandlerUPP (mac_do_receive_drag);

  err = InstallTrackingHandler (mac_do_track_dragUPP, window, NULL);
  if (err == noErr)
    err = InstallReceiveHandler (mac_do_receive_dragUPP, window, NULL);
#endif

  return err;
}

void
remove_drag_handler (window)
     WindowRef window;
{
#if TARGET_API_MAC_CARBON
  if (mac_do_track_dragUPP)
    RemoveTrackingHandler (mac_do_track_dragUPP, window);
  if (mac_do_receive_dragUPP)
    RemoveReceiveHandler (mac_do_receive_dragUPP, window);
#endif
}


/***********************************************************************
			Services menu support
***********************************************************************/
#ifdef MAC_OSX
OSStatus
install_service_handler ()
{
  static const EventTypeSpec specs[] =
    {{kEventClassService, kEventServiceGetTypes},
     {kEventClassService, kEventServiceCopy},
     {kEventClassService, kEventServicePaste},
     {kEventClassService, kEventServicePerform}};

  return InstallApplicationEventHandler (NewEventHandlerUPP
					 (mac_handle_service_event),
					 GetEventTypeCount (specs),
					 specs, NULL, NULL);
}

extern OSStatus mac_store_service_event P_ ((EventRef));

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
	buf = xrealloc (buf, size);
      else
	break;
    }
  if (err == noErr)
    {
      if (buf == NULL)
	err = memFullErr;
      else
	{
	  err = PutScrapFlavor (to_scrap, flavor_type, kScrapFlavorMaskNone,
				size, buf);
	  xfree (buf);
	}
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

  /* Check if Vmac_service_selection is a valid selection that has a
     corresponding scrap.  */
  if (!SYMBOLP (Vmac_service_selection))
    err = eventNotHandledErr;
  else
    err = mac_get_selection_from_symbol (Vmac_service_selection, 0, &cur_scrap);
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
		get_flavor_type_from_symbol (XCAR (XCAR (rest)), 0)))
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

      if (NILP (Fx_selection_owner_p (Vmac_service_selection)))
	break;
      else
	goto copy_all_flavors;

    case kEventServiceCopy:
      err = GetEventParameter (event, kEventParamScrapRef,
			       typeScrapRef, NULL,
			       sizeof (ScrapRef), NULL, &specific_scrap);
      if (err != noErr
	  || NILP (Fx_selection_owner_p (Vmac_service_selection)))
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
	err = GetScrapFlavorInfoList (cur_scrap, &count, flavor_info);
	if (err != noErr)
	  {
	    xfree (flavor_info);
	    flavor_info = NULL;
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
	  err = mac_clear_selection (&cur_scrap);
	if (err == noErr)
	  for (rest = Vselection_converter_alist; CONSP (rest);
	       rest = XCDR (rest))
	    {
	      if (! (CONSP (XCAR (rest)) && SYMBOLP (XCAR (XCAR (rest)))))
		continue;
	      flavor_type = get_flavor_type_from_symbol (XCAR (XCAR (rest)),
							 specific_scrap);
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
	  err = mac_store_service_event (event);
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
  defsubr (&Smac_cleanup_expired_apple_events);
  defsubr (&Smac_resume_apple_event);
  defsubr (&Smac_ae_set_reply_parameter);

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
means that the conversion could not be done.  */);
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

#if TARGET_API_MAC_CARBON
  DEFVAR_LISP ("mac-dnd-known-types", &Vmac_dnd_known_types,
	       doc: /* The types accepted by default for dropped data.
The types are chosen in the order they appear in the list.  */);
  Vmac_dnd_known_types = list4 (build_string ("hfs "), build_string ("utxt"),
				build_string ("TEXT"), build_string ("TIFF"));
#ifdef MAC_OSX
  Vmac_dnd_known_types = Fcons (build_string ("furl"), Vmac_dnd_known_types);
#endif
#endif

#ifdef MAC_OSX
  DEFVAR_LISP ("mac-service-selection", &Vmac_service_selection,
	       doc: /* Selection name for communication via Services menu.  */);
  Vmac_service_selection = intern ("PRIMARY");
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

  Qemacs_suspension_id = intern ("emacs-suspension-id");
  staticpro (&Qemacs_suspension_id);
}

/* arch-tag: f3c91ad8-99e0-4bd6-9eef-251b2f848732
   (do not change this comment) */
