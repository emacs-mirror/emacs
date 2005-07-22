/* Unix emulation routines for GNU Emacs on the Mac OS.
   Copyright (C) 2000, 2001, 2005  Free Software Foundation, Inc.

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

/* Contributed by Andrew Choi (akochoi@mac.com).  */

#include <config.h>

#include <stdio.h>
#include <errno.h>

#include "lisp.h"
#include "process.h"
#undef init_process
#include "systime.h"
#include "sysselect.h"
#include "blockinput.h"

#include "macterm.h"

#include "charset.h"
#include "coding.h"
#if !TARGET_API_MAC_CARBON
#include <Files.h>
#include <MacTypes.h>
#include <TextUtils.h>
#include <Folders.h>
#include <Resources.h>
#include <Aliases.h>
#include <FixMath.h>
#include <Timer.h>
#include <OSA.h>
#include <AppleScript.h>
#include <Scrap.h>
#include <Events.h>
#include <Processes.h>
#include <EPPC.h>
#include <MacLocales.h>
#include <Endian.h>
#endif	/* not TARGET_API_MAC_CARBON */

#include <utime.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <pwd.h>
#include <grp.h>
#include <sys/param.h>
#include <stdlib.h>
#include <fcntl.h>
#if __MWERKS__
#include <unistd.h>
#endif

/* The system script code. */
static int mac_system_script_code;

/* The system locale identifier string.  */
static Lisp_Object Vmac_system_locale;

/* An instance of the AppleScript component.  */
static ComponentInstance as_scripting_component;
/* The single script context used for all script executions.  */
static OSAID as_script_context;


/* When converting from Mac to Unix pathnames, /'s in folder names are
   converted to :'s.  This function, used in copying folder names,
   performs a strncat and converts all character a to b in the copy of
   the string s2 appended to the end of s1.  */

void
string_cat_and_replace (char *s1, const char *s2, int n, char a, char b)
{
  int l1 = strlen (s1);
  int l2 = strlen (s2);
  char *p = s1 + l1;
  int i;

  strncat (s1, s2, n);
  for (i = 0; i < l2; i++)
    {
      if (*p == a)
        *p = b;
      p++;
    }
}


/* Convert a Mac pathname to Posix form.  A Mac full pathname is one
   that does not begin with a ':' and contains at least one ':'. A Mac
   full pathname causes a '/' to be prepended to the Posix pathname.
   The algorithm for the rest of the pathname is as follows:
     For each segment between two ':',
       if it is non-null, copy as is and then add a '/' at the end,
       otherwise, insert a "../" into the Posix pathname.
   Returns 1 if successful; 0 if fails.  */

int
mac_to_posix_pathname (const char *mfn, char *ufn, int ufnbuflen)
{
  const char *p, *q, *pe;

  strcpy (ufn, "");

  if (*mfn == '\0')
    return 1;

  p = strchr (mfn, ':');
  if (p != 0 && p != mfn)  /* full pathname */
    strcat (ufn, "/");

  p = mfn;
  if (*p == ':')
    p++;

  pe = mfn + strlen (mfn);
  while (p < pe)
    {
      q = strchr (p, ':');
      if (q)
	{
	  if (q == p)
	    {  /* two consecutive ':' */
	      if (strlen (ufn) + 3 >= ufnbuflen)
		return 0;
	      strcat (ufn, "../");
	    }
	  else
	    {
	      if (strlen (ufn) + (q - p) + 1 >= ufnbuflen)
		return 0;
	      string_cat_and_replace (ufn, p, q - p, '/', ':');
	      strcat (ufn, "/");
	    }
	  p = q + 1;
	}
      else
	{
	  if (strlen (ufn) + (pe - p) >= ufnbuflen)
	    return 0;
	  string_cat_and_replace (ufn, p, pe - p, '/', ':');
	    /* no separator for last one */
	  p = pe;
	}
    }

  return 1;
}


extern char *get_temp_dir_name ();


/* Convert a Posix pathname to Mac form.  Approximately reverse of the
   above in algorithm.  */

int
posix_to_mac_pathname (const char *ufn, char *mfn, int mfnbuflen)
{
  const char *p, *q, *pe;
  char expanded_pathname[MAXPATHLEN+1];

  strcpy (mfn, "");

  if (*ufn == '\0')
    return 1;

  p = ufn;

  /* Check for and handle volume names.  Last comparison: strangely
     somewhere "/.emacs" is passed.  A temporary fix for now.  */
  if (*p == '/' && strchr (p+1, '/') == NULL && strcmp (p, "/.emacs") != 0)
    {
      if (strlen (p) + 1 > mfnbuflen)
	return 0;
      strcpy (mfn, p+1);
      strcat (mfn, ":");
      return 1;
    }

  /* expand to emacs dir found by init_emacs_passwd_dir */
  if (strncmp (p, "~emacs/", 7) == 0)
    {
      struct passwd *pw = getpwnam ("emacs");
      p += 7;
      if (strlen (pw->pw_dir) + strlen (p) > MAXPATHLEN)
	return 0;
      strcpy (expanded_pathname, pw->pw_dir);
      strcat (expanded_pathname, p);
      p = expanded_pathname;
        /* now p points to the pathname with emacs dir prefix */
    }
  else if (strncmp (p, "/tmp/", 5) == 0)
    {
      char *t = get_temp_dir_name ();
      p += 5;
      if (strlen (t) + strlen (p) > MAXPATHLEN)
	return 0;
      strcpy (expanded_pathname, t);
      strcat (expanded_pathname, p);
      p = expanded_pathname;
        /* now p points to the pathname with emacs dir prefix */
    }
  else if (*p != '/')  /* relative pathname */
    strcat (mfn, ":");

  if (*p == '/')
    p++;

  pe = p + strlen (p);
  while (p < pe)
    {
      q = strchr (p, '/');
      if (q)
	{
	  if (q - p == 2 && *p == '.' && *(p+1) == '.')
	    {
	      if (strlen (mfn) + 1 >= mfnbuflen)
		return 0;
	      strcat (mfn, ":");
	    }
	  else
	    {
	      if (strlen (mfn) + (q - p) + 1 >= mfnbuflen)
		return 0;
	      string_cat_and_replace (mfn, p, q - p, ':', '/');
	      strcat (mfn, ":");
	    }
	  p = q + 1;
	}
      else
	{
	  if (strlen (mfn) + (pe - p) >= mfnbuflen)
	    return 0;
	  string_cat_and_replace (mfn, p, pe - p, ':', '/');
	  p = pe;
	}
    }

  return 1;
}


/***********************************************************************
	 Conversion between Lisp and Core Foundation objects
 ***********************************************************************/

#if TARGET_API_MAC_CARBON
static Lisp_Object Qstring, Qnumber, Qboolean, Qdate, Qdata;
static Lisp_Object Qarray, Qdictionary;
#define DECODE_UTF_8(str) code_convert_string_norecord (str, Qutf_8, 0)

struct cfdict_context
{
  Lisp_Object *result;
  int with_tag, hash_bound;
};

/* C string to CFString.  */

CFStringRef
cfstring_create_with_utf8_cstring (c_str)
     const char *c_str;
{
  CFStringRef str;

  str = CFStringCreateWithCString (NULL, c_str, kCFStringEncodingUTF8);
  if (str == NULL)
    /* Failed to interpret as UTF 8.  Fall back on Mac Roman.  */
    str = CFStringCreateWithCString (NULL, c_str, kCFStringEncodingMacRoman);

  return str;
}


/* Lisp string to CFString.  */

CFStringRef
cfstring_create_with_string (s)
     Lisp_Object s;
{
  CFStringRef string = NULL;

  if (STRING_MULTIBYTE (s))
    {
      char *p, *end = SDATA (s) + SBYTES (s);

      for (p = SDATA (s); p < end; p++)
	if (!isascii (*p))
	  {
	    s = ENCODE_UTF_8 (s);
	    break;
	  }
      string = CFStringCreateWithBytes (NULL, SDATA (s), SBYTES (s),
					kCFStringEncodingUTF8, false);
    }

  if (string == NULL)
    /* Failed to interpret as UTF 8.  Fall back on Mac Roman.  */
    string = CFStringCreateWithBytes (NULL, SDATA (s), SBYTES (s),
				      kCFStringEncodingMacRoman, false);

  return string;
}


/* From CFData to a lisp string.  Always returns a unibyte string.  */

Lisp_Object
cfdata_to_lisp (data)
     CFDataRef data;
{
  CFIndex len = CFDataGetLength (data);
  Lisp_Object result = make_uninit_string (len);

  CFDataGetBytes (data, CFRangeMake (0, len), SDATA (result));

  return result;
}


/* From CFString to a lisp string.  Never returns a unibyte string
   (even if it only contains ASCII characters).
   This may cause GC during code conversion. */

Lisp_Object
cfstring_to_lisp (string)
     CFStringRef string;
{
  Lisp_Object result = Qnil;
  const char *s = CFStringGetCStringPtr (string, kCFStringEncodingUTF8);

  if (s)
    result = make_unibyte_string (s, strlen (s));
  else
    {
      CFDataRef data =
	CFStringCreateExternalRepresentation (NULL, string,
					      kCFStringEncodingUTF8, '?');

      if (data)
	{
	  result = cfdata_to_lisp (data);
	  CFRelease (data);
	}
    }

  if (!NILP (result))
    {
      result = DECODE_UTF_8 (result);
      /* This may be superfluous.  Just to make sure that the result
	 is a multibyte string.  */
      result = string_to_multibyte (result);
    }

  return result;
}


/* CFNumber to a lisp integer or a lisp float.  */

Lisp_Object
cfnumber_to_lisp (number)
     CFNumberRef number;
{
  Lisp_Object result = Qnil;
#if BITS_PER_EMACS_INT > 32
  SInt64 int_val;
  CFNumberType emacs_int_type = kCFNumberSInt64Type;
#else
  SInt32 int_val;
  CFNumberType emacs_int_type = kCFNumberSInt32Type;
#endif
  double float_val;

  if (CFNumberGetValue (number, emacs_int_type, &int_val)
      && !FIXNUM_OVERFLOW_P (int_val))
    result = make_number (int_val);
  else
    if (CFNumberGetValue (number, kCFNumberDoubleType, &float_val))
      result = make_float (float_val);
  return result;
}


/* CFDate to a list of three integers as in a return value of
   `current-time'.  */

Lisp_Object
cfdate_to_lisp (date)
     CFDateRef date;
{
  static const CFGregorianDate epoch_gdate = {1970, 1, 1, 0, 0, 0.0};
  static CFAbsoluteTime epoch = 0.0, sec;
  int high, low;

  if (epoch == 0.0)
    epoch = CFGregorianDateGetAbsoluteTime (epoch_gdate, NULL);

  sec = CFDateGetAbsoluteTime (date) - epoch;
  high = sec / 65536.0;
  low = sec - high * 65536.0;

  return list3 (make_number (high), make_number (low), make_number (0));
}


/* CFBoolean to a lisp symbol, `t' or `nil'.  */

Lisp_Object
cfboolean_to_lisp (boolean)
     CFBooleanRef boolean;
{
  return CFBooleanGetValue (boolean) ? Qt : Qnil;
}


/* Any Core Foundation object to a (lengthy) lisp string.  */

Lisp_Object
cfobject_desc_to_lisp (object)
     CFTypeRef object;
{
  Lisp_Object result = Qnil;
  CFStringRef desc = CFCopyDescription (object);

  if (desc)
    {
      result = cfstring_to_lisp (desc);
      CFRelease (desc);
    }

  return result;
}


/* Callback functions for cfproperty_list_to_lisp.  */

static void
cfdictionary_add_to_list (key, value, context)
     const void *key;
     const void *value;
     void *context;
{
  struct cfdict_context *cxt = (struct cfdict_context *)context;

  *cxt->result =
    Fcons (Fcons (cfstring_to_lisp (key),
		  cfproperty_list_to_lisp (value, cxt->with_tag,
					   cxt->hash_bound)),
	   *cxt->result);
}

static void
cfdictionary_puthash (key, value, context)
     const void *key;
     const void *value;
     void *context;
{
  Lisp_Object lisp_key = cfstring_to_lisp (key);
  struct cfdict_context *cxt = (struct cfdict_context *)context;
  struct Lisp_Hash_Table *h = XHASH_TABLE (*(cxt->result));
  unsigned hash_code;

  hash_lookup (h, lisp_key, &hash_code);
  hash_put (h, lisp_key,
	    cfproperty_list_to_lisp (value, cxt->with_tag, cxt->hash_bound),
	    hash_code);
}


/* Convert CFPropertyList PLIST to a lisp object.  If WITH_TAG is
   non-zero, a symbol that represents the type of the original Core
   Foundation object is prepended.  HASH_BOUND specifies which kinds
   of the lisp objects, alists or hash tables, are used as the targets
   of the conversion from CFDictionary.  If HASH_BOUND is negative,
   always generate alists.  If HASH_BOUND >= 0, generate an alist if
   the number of keys in the dictionary is smaller than HASH_BOUND,
   and a hash table otherwise.  */

Lisp_Object
cfproperty_list_to_lisp (plist, with_tag, hash_bound)
     CFPropertyListRef plist;
     int with_tag, hash_bound;
{
  CFTypeID type_id = CFGetTypeID (plist);
  Lisp_Object tag = Qnil, result = Qnil;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (tag, result);

  if (type_id == CFStringGetTypeID ())
    {
      tag = Qstring;
      result = cfstring_to_lisp (plist);
    }
  else if (type_id == CFNumberGetTypeID ())
    {
      tag = Qnumber;
      result = cfnumber_to_lisp (plist);
    }
  else if (type_id == CFBooleanGetTypeID ())
    {
      tag = Qboolean;
      result = cfboolean_to_lisp (plist);
    }
  else if (type_id == CFDateGetTypeID ())
    {
      tag = Qdate;
      result = cfdate_to_lisp (plist);
    }
  else if (type_id == CFDataGetTypeID ())
    {
      tag = Qdata;
      result = cfdata_to_lisp (plist);
    }
  else if (type_id == CFArrayGetTypeID ())
    {
      CFIndex index, count = CFArrayGetCount (plist);

      tag = Qarray;
      result = Fmake_vector (make_number (count), Qnil);
      for (index = 0; index < count; index++)
	XVECTOR (result)->contents[index] =
	  cfproperty_list_to_lisp (CFArrayGetValueAtIndex (plist, index),
				   with_tag, hash_bound);
    }
  else if (type_id == CFDictionaryGetTypeID ())
    {
      struct cfdict_context context;
      CFIndex count = CFDictionaryGetCount (plist);

      tag = Qdictionary;
      context.result  = &result;
      context.with_tag = with_tag;
      context.hash_bound = hash_bound;
      if (hash_bound < 0 || count < hash_bound)
	{
	  result = Qnil;
	  CFDictionaryApplyFunction (plist, cfdictionary_add_to_list,
				     &context);
	}
      else
	{
	  result = make_hash_table (Qequal,
				    make_number (count),
				    make_float (DEFAULT_REHASH_SIZE),
				    make_float (DEFAULT_REHASH_THRESHOLD),
				    Qnil, Qnil, Qnil);
	  CFDictionaryApplyFunction (plist, cfdictionary_puthash,
				     &context);
	}
    }
  else
    abort ();

  UNGCPRO;

  if (with_tag)
    result = Fcons (tag, result);

  return result;
}
#endif


/***********************************************************************
		 Emulation of the X Resource Manager
 ***********************************************************************/

/* Parser functions for resource lines.  Each function takes an
   address of a variable whose value points to the head of a string.
   The value will be advanced so that it points to the next character
   of the parsed part when the function returns.

   A resource name such as "Emacs*font" is parsed into a non-empty
   list called `quarks'.  Each element is either a Lisp string that
   represents a concrete component, a Lisp symbol LOOSE_BINDING
   (actually Qlambda) that represents any number (>=0) of intervening
   components, or a Lisp symbol SINGLE_COMPONENT (actually Qquote)
   that represents as any single component.  */

#define P (*p)

#define LOOSE_BINDING    Qlambda /* '*' ("L"oose) */
#define SINGLE_COMPONENT Qquote	 /* '?' ("Q"uestion) */

static void
skip_white_space (p)
     char **p;
{
  /* WhiteSpace = {<space> | <horizontal tab>} */
  while (*P == ' ' || *P == '\t')
    P++;
}

static int
parse_comment (p)
     char **p;
{
  /* Comment = "!" {<any character except null or newline>} */
  if (*P == '!')
    {
      P++;
      while (*P)
	if (*P++ == '\n')
	  break;
      return 1;
    }
  else
    return 0;
}

/* Don't interpret filename.  Just skip until the newline.  */
static int
parse_include_file (p)
     char **p;
{
  /* IncludeFile = "#" WhiteSpace "include" WhiteSpace FileName WhiteSpace */
  if (*P == '#')
    {
      P++;
      while (*P)
	if (*P++ == '\n')
	  break;
      return 1;
    }
  else
    return 0;
}

static char
parse_binding (p)
     char **p;
{
  /* Binding = "." | "*"  */
  if (*P == '.' || *P == '*')
    {
      char binding = *P++;

      while (*P == '.' || *P == '*')
	if (*P++ == '*')
	  binding = '*';
      return binding;
    }
  else
    return '\0';
}

static Lisp_Object
parse_component (p)
     char **p;
{
  /*  Component = "?" | ComponentName
      ComponentName = NameChar {NameChar}
      NameChar = "a"-"z" | "A"-"Z" | "0"-"9" | "_" | "-" */
  if (*P == '?')
    {
      P++;
      return SINGLE_COMPONENT;
    }
  else if (isalnum (*P) || *P == '_' || *P == '-')
    {
      char *start = P++;

      while (isalnum (*P) || *P == '_' || *P == '-')
	P++;

      return make_unibyte_string (start, P - start);
    }
  else
    return Qnil;
}

static Lisp_Object
parse_resource_name (p)
     char **p;
{
  Lisp_Object result = Qnil, component;
  char binding;

  /* ResourceName = [Binding] {Component Binding} ComponentName */
  if (parse_binding (p) == '*')
    result = Fcons (LOOSE_BINDING, result);

  component = parse_component (p);
  if (NILP (component))
    return Qnil;

  result = Fcons (component, result);
  while ((binding = parse_binding (p)) != '\0')
    {
      if (binding == '*')
	result = Fcons (LOOSE_BINDING, result);
      component = parse_component (p);
      if (NILP (component))
	return Qnil;
      else
	result = Fcons (component, result);
    }

  /* The final component should not be '?'.  */
  if (EQ (component, SINGLE_COMPONENT))
    return Qnil;

  return Fnreverse (result);
}

static Lisp_Object
parse_value (p)
     char **p;
{
  char *q, *buf;
  Lisp_Object seq = Qnil, result;
  int buf_len, total_len = 0, len, continue_p;

  q = strchr (P, '\n');
  buf_len = q ? q - P : strlen (P);
  buf = xmalloc (buf_len);

  while (1)
    {
      q = buf;
      continue_p = 0;
      while (*P)
	{
	  if (*P == '\n')
	    {
	      P++;
	      break;
	    }
	  else if (*P == '\\')
	    {
	      P++;
	      if (*P == '\0')
		break;
	      else if (*P == '\n')
		{
		  P++;
		  continue_p = 1;
		  break;
		}
	      else if (*P == 'n')
		{
		  *q++ = '\n';
		  P++;
		}
	      else if ('0' <= P[0] && P[0] <= '7'
		       && '0' <= P[1] && P[1] <= '7'
		       && '0' <= P[2] && P[2] <= '7')
		{
		  *q++ = (P[0] - '0' << 6) + (P[1] - '0' << 3) + (P[2] - '0');
		  P += 3;
		}
	      else
		*q++ = *P++;
	    }
	  else
	    *q++ = *P++;
	}
      len = q - buf;
      seq = Fcons (make_unibyte_string (buf, len), seq);
      total_len += len;

      if (continue_p)
	{
	  q = strchr (P, '\n');
	  len = q ? q - P : strlen (P);
	  if (len > buf_len)
	    {
	      xfree (buf);
	      buf_len = len;
	      buf = xmalloc (buf_len);
	    }
	}
      else
	break;
    }
  xfree (buf);

  if (SBYTES (XCAR (seq)) == total_len)
    return make_string (SDATA (XCAR (seq)), total_len);
  else
    {
      buf = xmalloc (total_len);
      q = buf + total_len;
      for (; CONSP (seq); seq = XCDR (seq))
	{
	  len = SBYTES (XCAR (seq));
	  q -= len;
	  memcpy (q, SDATA (XCAR (seq)), len);
	}
      result = make_string (buf, total_len);
      xfree (buf);
      return result;
    }
}

static Lisp_Object
parse_resource_line (p)
     char **p;
{
  Lisp_Object quarks, value;

  /* ResourceLine = Comment | IncludeFile | ResourceSpec | <empty line> */
  if (parse_comment (p) || parse_include_file (p))
    return Qnil;

  /* ResourceSpec = WhiteSpace ResourceName WhiteSpace ":" WhiteSpace Value */
  skip_white_space (p);
  quarks = parse_resource_name (p);
  if (NILP (quarks))
    goto cleanup;
  skip_white_space (p);
  if (*P != ':')
    goto cleanup;
  P++;
  skip_white_space (p);
  value = parse_value (p);
  return Fcons (quarks, value);

 cleanup:
  /* Skip the remaining data as a dummy value.  */
  parse_value (p);
  return Qnil;
}

#undef P

/* Equivalents of X Resource Manager functions.

   An X Resource Database acts as a collection of resource names and
   associated values.  It is implemented as a trie on quarks.  Namely,
   each edge is labeled by either a string, LOOSE_BINDING, or
   SINGLE_COMPONENT.  Each node has a node id, which is a unique
   nonnegative integer, and the root node id is 0.  A database is
   implemented as a hash table that maps a pair (SRC-NODE-ID .
   EDGE-LABEL) to DEST-NODE-ID.  It also holds a maximum node id used
   in the table as a value for HASHKEY_MAX_NID.  A value associated to
   a node is recorded as a value for the node id.  */

#define HASHKEY_MAX_NID (make_number (0))

static XrmDatabase
xrm_create_database ()
{
  XrmDatabase database;

  database = make_hash_table (Qequal, make_number (DEFAULT_HASH_SIZE),
			      make_float (DEFAULT_REHASH_SIZE),
			      make_float (DEFAULT_REHASH_THRESHOLD),
			      Qnil, Qnil, Qnil);
  Fputhash (HASHKEY_MAX_NID, make_number (0), database);

  return database;
}

static void
xrm_q_put_resource (database, quarks, value)
     XrmDatabase database;
     Lisp_Object quarks, value;
{
  struct Lisp_Hash_Table *h = XHASH_TABLE (database);
  unsigned hash_code;
  int max_nid, i;
  Lisp_Object node_id, key;

  max_nid = XINT (Fgethash (HASHKEY_MAX_NID, database, Qnil));

  XSETINT (node_id, 0);
  for (; CONSP (quarks); quarks = XCDR (quarks))
    {
      key = Fcons (node_id, XCAR (quarks));
      i = hash_lookup (h, key, &hash_code);
      if (i < 0)
	{
	  max_nid++;
	  XSETINT (node_id, max_nid);
	  hash_put (h, key, node_id, hash_code);
	}
      else
	node_id = HASH_VALUE (h, i);
    }
  Fputhash (node_id, value, database);

  Fputhash (HASHKEY_MAX_NID, make_number (max_nid), database);
}

/* Merge multiple resource entries specified by DATA into a resource
   database DATABASE.  DATA points to the head of a null-terminated
   string consisting of multiple resource lines.  It's like a
   combination of XrmGetStringDatabase and XrmMergeDatabases.  */

void
xrm_merge_string_database (database, data)
     XrmDatabase database;
     char *data;
{
  Lisp_Object quarks_value;

  while (*data)
    {
      quarks_value = parse_resource_line (&data);
      if (!NILP (quarks_value))
	xrm_q_put_resource (database,
			    XCAR (quarks_value), XCDR (quarks_value));
    }
}

static Lisp_Object
xrm_q_get_resource_1 (database, node_id, quark_name, quark_class)
     XrmDatabase database;
     Lisp_Object node_id, quark_name, quark_class;
{
  struct Lisp_Hash_Table *h = XHASH_TABLE (database);
  Lisp_Object key, labels[3], value;
  int i, k;

  if (!CONSP (quark_name))
    return Fgethash (node_id, database, Qnil);

  /* First, try tight bindings */
  labels[0] = XCAR (quark_name);
  labels[1] = XCAR (quark_class);
  labels[2] = SINGLE_COMPONENT;

  key = Fcons (node_id, Qnil);
  for (k = 0; k < sizeof (labels) / sizeof (*labels); k++)
    {
      XSETCDR (key, labels[k]);
      i = hash_lookup (h, key, NULL);
      if (i >= 0)
	{
	  value = xrm_q_get_resource_1 (database, HASH_VALUE (h, i),
					XCDR (quark_name), XCDR (quark_class));
	  if (!NILP (value))
	    return value;
	}
    }

  /* Then, try loose bindings */
  XSETCDR (key, LOOSE_BINDING);
  i = hash_lookup (h, key, NULL);
  if (i >= 0)
    {
      value = xrm_q_get_resource_1 (database, HASH_VALUE (h, i),
				    quark_name, quark_class);
      if (!NILP (value))
	return value;
      else
	return xrm_q_get_resource_1 (database, node_id,
				     XCDR (quark_name), XCDR (quark_class));
    }
  else
    return Qnil;
}

static Lisp_Object
xrm_q_get_resource (database, quark_name, quark_class)
     XrmDatabase database;
     Lisp_Object quark_name, quark_class;
{
  return xrm_q_get_resource_1 (database, make_number (0),
			       quark_name, quark_class);
}

/* Retrieve a resource value for the specified NAME and CLASS from the
   resource database DATABASE.  It corresponds to XrmGetResource.  */

Lisp_Object
xrm_get_resource (database, name, class)
     XrmDatabase database;
     char *name, *class;
{
  Lisp_Object quark_name, quark_class, tmp;
  int nn, nc;

  quark_name = parse_resource_name (&name);
  if (*name != '\0')
    return Qnil;
  for (tmp = quark_name, nn = 0; CONSP (tmp); tmp = XCDR (tmp), nn++)
    if (!STRINGP (XCAR (tmp)))
      return Qnil;

  quark_class = parse_resource_name (&class);
  if (*class != '\0')
    return Qnil;
  for (tmp = quark_class, nc = 0; CONSP (tmp); tmp = XCDR (tmp), nc++)
    if (!STRINGP (XCAR (tmp)))
      return Qnil;

  if (nn != nc)
    return Qnil;
  else
    return xrm_q_get_resource (database, quark_name, quark_class);
}

#if TARGET_API_MAC_CARBON
static Lisp_Object
xrm_cfproperty_list_to_value (plist)
     CFPropertyListRef plist;
{
  CFTypeID type_id = CFGetTypeID (plist);

  if (type_id == CFStringGetTypeID ())
    return cfstring_to_lisp (plist);
  else if (type_id == CFNumberGetTypeID ())
    {
      CFStringRef string;
      Lisp_Object result = Qnil;

      string = CFStringCreateWithFormat (NULL, NULL, CFSTR ("%@"), plist);
      if (string)
	{
	  result = cfstring_to_lisp (string);
	  CFRelease (string);
	}
      return result;
    }
  else if (type_id == CFBooleanGetTypeID ())
    return build_string (CFBooleanGetValue (plist) ? "true" : "false");
  else if (type_id == CFDataGetTypeID ())
    return cfdata_to_lisp (plist);
  else
    return Qnil;
}
#endif

/* Create a new resource database from the preferences for the
   application APPLICATION.  APPLICATION is either a string that
   specifies an application ID, or NULL that represents the current
   application.  */

XrmDatabase
xrm_get_preference_database (application)
     char *application;
{
#if TARGET_API_MAC_CARBON
  CFStringRef app_id, *keys, user_doms[2], host_doms[2];
  CFMutableSetRef key_set = NULL;
  CFArrayRef key_array;
  CFIndex index, count;
  char *res_name;
  XrmDatabase database;
  Lisp_Object quarks = Qnil, value = Qnil;
  CFPropertyListRef plist;
  int iu, ih;
  struct gcpro gcpro1, gcpro2, gcpro3;

  user_doms[0] = kCFPreferencesCurrentUser;
  user_doms[1] = kCFPreferencesAnyUser;
  host_doms[0] = kCFPreferencesCurrentHost;
  host_doms[1] = kCFPreferencesAnyHost;

  database = xrm_create_database ();

  GCPRO3 (database, quarks, value);

  BLOCK_INPUT;

  app_id = kCFPreferencesCurrentApplication;
  if (application)
    {
      app_id = cfstring_create_with_utf8_cstring (application);
      if (app_id == NULL)
	goto out;
    }

  key_set = CFSetCreateMutable (NULL, 0, &kCFCopyStringSetCallBacks);
  if (key_set == NULL)
    goto out;
  for (iu = 0; iu < sizeof (user_doms) / sizeof (*user_doms) ; iu++)
    for (ih = 0; ih < sizeof (host_doms) / sizeof (*host_doms); ih++)
      {
	key_array = CFPreferencesCopyKeyList (app_id, user_doms[iu],
					      host_doms[ih]);
	if (key_array)
	  {
	    count = CFArrayGetCount (key_array);
	    for (index = 0; index < count; index++)
	      CFSetAddValue (key_set,
			     CFArrayGetValueAtIndex (key_array, index));
	    CFRelease (key_array);
	  }
      }

  count = CFSetGetCount (key_set);
  keys = xmalloc (sizeof (CFStringRef) * count);
  if (keys == NULL)
    goto out;
  CFSetGetValues (key_set, (const void **)keys);
  for (index = 0; index < count; index++)
    {
      res_name = SDATA (cfstring_to_lisp (keys[index]));
      quarks = parse_resource_name (&res_name);
      if (!(NILP (quarks) || *res_name))
	{
	  plist = CFPreferencesCopyAppValue (keys[index], app_id);
	  value = xrm_cfproperty_list_to_value (plist);
	  CFRelease (plist);
	  if (!NILP (value))
	    xrm_q_put_resource (database, quarks, value);
	}
    }

  xfree (keys);
 out:
  if (key_set)
    CFRelease (key_set);
  CFRelease (app_id);

  UNBLOCK_INPUT;

  UNGCPRO;

  return database;
#else
  return xrm_create_database ();
#endif
}


#ifndef MAC_OSX

/* The following functions with "sys_" prefix are stubs to Unix
   functions that have already been implemented by CW or MPW.  The
   calls to them in Emacs source course are #define'd to call the sys_
   versions by the header files s-mac.h.  In these stubs pathnames are
   converted between their Unix and Mac forms.  */


/* Unix epoch is Jan 1, 1970 while Mac epoch is Jan 1, 1904: 66 years
   + 17 leap days.  These are for adjusting time values returned by
   MacOS Toolbox functions.  */

#define MAC_UNIX_EPOCH_DIFF  ((365L * 66 + 17) * 24 * 60 * 60)

#ifdef __MWERKS__
#if __MSL__ < 0x6000
/* CW Pro 5 epoch is Jan 1, 1900 (aaarghhhhh!); remember, 1900 is not
   a leap year!  This is for adjusting time_t values returned by MSL
   functions.  */
#define CW_OR_MPW_UNIX_EPOCH_DIFF ((365L * 70 + 17) * 24 * 60 * 60)
#else /* __MSL__ >= 0x6000 */
/* CW changes Pro 6 to follow Unix!  */
#define CW_OR_MPW_UNIX_EPOCH_DIFF ((365L * 66 + 17) * 24 * 60 * 60)
#endif /* __MSL__ >= 0x6000 */
#elif __MRC__
/* MPW library functions follow Unix (confused?).  */
#define CW_OR_MPW_UNIX_EPOCH_DIFF ((365L * 66 + 17) * 24 * 60 * 60)
#else /* not __MRC__ */
You lose!!!
#endif /* not __MRC__ */


/* Define our own stat function for both MrC and CW.  The reason for
   doing this: "stat" is both the name of a struct and function name:
   can't use the same trick like that for sys_open, sys_close, etc. to
   redirect Emacs's calls to our own version that converts Unix style
   filenames to Mac style filename because all sorts of compilation
   errors will be generated if stat is #define'd to be sys_stat.  */

int
stat_noalias (const char *path, struct stat *buf)
{
  char mac_pathname[MAXPATHLEN+1];
  CInfoPBRec cipb;

  if (posix_to_mac_pathname (path, mac_pathname, MAXPATHLEN+1) == 0)
    return -1;

  c2pstr (mac_pathname);
  cipb.hFileInfo.ioNamePtr = mac_pathname;
  cipb.hFileInfo.ioVRefNum = 0;
  cipb.hFileInfo.ioDirID = 0;
  cipb.hFileInfo.ioFDirIndex = 0;
    /* set to 0 to get information about specific dir or file */

  errno = PBGetCatInfo (&cipb, false);
  if (errno == -43) /* -43: fnfErr defined in Errors.h */
    errno = ENOENT;
  if (errno != noErr)
    return -1;

  if (cipb.hFileInfo.ioFlAttrib & 0x10)  /* bit 4 = 1 for directories */
    {
      buf->st_mode = S_IFDIR | S_IREAD | S_IEXEC;

      if (!(cipb.hFileInfo.ioFlAttrib & 0x1))
	buf->st_mode |= S_IWRITE;  /* bit 1 = 1 for locked files/directories */
      buf->st_ino = cipb.dirInfo.ioDrDirID;
      buf->st_dev = cipb.dirInfo.ioVRefNum;
      buf->st_size = cipb.dirInfo.ioDrNmFls;
        /* size of dir = number of files and dirs */
      buf->st_atime
	= buf->st_mtime
	= cipb.dirInfo.ioDrMdDat - MAC_UNIX_EPOCH_DIFF;
      buf->st_ctime = cipb.dirInfo.ioDrCrDat - MAC_UNIX_EPOCH_DIFF;
    }
  else
    {
      buf->st_mode = S_IFREG | S_IREAD;
      if (!(cipb.hFileInfo.ioFlAttrib & 0x1))
	buf->st_mode |= S_IWRITE;  /* bit 1 = 1 for locked files/directories */
      if (cipb.hFileInfo.ioFlFndrInfo.fdType == 'APPL')
	buf->st_mode |= S_IEXEC;
      buf->st_ino = cipb.hFileInfo.ioDirID;
      buf->st_dev = cipb.hFileInfo.ioVRefNum;
      buf->st_size = cipb.hFileInfo.ioFlLgLen;
      buf->st_atime
	= buf->st_mtime
	= cipb.hFileInfo.ioFlMdDat - MAC_UNIX_EPOCH_DIFF;
      buf->st_ctime = cipb.hFileInfo.ioFlCrDat - MAC_UNIX_EPOCH_DIFF;
    }

  if (cipb.hFileInfo.ioFlFndrInfo.fdFlags & 0x8000)
    {
      /* identify alias files as symlinks */
      buf->st_mode &= ~S_IFREG;
      buf->st_mode |= S_IFLNK;
    }

  buf->st_nlink = 1;
  buf->st_uid = getuid ();
  buf->st_gid = getgid ();
  buf->st_rdev = 0;

  return 0;
}


int
lstat (const char *path, struct stat *buf)
{
  int result;
  char true_pathname[MAXPATHLEN+1];

  /* Try looking for the file without resolving aliases first.  */
  if ((result = stat_noalias (path, buf)) >= 0)
    return result;

  if (find_true_pathname (path, true_pathname, MAXPATHLEN+1) == -1)
    return -1;

  return stat_noalias (true_pathname, buf);
}


int
stat (const char *path, struct stat *sb)
{
  int result;
  char true_pathname[MAXPATHLEN+1], fully_resolved_name[MAXPATHLEN+1];
  int len;

  if ((result = stat_noalias (path, sb)) >= 0 &&
      ! (sb->st_mode & S_IFLNK))
    return result;

  if (find_true_pathname (path, true_pathname, MAXPATHLEN+1) == -1)
    return -1;

  len = readlink (true_pathname, fully_resolved_name, MAXPATHLEN);
  if (len > -1)
    {
      fully_resolved_name[len] = '\0';
        /* in fact our readlink terminates strings */
      return lstat (fully_resolved_name, sb);
    }
  else
    return lstat (true_pathname, sb);
}


#if __MRC__
/* CW defines fstat in stat.mac.c while MPW does not provide this
   function.  Without the information of how to get from a file
   descriptor in MPW StdCLib to a Mac OS file spec, it should be hard
   to implement this function.  Fortunately, there is only one place
   where this function is called in our configuration: in fileio.c,
   where only the st_dev and st_ino fields are used to determine
   whether two fildes point to different i-nodes to prevent copying
   a file onto itself equal.  What we have here probably needs
   improvement.  */

int
fstat (int fildes, struct stat *buf)
{
  buf->st_dev = 0;
  buf->st_ino = fildes;
  buf->st_mode = S_IFREG;  /* added by T.I. for the copy-file */
  return 0;  /* success */
}
#endif  /* __MRC__ */


int
mkdir (const char *dirname, int mode)
{
#pragma unused(mode)

  HFileParam hfpb;
  char true_pathname[MAXPATHLEN+1], mac_pathname[MAXPATHLEN+1];

  if (find_true_pathname (dirname, true_pathname, MAXPATHLEN+1) == -1)
    return -1;

  if (posix_to_mac_pathname (true_pathname, mac_pathname, MAXPATHLEN+1) == 0)
    return -1;

  c2pstr (mac_pathname);
  hfpb.ioNamePtr = mac_pathname;
  hfpb.ioVRefNum = 0;  /* ignored unless name is invalid */
  hfpb.ioDirID = 0;  /* parent is the root */

  errno = PBDirCreate ((HParmBlkPtr) &hfpb, false);
    /* just return the Mac OSErr code for now */
  return errno == noErr ? 0 : -1;
}


#undef rmdir
sys_rmdir (const char *dirname)
{
  HFileParam hfpb;
  char mac_pathname[MAXPATHLEN+1];

  if (posix_to_mac_pathname (dirname, mac_pathname, MAXPATHLEN+1) == 0)
    return -1;

  c2pstr (mac_pathname);
  hfpb.ioNamePtr = mac_pathname;
  hfpb.ioVRefNum = 0;  /* ignored unless name is invalid */
  hfpb.ioDirID = 0;  /* parent is the root */

  errno = PBHDelete ((HParmBlkPtr) &hfpb, false);
  return errno == noErr ? 0 : -1;
}


#ifdef __MRC__
/* No implementation yet. */
int
execvp (const char *path, ...)
{
  return -1;
}
#endif /* __MRC__ */


int
utime (const char *path, const struct utimbuf *times)
{
  char true_pathname[MAXPATHLEN+1], fully_resolved_name[MAXPATHLEN+1];
  int len;
  char mac_pathname[MAXPATHLEN+1];
  CInfoPBRec cipb;

  if (find_true_pathname (path, true_pathname, MAXPATHLEN+1) == -1)
    return -1;

  len = readlink (true_pathname, fully_resolved_name, MAXPATHLEN);
  if (len > -1)
    fully_resolved_name[len] = '\0';
  else
    strcpy (fully_resolved_name, true_pathname);

  if (!posix_to_mac_pathname (fully_resolved_name, mac_pathname, MAXPATHLEN+1))
    return -1;

  c2pstr (mac_pathname);
  cipb.hFileInfo.ioNamePtr = mac_pathname;
  cipb.hFileInfo.ioVRefNum = 0;
  cipb.hFileInfo.ioDirID = 0;
  cipb.hFileInfo.ioFDirIndex = 0;
    /* set to 0 to get information about specific dir or file */

  errno = PBGetCatInfo (&cipb, false);
  if (errno != noErr)
    return -1;

  if (cipb.hFileInfo.ioFlAttrib & 0x10)  /* bit 4 = 1 for directories */
    {
      if (times)
	cipb.dirInfo.ioDrMdDat = times->modtime + MAC_UNIX_EPOCH_DIFF;
      else
	GetDateTime (&cipb.dirInfo.ioDrMdDat);
    }
  else
    {
      if (times)
	cipb.hFileInfo.ioFlMdDat = times->modtime + MAC_UNIX_EPOCH_DIFF;
      else
	GetDateTime (&cipb.hFileInfo.ioFlMdDat);
    }

  errno = PBSetCatInfo (&cipb, false);
  return errno == noErr ? 0 : -1;
}


#ifndef F_OK
#define F_OK 0
#endif
#ifndef X_OK
#define X_OK 1
#endif
#ifndef W_OK
#define W_OK 2
#endif

/* Like stat, but test for access mode in hfpb.ioFlAttrib */
int
access (const char *path, int mode)
{
  char true_pathname[MAXPATHLEN+1], fully_resolved_name[MAXPATHLEN+1];
  int len;
  char mac_pathname[MAXPATHLEN+1];
  CInfoPBRec cipb;

  if (find_true_pathname (path, true_pathname, MAXPATHLEN+1) == -1)
    return -1;

  len = readlink (true_pathname, fully_resolved_name, MAXPATHLEN);
  if (len > -1)
    fully_resolved_name[len] = '\0';
  else
    strcpy (fully_resolved_name, true_pathname);

  if (!posix_to_mac_pathname (fully_resolved_name, mac_pathname, MAXPATHLEN+1))
    return -1;

  c2pstr (mac_pathname);
  cipb.hFileInfo.ioNamePtr = mac_pathname;
  cipb.hFileInfo.ioVRefNum = 0;
  cipb.hFileInfo.ioDirID = 0;
  cipb.hFileInfo.ioFDirIndex = 0;
    /* set to 0 to get information about specific dir or file */

  errno = PBGetCatInfo (&cipb, false);
  if (errno != noErr)
    return -1;

  if (mode == F_OK)  /* got this far, file exists */
    return 0;

  if (mode & X_OK)
    if (cipb.hFileInfo.ioFlAttrib & 0x10)  /* path refers to a directory */
      return 0;
    else
      {
	if (cipb.hFileInfo.ioFlFndrInfo.fdType == 'APPL')
	  return 0;
	else
	  return -1;
      }

  if (mode & W_OK)
    return (cipb.hFileInfo.ioFlAttrib & 0x1) ? -1 : 0;
      /* don't allow if lock bit is on */

  return -1;
}


#define DEV_NULL_FD 0x10000

#undef open
int
sys_open (const char *path, int oflag)
{
  char true_pathname[MAXPATHLEN+1], fully_resolved_name[MAXPATHLEN+1];
  int len;
  char mac_pathname[MAXPATHLEN+1];

  if (strcmp (path, "/dev/null") == 0)
    return DEV_NULL_FD;  /* some bogus fd to be ignored in write */

  if (find_true_pathname (path, true_pathname, MAXPATHLEN+1) == -1)
    return -1;

  len = readlink (true_pathname, fully_resolved_name, MAXPATHLEN);
  if (len > -1)
    fully_resolved_name[len] = '\0';
  else
    strcpy (fully_resolved_name, true_pathname);

  if (!posix_to_mac_pathname (fully_resolved_name, mac_pathname, MAXPATHLEN+1))
    return -1;
  else
    {
#ifdef __MRC__
      int res = open (mac_pathname, oflag);
      /* if (oflag == O_WRONLY || oflag == O_RDWR) */
      if (oflag & O_CREAT)
        fsetfileinfo (mac_pathname, 'EMAx', 'TEXT');
      return res;
#else /* not __MRC__ */
      return open (mac_pathname, oflag);
#endif /* not __MRC__ */
    }
}


#undef creat
int
sys_creat (const char *path, mode_t mode)
{
  char true_pathname[MAXPATHLEN+1];
  int len;
  char mac_pathname[MAXPATHLEN+1];

  if (find_true_pathname (path, true_pathname, MAXPATHLEN+1) == -1)
    return -1;

  if (!posix_to_mac_pathname (true_pathname, mac_pathname, MAXPATHLEN+1))
    return -1;
  else
    {
#ifdef __MRC__
      int result = creat (mac_pathname);
      fsetfileinfo (mac_pathname, 'EMAx', 'TEXT');
      return result;
#else /* not __MRC__ */
      return creat (mac_pathname, mode);
#endif /* not __MRC__ */
    }
}


#undef unlink
int
sys_unlink (const char *path)
{
  char true_pathname[MAXPATHLEN+1], fully_resolved_name[MAXPATHLEN+1];
  int len;
  char mac_pathname[MAXPATHLEN+1];

  if (find_true_pathname (path, true_pathname, MAXPATHLEN+1) == -1)
    return -1;

  len = readlink (true_pathname, fully_resolved_name, MAXPATHLEN);
  if (len > -1)
    fully_resolved_name[len] = '\0';
  else
    strcpy (fully_resolved_name, true_pathname);

  if (!posix_to_mac_pathname (fully_resolved_name, mac_pathname, MAXPATHLEN+1))
    return -1;
  else
    return unlink (mac_pathname);
}


#undef read
int
sys_read (int fildes, char *buf, int count)
{
  if (fildes == 0)  /* this should not be used for console input */
    return -1;
  else
#if __MSL__ >= 0x6000
    return _read (fildes, buf, count);
#else
    return read (fildes, buf, count);
#endif
}


#undef write
int
sys_write (int fildes, const char *buf, int count)
{
  if (fildes == DEV_NULL_FD)
    return count;
  else
#if __MSL__ >= 0x6000
    return _write (fildes, buf, count);
#else
    return write (fildes, buf, count);
#endif
}


#undef rename
int
sys_rename (const char * old_name, const char * new_name)
{
  char true_old_pathname[MAXPATHLEN+1], true_new_pathname[MAXPATHLEN+1];
  char fully_resolved_old_name[MAXPATHLEN+1];
  int len;
  char mac_old_name[MAXPATHLEN+1], mac_new_name[MAXPATHLEN+1];

  if (find_true_pathname (old_name, true_old_pathname, MAXPATHLEN+1) == -1)
    return -1;

  len = readlink (true_old_pathname, fully_resolved_old_name, MAXPATHLEN);
  if (len > -1)
    fully_resolved_old_name[len] = '\0';
  else
    strcpy (fully_resolved_old_name, true_old_pathname);

  if (find_true_pathname (new_name, true_new_pathname, MAXPATHLEN+1) == -1)
    return -1;

  if (strcmp (fully_resolved_old_name, true_new_pathname) == 0)
    return 0;

  if (!posix_to_mac_pathname (fully_resolved_old_name,
			     mac_old_name,
			     MAXPATHLEN+1))
    return -1;

  if (!posix_to_mac_pathname(true_new_pathname, mac_new_name, MAXPATHLEN+1))
    return -1;

  /* If a file with new_name already exists, rename deletes the old
     file in Unix.  CW version fails in these situation.  So we add a
     call to unlink here.  */
  (void) unlink (mac_new_name);

  return rename (mac_old_name, mac_new_name);
}


#undef fopen
extern FILE *fopen (const char *name, const char *mode);
FILE *
sys_fopen (const char *name, const char *mode)
{
  char true_pathname[MAXPATHLEN+1], fully_resolved_name[MAXPATHLEN+1];
  int len;
  char mac_pathname[MAXPATHLEN+1];

  if (find_true_pathname (name, true_pathname, MAXPATHLEN+1) == -1)
    return 0;

  len = readlink (true_pathname, fully_resolved_name, MAXPATHLEN);
  if (len > -1)
    fully_resolved_name[len] = '\0';
  else
    strcpy (fully_resolved_name, true_pathname);

  if (!posix_to_mac_pathname (fully_resolved_name, mac_pathname, MAXPATHLEN+1))
    return 0;
  else
    {
#ifdef __MRC__
      if (mode[0] == 'w' || mode[0] == 'a')
        fsetfileinfo (mac_pathname, 'EMAx', 'TEXT');
#endif /* not __MRC__ */
      return fopen (mac_pathname, mode);
    }
}


#include "keyboard.h"
extern Boolean mac_wait_next_event (EventRecord *, UInt32, Boolean);

int
select (n,  rfds, wfds, efds, timeout)
  int n;
  SELECT_TYPE *rfds;
  SELECT_TYPE *wfds;
  SELECT_TYPE *efds;
  struct timeval *timeout;
{
  OSErr err;
#if TARGET_API_MAC_CARBON
  EventTimeout timeout_sec =
    (timeout
     ? (EMACS_SECS (*timeout) * kEventDurationSecond
	+ EMACS_USECS (*timeout) * kEventDurationMicrosecond)
     : kEventDurationForever);

  BLOCK_INPUT;
  err = ReceiveNextEvent (0, NULL, timeout_sec, kEventLeaveInQueue, NULL);
  UNBLOCK_INPUT;
#else /* not TARGET_API_MAC_CARBON */
  EventRecord e;
  UInt32 sleep_time = EMACS_SECS (*timeout) * 60 +
    ((EMACS_USECS (*timeout) * 60) / 1000000);

  /* Can only handle wait for keyboard input.  */
  if (n > 1 || wfds || efds)
    return -1;

  /* Also return true if an event other than a keyDown has occurred.
     This causes kbd_buffer_get_event in keyboard.c to call
     read_avail_input which in turn calls XTread_socket to poll for
     these events.  Otherwise these never get processed except but a
     very slow poll timer.  */
  if (mac_wait_next_event (&e, sleep_time, false))
    err = noErr;
  else
    err = -9875;		/* eventLoopTimedOutErr */
#endif /* not TARGET_API_MAC_CARBON */

  if (FD_ISSET (0, rfds))
    if (err == noErr)
      return 1;
    else
      {
	FD_ZERO (rfds);
	return 0;
      }
  else
    if (err == noErr)
      {
	if (input_polling_used ())
	  {
	    /* It could be confusing if a real alarm arrives while
	       processing the fake one.  Turn it off and let the
	       handler reset it.  */
	    extern void poll_for_input_1 P_ ((void));
	    int old_poll_suppress_count = poll_suppress_count;
	    poll_suppress_count = 1;
	    poll_for_input_1 ();
	    poll_suppress_count = old_poll_suppress_count;
	  }
	errno = EINTR;
	return -1;
      }
    else
      return 0;
}


/* Simulation of SIGALRM.  The stub for function signal stores the
   signal handler function in alarm_signal_func if a SIGALRM is
   encountered.  */

#include <signal.h>
#include "syssignal.h"

static TMTask mac_atimer_task;

static QElemPtr mac_atimer_qlink = (QElemPtr) &mac_atimer_task;

static int signal_mask = 0;

#ifdef __MRC__
__sigfun alarm_signal_func = (__sigfun) 0;
#elif __MWERKS__
__signal_func_ptr alarm_signal_func = (__signal_func_ptr) 0;
#else /* not __MRC__ and not __MWERKS__ */
You lose!!!
#endif /* not __MRC__ and not __MWERKS__ */

#undef signal
#ifdef __MRC__
extern __sigfun signal (int signal, __sigfun signal_func);
__sigfun
sys_signal (int signal_num, __sigfun signal_func)
#elif __MWERKS__
extern __signal_func_ptr signal (int signal, __signal_func_ptr signal_func);
__signal_func_ptr
sys_signal (int signal_num, __signal_func_ptr signal_func)
#else /* not __MRC__ and not __MWERKS__ */
     You lose!!!
#endif /* not __MRC__ and not __MWERKS__ */
{
  if (signal_num != SIGALRM)
    return signal (signal_num, signal_func);
  else
    {
#ifdef __MRC__
      __sigfun old_signal_func;
#elif __MWERKS__
      __signal_func_ptr old_signal_func;
#else
      You lose!!!
#endif
      old_signal_func = alarm_signal_func;
      alarm_signal_func = signal_func;
      return old_signal_func;
    }
}


static pascal void
mac_atimer_handler (qlink)
     TMTaskPtr qlink;
{
  if (alarm_signal_func)
    (alarm_signal_func) (SIGALRM);
}


static void
set_mac_atimer (count)
     long count;
{
  static TimerUPP mac_atimer_handlerUPP = NULL;

  if (mac_atimer_handlerUPP == NULL)
    mac_atimer_handlerUPP = NewTimerUPP (mac_atimer_handler);
  mac_atimer_task.tmCount = 0;
  mac_atimer_task.tmAddr = mac_atimer_handlerUPP;
  mac_atimer_qlink = (QElemPtr) &mac_atimer_task;
  InsTime (mac_atimer_qlink);
  if (count)
    PrimeTime (mac_atimer_qlink, count);
}


int
remove_mac_atimer (remaining_count)
     long *remaining_count;
{
  if (mac_atimer_qlink)
    {
      RmvTime (mac_atimer_qlink);
      if (remaining_count)
	*remaining_count = mac_atimer_task.tmCount;
      mac_atimer_qlink = NULL;

      return 0;
    }
  else
    return -1;
}


int
sigblock (int mask)
{
  int old_mask = signal_mask;

  signal_mask |= mask;

  if ((old_mask ^ signal_mask) & sigmask (SIGALRM))
    remove_mac_atimer (NULL);

  return old_mask;
}


int
sigsetmask (int mask)
{
  int old_mask = signal_mask;

  signal_mask = mask;

  if ((old_mask ^ signal_mask) & sigmask (SIGALRM))
    if (signal_mask & sigmask (SIGALRM))
      remove_mac_atimer (NULL);
    else
      set_mac_atimer (mac_atimer_task.tmCount);

  return old_mask;
}


int
alarm (int seconds)
{
  long remaining_count;

  if (remove_mac_atimer (&remaining_count) == 0)
    {
      set_mac_atimer (seconds * 1000);

      return remaining_count / 1000;
    }
  else
    {
      mac_atimer_task.tmCount = seconds * 1000;

      return 0;
    }
}


int
setitimer (which, value, ovalue)
     int which;
     const struct itimerval *value;
     struct itimerval *ovalue;
{
  long remaining_count;
  long count = (EMACS_SECS (value->it_value) * 1000
		+ (EMACS_USECS (value->it_value) + 999) / 1000);

  if (remove_mac_atimer (&remaining_count) == 0)
    {
      if (ovalue)
	{
	  bzero (ovalue, sizeof (*ovalue));
	  EMACS_SET_SECS_USECS (ovalue->it_value, remaining_count / 1000,
				(remaining_count % 1000) * 1000);
	}
      set_mac_atimer (count);
    }
  else
    mac_atimer_task.tmCount = count;

  return 0;
}


/* gettimeofday should return the amount of time (in a timeval
   structure) since midnight today.  The toolbox function Microseconds
   returns the number of microseconds (in a UnsignedWide value) since
   the machine was booted.  Also making this complicated is WideAdd,
   WideSubtract, etc.  take wide values.  */

int
gettimeofday (tp)
     struct timeval *tp;
{
  static inited = 0;
  static wide wall_clock_at_epoch, clicks_at_epoch;
  UnsignedWide uw_microseconds;
  wide w_microseconds;
  time_t sys_time (time_t *);

  /* If this function is called for the first time, record the number
     of seconds since midnight and the number of microseconds since
     boot at the time of this first call.  */
  if (!inited)
    {
      time_t systime;
      inited = 1;
      systime = sys_time (NULL);
      /* Store microseconds since midnight in wall_clock_at_epoch.  */
      WideMultiply (systime, 1000000L, &wall_clock_at_epoch);
      Microseconds (&uw_microseconds);
      /* Store microseconds since boot in clicks_at_epoch.  */
      clicks_at_epoch.hi = uw_microseconds.hi;
      clicks_at_epoch.lo = uw_microseconds.lo;
    }

  /* Get time since boot */
  Microseconds (&uw_microseconds);

  /* Convert to time since midnight*/
  w_microseconds.hi = uw_microseconds.hi;
  w_microseconds.lo = uw_microseconds.lo;
  WideSubtract (&w_microseconds, &clicks_at_epoch);
  WideAdd (&w_microseconds, &wall_clock_at_epoch);
  tp->tv_sec = WideDivide (&w_microseconds, 1000000L, &tp->tv_usec);

  return 0;
}


#ifdef __MRC__
unsigned int
sleep (unsigned int seconds)
{
  unsigned long time_up;
  EventRecord e;

  time_up = TickCount () + seconds * 60;
  while (TickCount () < time_up)
    {
      /* Accept no event; just wait. by T.I.  */
      WaitNextEvent (0, &e, 30, NULL);
    }

  return (0);
}
#endif /* __MRC__ */


/* The time functions adjust time values according to the difference
   between the Unix and CW epoches. */

#undef gmtime
extern struct tm *gmtime (const time_t *);
struct tm *
sys_gmtime (const time_t *timer)
{
  time_t unix_time = *timer + CW_OR_MPW_UNIX_EPOCH_DIFF;

  return gmtime (&unix_time);
}


#undef localtime
extern struct tm *localtime (const time_t *);
struct tm *
sys_localtime (const time_t *timer)
{
#if __MSL__ >= 0x6000
  time_t unix_time = *timer;
#else
  time_t unix_time = *timer + CW_OR_MPW_UNIX_EPOCH_DIFF;
#endif

  return localtime (&unix_time);
}


#undef ctime
extern char *ctime (const time_t *);
char *
sys_ctime (const time_t *timer)
{
#if __MSL__ >= 0x6000
  time_t unix_time = *timer;
#else
  time_t unix_time = *timer + CW_OR_MPW_UNIX_EPOCH_DIFF;
#endif

  return ctime (&unix_time);
}


#undef time
extern time_t time (time_t *);
time_t
sys_time (time_t *timer)
{
#if __MSL__ >= 0x6000
  time_t mac_time = time (NULL);
#else
  time_t mac_time = time (NULL) - CW_OR_MPW_UNIX_EPOCH_DIFF;
#endif

  if (timer)
    *timer = mac_time;

  return mac_time;
}


/* no subprocesses, empty wait */

int
wait (int pid)
{
  return 0;
}


void
croak (char *badfunc)
{
  printf ("%s not yet implemented\r\n", badfunc);
  exit (1);
}


char *
mktemp (char *template)
{
  int len, k;
  static seqnum = 0;

  len = strlen (template);
  k = len - 1;
  while (k >= 0 && template[k] == 'X')
    k--;

  k++;  /* make k index of first 'X' */

  if (k < len)
    {
      /* Zero filled, number of digits equal to the number of X's.  */
      sprintf (&template[k], "%0*d", len-k, seqnum++);

      return template;
    }
  else
    return 0;
}


/* Emulate getpwuid, getpwnam and others.  */

#define PASSWD_FIELD_SIZE 256

static char my_passwd_name[PASSWD_FIELD_SIZE];
static char my_passwd_dir[MAXPATHLEN+1];

static struct passwd my_passwd =
{
  my_passwd_name,
  my_passwd_dir,
};

static struct group my_group =
{
  /* There are no groups on the mac, so we just return "root" as the
     group name.  */
  "root",
};


/* Initialized by main () in macterm.c to pathname of emacs directory.  */

char emacs_passwd_dir[MAXPATHLEN+1];

char *
getwd (char *);

void
init_emacs_passwd_dir ()
{
  int found = false;

  if (getwd (emacs_passwd_dir) && getwd (my_passwd_dir))
    {
      /* Need pathname of first ancestor that begins with "emacs"
	 since Mac emacs application is somewhere in the emacs-*
	 tree.  */
      int len = strlen (emacs_passwd_dir);
      int j = len - 1;
        /* j points to the "/" following the directory name being
	   compared.  */
      int i = j - 1;
      while (i >= 0 && !found)
	{
	  while (i >= 0 && emacs_passwd_dir[i] != '/')
	    i--;
	  if (emacs_passwd_dir[i] == '/' && i+5 < len)
	    found = (strncmp (&(emacs_passwd_dir[i+1]), "emacs", 5) == 0);
	  if (found)
	    emacs_passwd_dir[j+1] = '\0';
	  else
	    {
	      j = i;
	      i = j - 1;
	    }
	}
    }

  if (!found)
    {
      /* Setting to "/" probably won't work but set it to something
	 anyway.  */
      strcpy (emacs_passwd_dir, "/");
      strcpy (my_passwd_dir, "/");
    }
}


static struct passwd emacs_passwd =
{
  "emacs",
  emacs_passwd_dir,
};

static int my_passwd_inited = 0;


static void
init_my_passwd ()
{
  char **owner_name;

  /* Note: my_passwd_dir initialized in int_emacs_passwd_dir to
     directory where Emacs was started.  */

  owner_name = (char **) GetResource ('STR ',-16096);
  if (owner_name)
    {
      HLock (owner_name);
      BlockMove ((unsigned char *) *owner_name,
		 (unsigned char *) my_passwd_name,
		 *owner_name[0]+1);
      HUnlock (owner_name);
      p2cstr ((unsigned char *) my_passwd_name);
    }
  else
    my_passwd_name[0] = 0;
}


struct passwd *
getpwuid (uid_t uid)
{
  if (!my_passwd_inited)
    {
      init_my_passwd ();
      my_passwd_inited = 1;
    }

  return &my_passwd;
}


struct group *
getgrgid (gid_t gid)
{
  return &my_group;
}


struct passwd *
getpwnam (const char *name)
{
  if (strcmp (name, "emacs") == 0)
  	return &emacs_passwd;

  if (!my_passwd_inited)
    {
      init_my_passwd ();
      my_passwd_inited = 1;
    }

  return &my_passwd;
}


/* The functions fork, kill, sigsetmask, sigblock, request_sigio,
   setpgrp, setpriority, and unrequest_sigio are defined to be empty
   as in msdos.c.  */


int
fork ()
{
  return -1;
}


int
kill (int x, int y)
{
  return -1;
}


void
sys_subshell ()
{
  error ("Can't spawn subshell");
}


void
request_sigio (void)
{
}


void
unrequest_sigio (void)
{
}


int
setpgrp ()
{
  return 0;
}


/* No pipes yet.  */

int
pipe (int _fildes[2])
{
  errno = EACCES;
  return -1;
}


/* Hard and symbolic links.  */

int
symlink (const char *name1, const char *name2)
{
  errno = ENOENT;
  return -1;
}


int
link (const char *name1, const char *name2)
{
  errno = ENOENT;
  return -1;
}

#endif  /* ! MAC_OSX */

/* Determine the path name of the file specified by VREFNUM, DIRID,
   and NAME and place that in the buffer PATH of length
   MAXPATHLEN.  */
int
path_from_vol_dir_name (char *path, int man_path_len, short vol_ref_num,
			long dir_id, ConstStr255Param name)
{
  Str255 dir_name;
  CInfoPBRec cipb;
  OSErr err;

  if (strlen (name) > man_path_len)
    return 0;

  memcpy (dir_name, name, name[0]+1);
  memcpy (path, name, name[0]+1);
  p2cstr (path);

  cipb.dirInfo.ioDrParID = dir_id;
  cipb.dirInfo.ioNamePtr = dir_name;

  do
    {
      cipb.dirInfo.ioVRefNum = vol_ref_num;
      cipb.dirInfo.ioFDirIndex = -1;
      cipb.dirInfo.ioDrDirID = cipb.dirInfo.ioDrParID;
        /* go up to parent each time */

      err = PBGetCatInfo (&cipb, false);
      if (err != noErr)
        return 0;

      p2cstr (dir_name);
      if (strlen (dir_name) + strlen (path) + 1 >= man_path_len)
        return 0;

      strcat (dir_name, ":");
      strcat (dir_name, path);
        /* attach to front since we're going up directory tree */
      strcpy (path, dir_name);
    }
  while (cipb.dirInfo.ioDrDirID != fsRtDirID);
    /* stop when we see the volume's root directory */

  return 1;  /* success */
}


OSErr
posix_pathname_to_fsspec (ufn, fs)
     const char *ufn;
     FSSpec *fs;
{
  Str255 mac_pathname;

  if (posix_to_mac_pathname (ufn, mac_pathname, sizeof (mac_pathname)) == 0)
    return fnfErr;
  else
    {
      c2pstr (mac_pathname);
      return FSMakeFSSpec (0, 0, mac_pathname, fs);
    }
}

OSErr
fsspec_to_posix_pathname (fs, ufn, ufnbuflen)
     const FSSpec *fs;
     char *ufn;
     int ufnbuflen;
{
  char mac_pathname[MAXPATHLEN];

  if (path_from_vol_dir_name (mac_pathname, sizeof (mac_pathname) - 1,
			      fs->vRefNum, fs->parID, fs->name)
      && mac_to_posix_pathname (mac_pathname, ufn, ufnbuflen))
    return noErr;
  else
    return fnfErr;
}

#ifndef MAC_OSX

int
readlink (const char *path, char *buf, int bufsiz)
{
  char mac_sym_link_name[MAXPATHLEN+1];
  OSErr err;
  FSSpec fsspec;
  Boolean target_is_folder, was_aliased;
  Str255 directory_name, mac_pathname;
  CInfoPBRec cipb;

  if (posix_to_mac_pathname (path, mac_sym_link_name, MAXPATHLEN+1) == 0)
    return -1;

  c2pstr (mac_sym_link_name);
  err = FSMakeFSSpec (0, 0, mac_sym_link_name, &fsspec);
  if (err != noErr)
    {
      errno = ENOENT;
      return -1;
    }

  err = ResolveAliasFile (&fsspec, true, &target_is_folder, &was_aliased);
  if (err != noErr || !was_aliased)
    {
      errno = ENOENT;
      return -1;
    }

  if (path_from_vol_dir_name (mac_pathname, 255, fsspec.vRefNum, fsspec.parID,
			      fsspec.name) == 0)
    {
      errno = ENOENT;
      return -1;
    }

  if (mac_to_posix_pathname (mac_pathname, buf, bufsiz) == 0)
    {
      errno = ENOENT;
      return -1;
    }

  return strlen (buf);
}


/* Convert a path to one with aliases fully expanded.  */

static int
find_true_pathname (const char *path, char *buf, int bufsiz)
{
  char *q, temp[MAXPATHLEN+1];
  const char *p;
  int len;

  if (bufsiz <= 0 || path == 0 || path[0] == '\0')
    return -1;

  buf[0] = '\0';

  p = path;
  if (*p == '/')
    q = strchr (p + 1, '/');
  else
    q = strchr (p, '/');
  len = 0;  /* loop may not be entered, e.g., for "/" */

  while (q)
    {
      strcpy (temp, buf);
      strncat (temp, p, q - p);
      len = readlink (temp, buf, bufsiz);
      if (len <= -1)
        {
          if (strlen (temp) + 1 > bufsiz)
            return -1;
          strcpy (buf, temp);
        }
      strcat (buf, "/");
      len++;
      p = q + 1;
      q = strchr(p, '/');
    }

  if (len + strlen (p) + 1 >= bufsiz)
    return -1;

  strcat (buf, p);
  return len + strlen (p);
}


mode_t
umask (mode_t numask)
{
  static mode_t mask = 022;
  mode_t oldmask = mask;
  mask = numask;
  return oldmask;
}


int
chmod (const char *path, mode_t mode)
{
  /* say it always succeed for now */
  return 0;
}


int
fchmod (int fd, mode_t mode)
{
  /* say it always succeed for now */
  return 0;
}


int
fchown (int fd, uid_t owner, gid_t group)
{
  /* say it always succeed for now */
  return 0;
}


int
dup (int oldd)
{
#ifdef __MRC__
  return fcntl (oldd, F_DUPFD, 0);
#elif __MWERKS__
  /* current implementation of fcntl in fcntl.mac.c simply returns old
     descriptor */
  return fcntl (oldd, F_DUPFD);
#else
You lose!!!
#endif
}


/* This is from the original sysdep.c.  Emulate BSD dup2.  First close
   newd if it already exists.  Then, attempt to dup oldd.  If not
   successful, call dup2 recursively until we are, then close the
   unsuccessful ones.  */

int
dup2 (int oldd, int newd)
{
  int fd, ret;

  close (newd);

  fd = dup (oldd);
  if (fd == -1)
    return -1;
  if (fd == newd)
    return newd;
  ret = dup2 (oldd, newd);
  close (fd);
  return ret;
}


/* let it fail for now */

char *
sbrk (int incr)
{
  return (char *) -1;
}


int
fsync (int fd)
{
  return 0;
}


int
ioctl (int d, int request, void *argp)
{
  return -1;
}


#ifdef __MRC__
int
isatty (int fildes)
{
  if (fildes >=0 && fildes <= 2)
    return 1;
  else
    return 0;
}


int
getgid ()
{
  return 100;
}


int
getegid ()
{
  return 100;
}


int
getuid ()
{
  return 200;
}


int
geteuid ()
{
  return 200;
}
#endif /* __MRC__ */


#ifdef __MWERKS__
#if __MSL__ < 0x6000
#undef getpid
int
getpid ()
{
  return 9999;
}
#endif
#endif /* __MWERKS__ */

#endif /* ! MAC_OSX */


/* Return the path to the directory in which Emacs can create
   temporary files.  The MacOS "temporary items" directory cannot be
   used because it removes the file written by a process when it
   exits.  In that sense it's more like "/dev/null" than "/tmp" (but
   again not exactly).  And of course Emacs needs to read back the
   files written by its subprocesses.  So here we write the files to a
   directory "Emacs" in the Preferences Folder.  This directory is
   created if it does not exist.  */

char *
get_temp_dir_name ()
{
  static char *temp_dir_name = NULL;
  short vol_ref_num;
  long dir_id;
  OSErr err;
  Str255 dir_name, full_path;
  CInfoPBRec cpb;
  char unix_dir_name[MAXPATHLEN+1];
  DIR *dir;

  /* Cache directory name with pointer temp_dir_name.
     Look for it only the first time.  */
  if (!temp_dir_name)
    {
      err = FindFolder (kOnSystemDisk, kPreferencesFolderType, kCreateFolder,
			&vol_ref_num, &dir_id);
      if (err != noErr)
	return NULL;

      if (!path_from_vol_dir_name (full_path, 255, vol_ref_num, dir_id, "\p"))
        return NULL;

      if (strlen (full_path) + 6 <= MAXPATHLEN)
	strcat (full_path, "Emacs:");
      else
	return NULL;

      if (!mac_to_posix_pathname (full_path, unix_dir_name, MAXPATHLEN+1))
	return NULL;

      dir = opendir (unix_dir_name);  /* check whether temp directory exists */
      if (dir)
	closedir (dir);
      else if (mkdir (unix_dir_name, 0700) != 0)  /* create it if not */
	return NULL;

      temp_dir_name = (char *) malloc (strlen (unix_dir_name) + 1);
      strcpy (temp_dir_name, unix_dir_name);
    }

  return temp_dir_name;
}

#ifndef MAC_OSX

/* Allocate and construct an array of pointers to strings from a list
   of strings stored in a 'STR#' resource.  The returned pointer array
   is stored in the style of argv and environ: if the 'STR#' resource
   contains numString strings, a pointer array with numString+1
   elements is returned in which the last entry contains a null
   pointer.  The pointer to the pointer array is passed by pointer in
   parameter t.  The resource ID of the 'STR#' resource is passed in
   parameter StringListID.
   */

void
get_string_list (char ***t, short string_list_id)
{
  Handle h;
  Ptr p;
  int i, num_strings;

  h = GetResource ('STR#', string_list_id);
  if (h)
    {
      HLock (h);
      p = *h;
      num_strings = * (short *) p;
      p += sizeof(short);
      *t = (char **) malloc (sizeof (char *) * (num_strings + 1));
      for (i = 0; i < num_strings; i++)
        {
          short length = *p++;
          (*t)[i] = (char *) malloc (length + 1);
          strncpy ((*t)[i], p, length);
          (*t)[i][length] = '\0';
          p += length;
        }
      (*t)[num_strings] = 0;
      HUnlock (h);
    }
  else
    {
      /* Return no string in case GetResource fails.  Bug fixed by
         Ikegami Tsutomu.  Caused MPW build to crash without sym -on
         option (no sym -on implies -opt local). */
      *t = (char **) malloc (sizeof (char *));
      (*t)[0] = 0;
    }
}


static char *
get_path_to_system_folder ()
{
  short vol_ref_num;
  long dir_id;
  OSErr err;
  Str255 dir_name, full_path;
  CInfoPBRec cpb;
  static char system_folder_unix_name[MAXPATHLEN+1];
  DIR *dir;

  err = FindFolder (kOnSystemDisk, kSystemFolderType, kDontCreateFolder,
		    &vol_ref_num, &dir_id);
  if (err != noErr)
    return NULL;

  if (!path_from_vol_dir_name (full_path, 255, vol_ref_num, dir_id, "\p"))
    return NULL;

  if (!mac_to_posix_pathname (full_path, system_folder_unix_name,
			      MAXPATHLEN+1))
    return NULL;

  return system_folder_unix_name;
}


char **environ;

#define ENVIRON_STRING_LIST_ID 128

/* Get environment variable definitions from STR# resource.  */

void
init_environ ()
{
  int i;

  get_string_list (&environ, ENVIRON_STRING_LIST_ID);

  i = 0;
  while (environ[i])
    i++;

  /* Make HOME directory the one Emacs starts up in if not specified
     by resource.  */
  if (getenv ("HOME") == NULL)
    {
      environ = (char **) realloc (environ, sizeof (char *) * (i + 2));
      if (environ)
        {
          environ[i] = (char *) malloc (strlen (my_passwd_dir) + 6);
          if (environ[i])
            {
              strcpy (environ[i], "HOME=");
              strcat (environ[i], my_passwd_dir);
            }
          environ[i+1] = 0;
          i++;
        }
    }

  /* Make HOME directory the one Emacs starts up in if not specified
     by resource.  */
  if (getenv ("MAIL") == NULL)
    {
      environ = (char **) realloc (environ, sizeof (char *) * (i + 2));
      if (environ)
        {
          char * path_to_system_folder = get_path_to_system_folder ();
          environ[i] = (char *) malloc (strlen (path_to_system_folder) + 22);
          if (environ[i])
            {
              strcpy (environ[i], "MAIL=");
              strcat (environ[i], path_to_system_folder);
              strcat (environ[i], "Eudora Folder/In");
            }
          environ[i+1] = 0;
        }
    }
}


/* Return the value of the environment variable NAME.  */

char *
getenv (const char *name)
{
  int length = strlen(name);
  char **e;

  for (e = environ; *e != 0; e++)
    if (strncmp(*e, name, length) == 0 && (*e)[length] == '=')
      return &(*e)[length + 1];

  if (strcmp (name, "TMPDIR") == 0)
    return get_temp_dir_name ();

  return 0;
}


#ifdef __MRC__
/* see Interfaces&Libraries:Interfaces:CIncludes:signal.h */
char *sys_siglist[] =
{
  "Zero is not a signal!!!",
  "Abort", /* 1 */
  "Interactive user interrupt", /* 2 */ "?",
  "Floating point exception", /* 4 */ "?", "?", "?",
  "Illegal instruction", /* 8 */ "?", "?", "?", "?", "?", "?", "?",
  "Segment violation", /* 16 */ "?", "?", "?", "?", "?", "?", "?",
    "?", "?", "?", "?", "?", "?", "?", "?",
  "Terminal"  /* 32 */
};
#elif __MWERKS__
char *sys_siglist[] =
{
  "Zero is not a signal!!!",
  "Abort",
  "Floating point exception",
  "Illegal instruction",
  "Interactive user interrupt",
  "Segment violation",
  "Terminal"
};
#else /* not __MRC__ and not __MWERKS__ */
You lose!!!
#endif /* not __MRC__ and not __MWERKS__ */


#include <utsname.h>

int
uname (struct utsname *name)
{
  char **system_name;
  system_name = GetString (-16413);  /* IM - Resource Manager Reference */
  if (system_name)
    {
      BlockMove (*system_name, name->nodename, (*system_name)[0]+1);
      p2cstr (name->nodename);
      return 0;
    }
  else
    return -1;
}


/* Event class of HLE sent to subprocess.  */
const OSType kEmacsSubprocessSend = 'ESND';

/* Event class of HLE sent back from subprocess.  */
const OSType kEmacsSubprocessReply = 'ERPY';


char *
mystrchr (char *s, char c)
{
  while (*s && *s != c)
    {
      if (*s == '\\')
	s++;
      s++;
    }

  if (*s)
    {
      *s = '\0';
      return s;
    }
  else
    return NULL;
}


char *
mystrtok (char *s)
{
  while (*s)
    s++;

  return s + 1;
}


void
mystrcpy (char *to, char *from)
{
  while (*from)
    {
      if (*from == '\\')
	from++;
      *to++ = *from++;
    }
  *to = '\0';
}


/* Start a Mac subprocess.  Arguments for it is passed in argv (null
   terminated).  The process should run with the default directory
   "workdir", read input from "infn", and write output and error to
   "outfn" and "errfn", resp.  The Process Manager call
   LaunchApplication is used to start the subprocess.  We use high
   level events as the mechanism to pass arguments to the subprocess
   and to make Emacs wait for the subprocess to terminate and pass
   back a result code.  The bulk of the code here packs the arguments
   into one message to be passed together with the high level event.
   Emacs also sometimes starts a subprocess using a shell to perform
   wildcard filename expansion.  Since we don't really have a shell on
   the Mac, this case is detected and the starting of the shell is
   by-passed.  We really need to add code here to do filename
   expansion to support such functionality. */

int
run_mac_command (argv, workdir, infn, outfn, errfn)
     unsigned char **argv;
     const char *workdir;
     const char *infn, *outfn, *errfn;
{
#if TARGET_API_MAC_CARBON
  return -1;
#else /* not TARGET_API_MAC_CARBON */
  char macappname[MAXPATHLEN+1], macworkdir[MAXPATHLEN+1];
  char macinfn[MAXPATHLEN+1], macoutfn[MAXPATHLEN+1], macerrfn[MAXPATHLEN+1];
  int paramlen, argc, newargc, j, retries;
  char **newargv, *param, *p;
  OSErr iErr;
  FSSpec spec;
  LaunchParamBlockRec lpbr;
  EventRecord send_event, reply_event;
  RgnHandle cursor_region_handle;
  TargetID targ;
  unsigned long ref_con, len;

  if (posix_to_mac_pathname (workdir, macworkdir, MAXPATHLEN+1) == 0)
    return -1;
  if (posix_to_mac_pathname (infn, macinfn, MAXPATHLEN+1) == 0)
    return -1;
  if (posix_to_mac_pathname (outfn, macoutfn, MAXPATHLEN+1) == 0)
    return -1;
  if (posix_to_mac_pathname (errfn, macerrfn, MAXPATHLEN+1) == 0)
    return -1;

  paramlen = strlen (macworkdir) + strlen (macinfn) + strlen (macoutfn)
             + strlen (macerrfn) + 4;  /* count nulls at end of strings */

  argc = 0;
  while (argv[argc])
    argc++;

  if (argc == 0)
    return -1;

  /* If a subprocess is invoked with a shell, we receive 3 arguments
     of the form: "<path to emacs bins>/sh" "-c" "<path to emacs
     bins>/<command> <command args>" */
  j = strlen (argv[0]);
  if (j >= 3 && strcmp (argv[0]+j-3, "/sh") == 0
      && argc == 3 && strcmp (argv[1], "-c") == 0)
    {
      char *command, *t, tempmacpathname[MAXPATHLEN+1];

      /* The arguments for the command in argv[2] are separated by
	 spaces.  Count them and put the count in newargc.  */
      command = (char *) alloca (strlen (argv[2])+2);
      strcpy (command, argv[2]);
      if (command[strlen (command) - 1] != ' ')
	strcat (command, " ");

      t = command;
      newargc = 0;
      t = mystrchr (t, ' ');
      while (t)
	{
	  newargc++;
	  t = mystrchr (t+1, ' ');
	}

      newargv = (char **) alloca (sizeof (char *) * newargc);

      t = command;
      for (j = 0; j < newargc; j++)
	{
	  newargv[j] = (char *) alloca (strlen (t) + 1);
	  mystrcpy (newargv[j], t);

	  t = mystrtok (t);
	  paramlen += strlen (newargv[j]) + 1;
	}

      if (strncmp (newargv[0], "~emacs/", 7) == 0)
	{
	  if (posix_to_mac_pathname (newargv[0], tempmacpathname, MAXPATHLEN+1)
	      == 0)
	    return -1;
	}
      else
	{  /* sometimes Emacs call "sh" without a path for the command */
#if 0
	  char *t = (char *) alloca (strlen (newargv[0]) + 7 + 1);
	  strcpy (t, "~emacs/");
	  strcat (t, newargv[0]);
#endif /* 0 */
	  Lisp_Object path;
	  openp (Vexec_path, build_string (newargv[0]), Vexec_suffixes, &path,
		 make_number (X_OK));

	  if (NILP (path))
	    return -1;
	  if (posix_to_mac_pathname (SDATA (path), tempmacpathname,
				    MAXPATHLEN+1) == 0)
	    return -1;
	}
      strcpy (macappname, tempmacpathname);
    }
  else
    {
      if (posix_to_mac_pathname (argv[0], macappname, MAXPATHLEN+1) == 0)
	return -1;

      newargv = (char **) alloca (sizeof (char *) * argc);
      newargc = argc;
      for (j = 1; j < argc; j++)
	{
	  if (strncmp (argv[j], "~emacs/", 7) == 0)
	    {
	      char *t = strchr (argv[j], ' ');
	      if (t)
		{
		  char tempcmdname[MAXPATHLEN+1], tempmaccmdname[MAXPATHLEN+1];
		  strncpy (tempcmdname, argv[j], t-argv[j]);
		  tempcmdname[t-argv[j]] = '\0';
		  if (posix_to_mac_pathname (tempcmdname, tempmaccmdname,
					    MAXPATHLEN+1) == 0)
		    return -1;
		  newargv[j] = (char *) alloca (strlen (tempmaccmdname)
						+ strlen (t) + 1);
		  strcpy (newargv[j], tempmaccmdname);
		  strcat (newargv[j], t);
		}
	      else
		{
		  char tempmaccmdname[MAXPATHLEN+1];
		  if (posix_to_mac_pathname (argv[j], tempmaccmdname,
					    MAXPATHLEN+1) == 0)
		    return -1;
		  newargv[j] = (char *) alloca (strlen (tempmaccmdname)+1);
		  strcpy (newargv[j], tempmaccmdname);
		}
	    }
	  else
	    newargv[j] = argv[j];
	  paramlen += strlen (newargv[j]) + 1;
	}
    }

  /* After expanding all the arguments, we now know the length of the
     parameter block to be sent to the subprocess as a message
     attached to the HLE.  */
  param = (char *) malloc (paramlen + 1);
  if (!param)
    return -1;

  p = param;
  *p++ = newargc;
    /* first byte of message contains number of arguments for command */
  strcpy (p, macworkdir);
  p += strlen (macworkdir);
  *p++ = '\0';
    /* null terminate strings sent so it's possible to use strcpy over there */
  strcpy (p, macinfn);
  p += strlen (macinfn);
  *p++ = '\0';
  strcpy (p, macoutfn);
  p += strlen (macoutfn);
  *p++ = '\0';
  strcpy (p, macerrfn);
  p += strlen (macerrfn);
  *p++ = '\0';
  for (j = 1; j < newargc; j++)
    {
      strcpy (p, newargv[j]);
      p += strlen (newargv[j]);
      *p++ = '\0';
    }

  c2pstr (macappname);

  iErr = FSMakeFSSpec (0, 0, macappname, &spec);

  if (iErr != noErr)
    {
      free (param);
      return -1;
    }

  lpbr.launchBlockID = extendedBlock;
  lpbr.launchEPBLength = extendedBlockLen;
  lpbr.launchControlFlags = launchContinue + launchNoFileFlags;
  lpbr.launchAppSpec = &spec;
  lpbr.launchAppParameters = NULL;

  iErr = LaunchApplication (&lpbr);  /* call the subprocess */
  if (iErr != noErr)
    {
      free (param);
      return -1;
    }

  send_event.what = kHighLevelEvent;
  send_event.message = kEmacsSubprocessSend;
    /* Event ID stored in "where" unused */

  retries = 3;
  /* OS may think current subprocess has terminated if previous one
     terminated recently.  */
  do
    {
      iErr = PostHighLevelEvent (&send_event, &lpbr.launchProcessSN, 0, param,
				 paramlen + 1, receiverIDisPSN);
    }
  while (iErr == sessClosedErr && retries-- > 0);

  if (iErr != noErr)
    {
      free (param);
      return -1;
    }

  cursor_region_handle = NewRgn ();

  /* Wait for the subprocess to finish, when it will send us a ERPY
     high level event.  */
  while (1)
    if (WaitNextEvent (highLevelEventMask, &reply_event, 180,
		       cursor_region_handle)
	&& reply_event.message == kEmacsSubprocessReply)
      break;

  /* The return code is sent through the refCon */
  iErr = AcceptHighLevelEvent (&targ, &ref_con, NULL, &len);
  if (iErr != noErr)
    {
      DisposeHandle ((Handle) cursor_region_handle);
      free (param);
      return -1;
    }

  DisposeHandle ((Handle) cursor_region_handle);
  free (param);

  return ref_con;
#endif /* not TARGET_API_MAC_CARBON */
}


DIR *
opendir (const char *dirname)
{
  char true_pathname[MAXPATHLEN+1], fully_resolved_name[MAXPATHLEN+1];
  char mac_pathname[MAXPATHLEN+1], vol_name[MAXPATHLEN+1];
  DIR *dirp;
  CInfoPBRec cipb;
  HVolumeParam vpb;
  int len, vol_name_len;

  if (find_true_pathname (dirname, true_pathname, MAXPATHLEN+1) == -1)
    return 0;

  len = readlink (true_pathname, fully_resolved_name, MAXPATHLEN);
  if (len > -1)
    fully_resolved_name[len] = '\0';
  else
    strcpy (fully_resolved_name, true_pathname);

  dirp = (DIR *) malloc (sizeof(DIR));
  if (!dirp)
    return 0;

  /* Handle special case when dirname is "/": sets up for readir to
     get all mount volumes.  */
  if (strcmp (fully_resolved_name, "/") == 0)
    {
      dirp->getting_volumes = 1;  /* special all mounted volumes DIR struct */
      dirp->current_index = 1;  /* index for first volume */
      return dirp;
    }

  /* Handle typical cases: not accessing all mounted volumes.  */
  if (!posix_to_mac_pathname (fully_resolved_name, mac_pathname, MAXPATHLEN+1))
    return 0;

  /* Emacs calls opendir without the trailing '/', Mac needs trailing ':' */
  len = strlen (mac_pathname);
  if (mac_pathname[len - 1] != ':' && len < MAXPATHLEN)
    strcat (mac_pathname, ":");

  /* Extract volume name */
  vol_name_len = strchr (mac_pathname, ':') - mac_pathname;
  strncpy (vol_name, mac_pathname, vol_name_len);
  vol_name[vol_name_len] = '\0';
  strcat (vol_name, ":");

  c2pstr (mac_pathname);
  cipb.hFileInfo.ioNamePtr = mac_pathname;
    /* using full pathname so vRefNum and DirID ignored */
  cipb.hFileInfo.ioVRefNum = 0;
  cipb.hFileInfo.ioDirID = 0;
  cipb.hFileInfo.ioFDirIndex = 0;
    /* set to 0 to get information about specific dir or file */

  errno = PBGetCatInfo (&cipb, false);
  if (errno != noErr)
    {
      errno = ENOENT;
      return 0;
    }

  if (!(cipb.hFileInfo.ioFlAttrib & 0x10))  /* bit 4 = 1 for directories */
    return 0;  /* not a directory */

  dirp->dir_id = cipb.dirInfo.ioDrDirID;  /* used later in readdir */
  dirp->getting_volumes = 0;
  dirp->current_index = 1;  /* index for first file/directory */

  c2pstr (vol_name);
  vpb.ioNamePtr = vol_name;
    /* using full pathname so vRefNum and DirID ignored */
  vpb.ioVRefNum = 0;
  vpb.ioVolIndex = -1;
  errno = PBHGetVInfo ((union HParamBlockRec *) &vpb, false);
  if (errno != noErr)
    {
      errno = ENOENT;
      return 0;
    }

  dirp->vol_ref_num = vpb.ioVRefNum;

  return dirp;
}

int
closedir (DIR *dp)
{
  free (dp);

  return 0;
}


struct dirent *
readdir (DIR *dp)
{
  HParamBlockRec hpblock;
  CInfoPBRec cipb;
  static struct dirent s_dirent;
  static Str255 s_name;
  int done;
  char *p;

  /* Handle the root directory containing the mounted volumes.  Call
     PBHGetVInfo specifying an index to obtain the info for a volume.
     PBHGetVInfo returns an error when it receives an index beyond the
     last volume, at which time we should return a nil dirent struct
     pointer.  */
  if (dp->getting_volumes)
    {
      hpblock.volumeParam.ioNamePtr = s_name;
      hpblock.volumeParam.ioVRefNum = 0;
      hpblock.volumeParam.ioVolIndex = dp->current_index;

      errno = PBHGetVInfo (&hpblock, false);
      if (errno != noErr)
	{
	  errno = ENOENT;
	  return 0;
	}

      p2cstr (s_name);
      strcat (s_name, "/");  /* need "/" for stat to work correctly */

      dp->current_index++;

      s_dirent.d_ino = hpblock.volumeParam.ioVRefNum;
      s_dirent.d_name = s_name;

      return &s_dirent;
    }
  else
    {
      cipb.hFileInfo.ioVRefNum = dp->vol_ref_num;
      cipb.hFileInfo.ioNamePtr = s_name;
        /* location to receive filename returned */

      /* return only visible files */
      done = false;
      while (!done)
	{
	  cipb.hFileInfo.ioDirID = dp->dir_id;
	    /* directory ID found by opendir */
	  cipb.hFileInfo.ioFDirIndex = dp->current_index;

	  errno = PBGetCatInfo (&cipb, false);
	  if (errno != noErr)
	    {
	      errno = ENOENT;
	      return 0;
	    }

	  /* insist on a visible entry */
	  if (cipb.hFileInfo.ioFlAttrib & 0x10)  /* directory? */
	    done = !(cipb.dirInfo.ioDrUsrWds.frFlags & fInvisible);
	  else
	    done = !(cipb.hFileInfo.ioFlFndrInfo.fdFlags & fInvisible);

	  dp->current_index++;
	}

      p2cstr (s_name);

      p = s_name;
      while (*p)
        {
          if (*p == '/')
            *p = ':';
          p++;
        }

      s_dirent.d_ino = cipb.dirInfo.ioDrDirID;
        /* value unimportant: non-zero for valid file */
      s_dirent.d_name = s_name;

      return &s_dirent;
    }
}


char *
getwd (char *path)
{
  char mac_pathname[MAXPATHLEN+1];
  Str255 directory_name;
  OSErr errno;
  CInfoPBRec cipb;

  if (path_from_vol_dir_name (mac_pathname, 255, 0, 0, "\p") == 0)
    return NULL;

  if (mac_to_posix_pathname (mac_pathname, path, MAXPATHLEN+1) == 0)
    return 0;
  else
    return path;
}

#endif  /* ! MAC_OSX */


void
initialize_applescript ()
{
  AEDesc null_desc;
  OSAError osaerror;

  /* if open fails, as_scripting_component is set to NULL.  Its
     subsequent use in OSA calls will fail with badComponentInstance
     error.  */
  as_scripting_component = OpenDefaultComponent (kOSAComponentType,
						 kAppleScriptSubtype);

  null_desc.descriptorType = typeNull;
  null_desc.dataHandle = 0;
  osaerror = OSAMakeContext (as_scripting_component, &null_desc,
			     kOSANullScript, &as_script_context);
  if (osaerror)
    as_script_context = kOSANullScript;
      /* use default context if create fails */
}


void
terminate_applescript()
{
  OSADispose (as_scripting_component, as_script_context);
  CloseComponent (as_scripting_component);
}

/* Convert a lisp string to the 4 byte character code.  */
 
OSType
mac_get_code_from_arg(Lisp_Object arg, OSType defCode)
{
  OSType result;
  if (NILP(arg))
    {
      result = defCode;
    } 
  else
    {
      /* check type string */
      CHECK_STRING(arg);
      if (SBYTES (arg) != 4)
	{
	  error ("Wrong argument: need string of length 4 for code");
	}
      result = EndianU32_BtoN (*((UInt32 *) SDATA (arg)));
    }
  return result;
}

/* Convert the 4 byte character code into a 4 byte string.  */

Lisp_Object
mac_get_object_from_code(OSType defCode)
{
  UInt32 code = EndianU32_NtoB (defCode);

  return make_unibyte_string ((char *)&code, 4);
}


DEFUN ("mac-get-file-creator", Fmac_get_file_creator, Smac_get_file_creator, 1, 1, 0,
       doc: /* Get the creator code of FILENAME as a four character string. */)
  (filename)
     Lisp_Object filename;
{
  OSErr	status;
#ifdef MAC_OSX
  FSRef fref;
#else
  FSSpec fss;
#endif
  OSType cCode;
  Lisp_Object result = Qnil;
  CHECK_STRING (filename);

  if (NILP(Ffile_exists_p(filename)) || !NILP(Ffile_directory_p(filename))) {
    return Qnil;
  }
  filename = Fexpand_file_name (filename, Qnil);

  BLOCK_INPUT;
#ifdef MAC_OSX
  status = FSPathMakeRef(SDATA(ENCODE_FILE(filename)), &fref, NULL);
#else
  status = posix_pathname_to_fsspec (SDATA (ENCODE_FILE (filename)), &fss);
#endif

  if (status == noErr)
    {
#ifdef MAC_OSX
      FSCatalogInfo catalogInfo;

      status = FSGetCatalogInfo(&fref, kFSCatInfoFinderInfo,
				&catalogInfo, NULL, NULL, NULL);
#else
      FInfo finder_info;

      status = FSpGetFInfo (&fss, &finder_info);
#endif
      if (status == noErr) 
	{
#ifdef MAC_OSX
	  result = mac_get_object_from_code(((FileInfo*)&catalogInfo.finderInfo)->fileCreator);
#else
	  result = mac_get_object_from_code (finder_info.fdCreator);
#endif
	}
    }
  UNBLOCK_INPUT;
  if (status != noErr) {
    error ("Error while getting file information.");
  }
  return result;
}

DEFUN ("mac-get-file-type", Fmac_get_file_type, Smac_get_file_type, 1, 1, 0,
       doc: /* Get the type code of FILENAME as a four character string. */)
  (filename)
     Lisp_Object filename;
{
  OSErr	status;
#ifdef MAC_OSX
  FSRef fref;
#else
  FSSpec fss;
#endif
  OSType cCode;
  Lisp_Object result = Qnil;
  CHECK_STRING (filename);

  if (NILP(Ffile_exists_p(filename)) || !NILP(Ffile_directory_p(filename))) {
    return Qnil;
  }
  filename = Fexpand_file_name (filename, Qnil);

  BLOCK_INPUT;
#ifdef MAC_OSX
  status = FSPathMakeRef(SDATA(ENCODE_FILE(filename)), &fref, NULL);
#else
  status = posix_pathname_to_fsspec (SDATA (ENCODE_FILE (filename)), &fss);
#endif

  if (status == noErr)
    {
#ifdef MAC_OSX
      FSCatalogInfo catalogInfo;

      status = FSGetCatalogInfo(&fref, kFSCatInfoFinderInfo,
				&catalogInfo, NULL, NULL, NULL);
#else
      FInfo finder_info;

      status = FSpGetFInfo (&fss, &finder_info);
#endif
      if (status == noErr) 
	{
#ifdef MAC_OSX
	  result = mac_get_object_from_code(((FileInfo*)&catalogInfo.finderInfo)->fileType);
#else
	  result = mac_get_object_from_code (finder_info.fdType);
#endif
	}
    }
  UNBLOCK_INPUT;
  if (status != noErr) {
    error ("Error while getting file information.");
  }
  return result;
}

DEFUN ("mac-set-file-creator", Fmac_set_file_creator, Smac_set_file_creator, 1, 2, 0,
       doc: /* Set creator code of file FILENAME to CODE.
If non-nil, CODE must be a 4-character string.  Otherwise, 'EMAx' is
assumed. Return non-nil if successful.  */)
  (filename, code)
     Lisp_Object filename, code;
{
  OSErr	status;
#ifdef MAC_OSX
  FSRef fref;
#else
  FSSpec fss;
#endif
  OSType cCode;
  CHECK_STRING (filename);

  cCode = mac_get_code_from_arg(code, 'EMAx');

  if (NILP(Ffile_exists_p(filename)) || !NILP(Ffile_directory_p(filename))) {
    return Qnil;
  }
  filename = Fexpand_file_name (filename, Qnil);

  BLOCK_INPUT;
#ifdef MAC_OSX
  status = FSPathMakeRef(SDATA(ENCODE_FILE(filename)), &fref, NULL);
#else
  status = posix_pathname_to_fsspec (SDATA (ENCODE_FILE (filename)), &fss);
#endif

  if (status == noErr)
    {
#ifdef MAC_OSX
      FSCatalogInfo catalogInfo;
      FSRef parentDir;
      status = FSGetCatalogInfo(&fref, kFSCatInfoFinderInfo,
				&catalogInfo, NULL, NULL, &parentDir);
#else
      FInfo finder_info;

      status = FSpGetFInfo (&fss, &finder_info);
#endif
      if (status == noErr) 
	{
#ifdef MAC_OSX
	((FileInfo*)&catalogInfo.finderInfo)->fileCreator = cCode;
	status = FSSetCatalogInfo(&fref, kFSCatInfoFinderInfo, &catalogInfo);
	/* TODO: on Mac OS 10.2, we need to touch the parent dir, FNNotify? */
#else
	finder_info.fdCreator = cCode;
	status = FSpSetFInfo (&fss, &finder_info);
#endif
	}
    }
  UNBLOCK_INPUT;
  if (status != noErr) {
    error ("Error while setting creator information.");
  }
  return Qt;
}

DEFUN ("mac-set-file-type", Fmac_set_file_type, Smac_set_file_type, 2, 2, 0,
       doc: /* Set file code of file FILENAME to CODE.
CODE must be a 4-character string.  Return non-nil if successful.  */)
  (filename, code)
     Lisp_Object filename, code;
{
  OSErr	status;
#ifdef MAC_OSX
  FSRef fref;
#else
  FSSpec fss;
#endif
  OSType cCode;
  CHECK_STRING (filename);

  cCode = mac_get_code_from_arg(code, 0); /* Default to empty code*/

  if (NILP(Ffile_exists_p(filename)) || !NILP(Ffile_directory_p(filename))) {
    return Qnil;
  }
  filename = Fexpand_file_name (filename, Qnil);

  BLOCK_INPUT;
#ifdef MAC_OSX
  status = FSPathMakeRef(SDATA(ENCODE_FILE(filename)), &fref, NULL);
#else
  status = posix_pathname_to_fsspec (SDATA (ENCODE_FILE (filename)), &fss);
#endif

  if (status == noErr)
    {
#ifdef MAC_OSX
      FSCatalogInfo catalogInfo;
      FSRef parentDir;
      status = FSGetCatalogInfo(&fref, kFSCatInfoFinderInfo,
				&catalogInfo, NULL, NULL, &parentDir);
#else
      FInfo finder_info;

      status = FSpGetFInfo (&fss, &finder_info);
#endif
      if (status == noErr) 
	{
#ifdef MAC_OSX
	((FileInfo*)&catalogInfo.finderInfo)->fileType = cCode;
	status = FSSetCatalogInfo(&fref, kFSCatInfoFinderInfo, &catalogInfo);
	/* TODO: on Mac OS 10.2, we need to touch the parent dir, FNNotify? */
#else
	finder_info.fdType = cCode;
	status = FSpSetFInfo (&fss, &finder_info);
#endif
	}
    }
  UNBLOCK_INPUT;
  if (status != noErr) {
    error ("Error while setting creator information.");
  }
  return Qt;
}


/* Compile and execute the AppleScript SCRIPT and return the error
   status as function value.  A zero is returned if compilation and
   execution is successful, in which case RESULT returns a pointer to
   a string containing the resulting script value.  Otherwise, the Mac
   error code is returned and RESULT returns a pointer to an error
   string.  In both cases the caller should deallocate the storage
   used by the string pointed to by RESULT if it is non-NULL.  For
   documentation on the MacOS scripting architecture, see Inside
   Macintosh - Interapplication Communications: Scripting Components.  */

static long
do_applescript (char *script, char **result)
{
  AEDesc script_desc, result_desc, error_desc;
  OSErr error;
  OSAError osaerror;
  long length;

  *result = 0;

  if (!as_scripting_component)
    initialize_applescript();

  error = AECreateDesc (typeChar, script, strlen(script), &script_desc);
  if (error)
    return error;

  osaerror = OSADoScript (as_scripting_component, &script_desc, kOSANullScript,
			  typeChar, kOSAModeNull, &result_desc);

  if (osaerror == errOSAScriptError)
    {
      /* error executing AppleScript: retrieve error message */
      if (!OSAScriptError (as_scripting_component, kOSAErrorMessage, typeChar,
			   &error_desc))
        {
#if TARGET_API_MAC_CARBON
          length = AEGetDescDataSize (&error_desc);
          *result = (char *) xmalloc (length + 1);
          if (*result)
            {
              AEGetDescData (&error_desc, *result, length);
              *(*result + length) = '\0';
            }
#else /* not TARGET_API_MAC_CARBON */
          HLock (error_desc.dataHandle);
          length = GetHandleSize(error_desc.dataHandle);
          *result = (char *) xmalloc (length + 1);
          if (*result)
            {
              memcpy (*result, *(error_desc.dataHandle), length);
              *(*result + length) = '\0';
            }
          HUnlock (error_desc.dataHandle);
#endif /* not TARGET_API_MAC_CARBON */
          AEDisposeDesc (&error_desc);
        }
    }
  else if (osaerror == noErr)  /* success: retrieve resulting script value */
    {
#if TARGET_API_MAC_CARBON
      length = AEGetDescDataSize (&result_desc);
      *result = (char *) xmalloc (length + 1);
      if (*result)
        {
          AEGetDescData (&result_desc, *result, length);
          *(*result + length) = '\0';
        }
#else /* not TARGET_API_MAC_CARBON */
      HLock (result_desc.dataHandle);
      length = GetHandleSize(result_desc.dataHandle);
      *result = (char *) xmalloc (length + 1);
      if (*result)
        {
          memcpy (*result, *(result_desc.dataHandle), length);
          *(*result + length) = '\0';
        }
      HUnlock (result_desc.dataHandle);
#endif /* not TARGET_API_MAC_CARBON */
      AEDisposeDesc (&result_desc);
    }

  AEDisposeDesc (&script_desc);

  return osaerror;
}


DEFUN ("do-applescript", Fdo_applescript, Sdo_applescript, 1, 1, 0,
       doc: /* Compile and execute AppleScript SCRIPT and retrieve and return the result.
If compilation and execution are successful, the resulting script
value is returned as a string.  Otherwise the function aborts and
displays the error message returned by the AppleScript scripting
component.  */)
  (script)
    Lisp_Object script;
{
  char *result, *temp;
  Lisp_Object lisp_result;
  long status;

  CHECK_STRING (script);

  BLOCK_INPUT;
  status = do_applescript (SDATA (script), &result);
  UNBLOCK_INPUT;
  if (status)
    {
      if (!result)
        error ("AppleScript error %d", status);
      else
        {
          /* Unfortunately only OSADoScript in do_applescript knows how
             how large the resulting script value or error message is
             going to be and therefore as caller memory must be
             deallocated here.  It is necessary to free the error
             message before calling error to avoid a memory leak.  */
          temp = (char *) alloca (strlen (result) + 1);
          strcpy (temp, result);
          xfree (result);
          error (temp);
        }
    }
  else
    {
      lisp_result = build_string (result);
      xfree (result);
      return lisp_result;
    }
}


DEFUN ("mac-file-name-to-posix", Fmac_file_name_to_posix,
       Smac_file_name_to_posix, 1, 1, 0,
       doc: /* Convert Macintosh filename to Posix form.  */)
     (mac_filename)
     Lisp_Object mac_filename;
{
  char posix_filename[MAXPATHLEN+1];

  CHECK_STRING (mac_filename);

  if (mac_to_posix_pathname (SDATA (mac_filename), posix_filename,
			   MAXPATHLEN))
    return build_string (posix_filename);
  else
    return Qnil;
}


DEFUN ("posix-file-name-to-mac", Fposix_file_name_to_mac,
       Sposix_file_name_to_mac, 1, 1, 0,
       doc: /* Convert Posix filename to Mac form.  */)
     (posix_filename)
     Lisp_Object posix_filename;
{
  char mac_filename[MAXPATHLEN+1];

  CHECK_STRING (posix_filename);

  if (posix_to_mac_pathname (SDATA (posix_filename), mac_filename,
			   MAXPATHLEN))
    return build_string (mac_filename);
  else
    return Qnil;
}


#if TARGET_API_MAC_CARBON
static Lisp_Object Qxml, Qmime_charset;
static Lisp_Object QNFD, QNFKD, QNFC, QNFKC, QHFS_plus_D, QHFS_plus_C;

DEFUN ("mac-get-preference", Fmac_get_preference, Smac_get_preference, 1, 4, 0,
       doc: /* Return the application preference value for KEY.
KEY is either a string specifying a preference key, or a list of key
strings.  If it is a list, the (i+1)-th element is used as a key for
the CFDictionary value obtained by the i-th element.  If lookup is
failed at some stage, nil is returned.

Optional arg APPLICATION is an application ID string.  If omitted or
nil, that stands for the current application.

Optional arg FORMAT specifies the data format of the return value.  If
omitted or nil, each Core Foundation object is converted into a
corresponding Lisp object as follows:

  Core Foundation    Lisp                           Tag
  ------------------------------------------------------------
  CFString           Multibyte string               string
  CFNumber           Integer or float               number
  CFBoolean          Symbol (t or nil)              boolean
  CFDate             List of three integers         date
                       (cf. `current-time')
  CFData             Unibyte string                 data
  CFArray            Vector                         array
  CFDictionary       Alist or hash table            dictionary
                       (depending on HASH-BOUND)

If it is t, a symbol that represents the type of the original Core
Foundation object is prepended.  If it is `xml', the value is returned
as an XML representation.

Optional arg HASH-BOUND specifies which kinds of the list objects,
alists or hash tables, are used as the targets of the conversion from
CFDictionary.  If HASH-BOUND is a negative integer or nil, always
generate alists.  If HASH-BOUND >= 0, generate an alist if the number
of keys in the dictionary is smaller than HASH-BOUND, and a hash table
otherwise.  */)
  (key, application, format, hash_bound)
     Lisp_Object key, application, format, hash_bound;
{
  CFStringRef app_id, key_str;
  CFPropertyListRef app_plist = NULL, plist;
  Lisp_Object result = Qnil, tmp;

  if (STRINGP (key))
    key = Fcons (key, Qnil);
  else
    {
      CHECK_CONS (key);
      for (tmp = key; CONSP (tmp); tmp = XCDR (tmp))
	CHECK_STRING_CAR (tmp);
      if (!NILP (tmp))
	wrong_type_argument (Qlistp, key);
    }
  if (!NILP (application))
    CHECK_STRING (application);
  CHECK_SYMBOL (format);
  if (!NILP (hash_bound))
    CHECK_NUMBER (hash_bound);

  BLOCK_INPUT;

  app_id = kCFPreferencesCurrentApplication;
  if (!NILP (application))
    {
      app_id = cfstring_create_with_string (application);
      if (app_id == NULL)
	goto out;
    }
  key_str = cfstring_create_with_string (XCAR (key));
  if (key_str == NULL)
    goto out;
  app_plist = CFPreferencesCopyAppValue (key_str, app_id);
  CFRelease (key_str);
  if (app_plist == NULL)
    goto out;

  plist = app_plist;
  for (key = XCDR (key); CONSP (key); key = XCDR (key))
    {
      if (CFGetTypeID (plist) != CFDictionaryGetTypeID ())
	break;
      key_str = cfstring_create_with_string (XCAR (key));
      if (key_str == NULL)
	goto out;
      plist = CFDictionaryGetValue (plist, key_str);
      CFRelease (key_str);
      if (plist == NULL)
	goto out;
    }

  if (NILP (key))
    if (EQ (format, Qxml))
      {
	CFDataRef data = CFPropertyListCreateXMLData (NULL, plist);
	if (data == NULL)
	  goto out;
	result = cfdata_to_lisp (data);
	CFRelease (data);
      }
    else
      result =
	cfproperty_list_to_lisp (plist, EQ (format, Qt),
				 NILP (hash_bound) ? -1 : XINT (hash_bound));

 out:
  if (app_plist)
    CFRelease (app_plist);
  CFRelease (app_id);

  UNBLOCK_INPUT;

  return result;
}


static CFStringEncoding
get_cfstring_encoding_from_lisp (obj)
     Lisp_Object obj;
{
  CFStringRef iana_name;
  CFStringEncoding encoding = kCFStringEncodingInvalidId;

  if (INTEGERP (obj))
    return XINT (obj);

  if (SYMBOLP (obj) && !NILP (obj) && !NILP (Fcoding_system_p (obj)))
    {
      Lisp_Object coding_spec, plist;

      coding_spec = Fget (obj, Qcoding_system);
      plist = XVECTOR (coding_spec)->contents[3];
      obj = Fplist_get (XVECTOR (coding_spec)->contents[3], Qmime_charset);
    }

  if (SYMBOLP (obj))
    obj = SYMBOL_NAME (obj);

  if (STRINGP (obj))
    {
      iana_name = cfstring_create_with_string (obj);
      if (iana_name)
	{
	  encoding = CFStringConvertIANACharSetNameToEncoding (iana_name);
	  CFRelease (iana_name);
	}
    }

  return encoding;
}

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1020
static CFStringRef
cfstring_create_normalized (str, symbol)
     CFStringRef str;
     Lisp_Object symbol;
{
  int form = -1;
  TextEncodingVariant variant;
  float initial_mag = 0.0;
  CFStringRef result = NULL;

  if (EQ (symbol, QNFD))
    form = kCFStringNormalizationFormD;
  else if (EQ (symbol, QNFKD))
    form = kCFStringNormalizationFormKD;
  else if (EQ (symbol, QNFC))
    form = kCFStringNormalizationFormC;
  else if (EQ (symbol, QNFKC))
    form = kCFStringNormalizationFormKC;
  else if (EQ (symbol, QHFS_plus_D))
    {
      variant = kUnicodeHFSPlusDecompVariant;
      initial_mag = 1.5;
    }
  else if (EQ (symbol, QHFS_plus_C))
    {
      variant = kUnicodeHFSPlusCompVariant;
      initial_mag = 1.0;
    }

  if (form >= 0)
    {
      CFMutableStringRef mut_str = CFStringCreateMutableCopy (NULL, 0, str);

      if (mut_str)
	{
	  CFStringNormalize (mut_str, form);
	  result = mut_str;
	}
    }
  else if (initial_mag > 0.0)
    {
      UnicodeToTextInfo uni = NULL;
      UnicodeMapping map;
      CFIndex length;
      UniChar *in_text, *buffer = NULL, *out_buf = NULL;
      OSErr err = noErr;
      ByteCount out_read, out_size, out_len;

      map.unicodeEncoding = CreateTextEncoding (kTextEncodingUnicodeDefault,
						kUnicodeNoSubset,
						kTextEncodingDefaultFormat);
      map.otherEncoding = CreateTextEncoding (kTextEncodingUnicodeDefault,
					      variant,
					      kTextEncodingDefaultFormat);
      map.mappingVersion = kUnicodeUseLatestMapping;

      length = CFStringGetLength (str);
      out_size = (int)((float)length * initial_mag) * sizeof (UniChar);
      if (out_size < 32)
	out_size = 32;

      in_text = (UniChar *)CFStringGetCharactersPtr (str);
      if (in_text == NULL)
	{
	  buffer = xmalloc (sizeof (UniChar) * length);
	  if (buffer)
	    {
	      CFStringGetCharacters (str, CFRangeMake (0, length), buffer);
	      in_text = buffer;
	    }
	}

      if (in_text)
	err = CreateUnicodeToTextInfo(&map, &uni);
      while (err == noErr)
	{
	  out_buf = xmalloc (out_size);
	  if (out_buf == NULL)
	    err = mFulErr;
	  else
	    err = ConvertFromUnicodeToText (uni, length * sizeof (UniChar),
					    in_text,
					    kUnicodeDefaultDirectionMask,
					    0, NULL, NULL, NULL,
					    out_size, &out_read, &out_len,
					    out_buf);
	  if (err == noErr && out_read < length * sizeof (UniChar))
	    {
	      xfree (out_buf);
	      out_size += length;
	    }
	  else
	    break;
	}
      if (err == noErr)
	result = CFStringCreateWithCharacters (NULL, out_buf,
					       out_len / sizeof (UniChar));
      if (uni)
	DisposeUnicodeToTextInfo (&uni);
      if (out_buf)
	xfree (out_buf);
      if (buffer)
	xfree (buffer);
    }
  else
    {
      result = str;
      CFRetain (result);
    }

  return result;
}
#endif

DEFUN ("mac-code-convert-string", Fmac_code_convert_string, Smac_code_convert_string, 3, 4, 0,
       doc: /* Convert STRING from SOURCE encoding to TARGET encoding.
The conversion is performed using the converter provided by the system.
Each encoding is specified by either a coding system symbol, a mime
charset string, or an integer as a CFStringEncoding value.
On Mac OS X 10.2 and later, you can do Unicode Normalization by
specifying the optional argument NORMALIZATION-FORM with a symbol NFD,
NFKD, NFC, NFKC, HFS+D, or HFS+C.
On successful conversion, returns the result string, else returns
nil.  */)
  (string, source, target, normalization_form)
     Lisp_Object string, source, target, normalization_form;
{
  Lisp_Object result = Qnil;
  CFStringEncoding src_encoding, tgt_encoding;
  CFStringRef str = NULL;
  CFDataRef data = NULL;

  CHECK_STRING (string);
  if (!INTEGERP (source) && !STRINGP (source))
    CHECK_SYMBOL (source);
  if (!INTEGERP (target) && !STRINGP (target))
    CHECK_SYMBOL (target);
  CHECK_SYMBOL (normalization_form);

  BLOCK_INPUT;

  src_encoding = get_cfstring_encoding_from_lisp (source);
  tgt_encoding = get_cfstring_encoding_from_lisp (target);

  /* We really want string_to_unibyte, but since it doesn't exist yet, we
     use string_as_unibyte which works as well, except for the fact that
     it's too permissive (it doesn't check that the multibyte string only
     contain single-byte chars).  */
  string = Fstring_as_unibyte (string);
  if (src_encoding != kCFStringEncodingInvalidId
      && tgt_encoding != kCFStringEncodingInvalidId)
    str = CFStringCreateWithBytes (NULL, SDATA (string), SBYTES (string),
				   src_encoding, true);
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1020
  if (str)
    {
      CFStringRef saved_str = str;

      str = cfstring_create_normalized (saved_str, normalization_form);
      CFRelease (saved_str);
    }
#endif
  if (str)
    {
      data = CFStringCreateExternalRepresentation (NULL, str,
						   tgt_encoding, '\0');
      CFRelease (str);
    }
  if (data)
    {
      result = cfdata_to_lisp (data);
      CFRelease (data);
    }

  UNBLOCK_INPUT;

  return result;
}
#endif	/* TARGET_API_MAC_CARBON */


DEFUN ("mac-clear-font-name-table", Fmac_clear_font_name_table, Smac_clear_font_name_table, 0, 0, 0,
       doc: /* Clear the font name table.  */)
  ()
{
  check_mac ();
  mac_clear_font_name_table ();
  return Qnil;
}

#ifdef MAC_OSX
#undef select

extern int inhibit_window_system;
extern int noninteractive;

/* Unlike in X11, window events in Carbon do not come from sockets.
   So we cannot simply use `select' to monitor two kinds of inputs:
   window events and process outputs.  We emulate such functionality
   by regarding fd 0 as the window event channel and simultaneously
   monitoring both kinds of input channels.  It is implemented by
   dividing into some cases:
   1. The window event channel is not involved.
      -> Use `select'.
   2. Sockets are not involved.
      -> Use ReceiveNextEvent.
   3. [If SELECT_USE_CFSOCKET is defined]
      Only the window event channel and socket read channels are
      involved, and timeout is not too short (greater than
      SELECT_TIMEOUT_THRESHHOLD_RUNLOOP seconds).
      -> Create CFSocket for each socket and add it into the current
         event RunLoop so that an `ready-to-read' event can be posted
         to the event queue that is also used for window events.  Then
         ReceiveNextEvent can wait for both kinds of inputs.
   4. Otherwise.
      -> Periodically poll the window input channel while repeatedly
         executing `select' with a short timeout
         (SELECT_POLLING_PERIOD_USEC microseconds).  */

#define SELECT_POLLING_PERIOD_USEC 20000
#ifdef SELECT_USE_CFSOCKET
#define SELECT_TIMEOUT_THRESHOLD_RUNLOOP 0.2
#define EVENT_CLASS_SOCK 'Sock'

static void
socket_callback (s, type, address, data, info)
     CFSocketRef s;
     CFSocketCallBackType type;
     CFDataRef address;
     const void *data;
     void *info;
{
  EventRef event;

  CreateEvent (NULL, EVENT_CLASS_SOCK, 0, 0, kEventAttributeNone, &event);
  PostEventToQueue (GetCurrentEventQueue (), event, kEventPriorityStandard);
  ReleaseEvent (event);
}
#endif	/* SELECT_USE_CFSOCKET */

static int
select_and_poll_event (n, rfds, wfds, efds, timeout)
     int n;
     SELECT_TYPE *rfds;
     SELECT_TYPE *wfds;
     SELECT_TYPE *efds;
     struct timeval *timeout;
{
  int r;
  OSErr err;

  r = select (n, rfds, wfds, efds, timeout);
  if (r != -1)
    {
      BLOCK_INPUT;
      err = ReceiveNextEvent (0, NULL, kEventDurationNoWait,
			      kEventLeaveInQueue, NULL);
      UNBLOCK_INPUT;
      if (err == noErr)
	{
	  FD_SET (0, rfds);
	  r++;
	}
    }
  return r;
}

#if MAC_OS_X_VERSION_MAX_ALLOWED < 1020
#undef SELECT_INVALIDATE_CFSOCKET
#endif

int
sys_select (n, rfds, wfds, efds, timeout)
     int n;
     SELECT_TYPE *rfds;
     SELECT_TYPE *wfds;
     SELECT_TYPE *efds;
     struct timeval *timeout;
{
  OSErr err;
  int i, r;
  EMACS_TIME select_timeout;

  if (inhibit_window_system || noninteractive
      || rfds == NULL || !FD_ISSET (0, rfds))
    return select (n, rfds, wfds, efds, timeout);

  FD_CLR (0, rfds);

  if (wfds == NULL && efds == NULL)
    {
      int nsocks = 0;
      SELECT_TYPE orfds = *rfds;

      EventTimeout timeout_sec =
	(timeout
	 ? (EMACS_SECS (*timeout) * kEventDurationSecond
	    + EMACS_USECS (*timeout) * kEventDurationMicrosecond)
	 : kEventDurationForever);

      for (i = 1; i < n; i++)
	if (FD_ISSET (i, rfds))
	  nsocks++;

      if (nsocks == 0)
	{
	  BLOCK_INPUT;
	  err = ReceiveNextEvent (0, NULL, timeout_sec,
				  kEventLeaveInQueue, NULL);
	  UNBLOCK_INPUT;
	  if (err == noErr)
	    {
	      FD_SET (0, rfds);
	      return 1;
	    }
	  else
	    return 0;
	}

      /* Avoid initial overhead of RunLoop setup for the case that
	 some input is already available.  */
      EMACS_SET_SECS_USECS (select_timeout, 0, 0);
      r = select_and_poll_event (n, rfds, wfds, efds, &select_timeout);
      if (r != 0 || timeout_sec == 0.0)
	return r;

      *rfds = orfds;

#ifdef SELECT_USE_CFSOCKET
      if (timeout_sec > 0 && timeout_sec <= SELECT_TIMEOUT_THRESHOLD_RUNLOOP)
	goto poll_periodically;

      {
	CFRunLoopRef runloop =
	  (CFRunLoopRef) GetCFRunLoopFromEventLoop (GetCurrentEventLoop ());
	EventTypeSpec specs[] = {{EVENT_CLASS_SOCK, 0}};
#ifdef SELECT_INVALIDATE_CFSOCKET
	CFSocketRef *shead, *s;
#else
	CFRunLoopSourceRef *shead, *s;
#endif

	BLOCK_INPUT;

#ifdef SELECT_INVALIDATE_CFSOCKET
	shead = xmalloc (sizeof (CFSocketRef) * nsocks);
#else
	shead = xmalloc (sizeof (CFRunLoopSourceRef) * nsocks);
#endif
	s = shead;
	for (i = 1; i < n; i++)
	  if (FD_ISSET (i, rfds))
	    {
	      CFSocketRef socket =
		CFSocketCreateWithNative (NULL, i, kCFSocketReadCallBack,
					  socket_callback, NULL);
	      CFRunLoopSourceRef source =
		CFSocketCreateRunLoopSource (NULL, socket, 0);

#ifdef SELECT_INVALIDATE_CFSOCKET
	      CFSocketSetSocketFlags (socket, 0);
#endif
	      CFRunLoopAddSource (runloop, source, kCFRunLoopDefaultMode);
#ifdef SELECT_INVALIDATE_CFSOCKET
	      CFRelease (source);
	      *s = socket;
#else
	      CFRelease (socket);
	      *s = source;
#endif
	      s++;
	    }

	err = ReceiveNextEvent (0, NULL, timeout_sec, kEventLeaveInQueue, NULL);

	do
	  {
	    --s;
#ifdef SELECT_INVALIDATE_CFSOCKET
	    CFSocketInvalidate (*s);
#else
	    CFRunLoopRemoveSource (runloop, *s, kCFRunLoopDefaultMode);
#endif
	    CFRelease (*s);
	  }
	while (s != shead);

	xfree (shead);

	if (err)
	  {
	    FD_ZERO (rfds);
	    r = 0;
	  }
	else
	  {
	    FlushEventsMatchingListFromQueue (GetCurrentEventQueue (),
					      GetEventTypeCount (specs),
					      specs);
	    EMACS_SET_SECS_USECS (select_timeout, 0, 0);
	    r = select_and_poll_event (n, rfds, wfds, efds, &select_timeout);
	  }

	UNBLOCK_INPUT;

	return r;
      }
#endif	/* SELECT_USE_CFSOCKET */
    }

 poll_periodically:
  {
    EMACS_TIME end_time, now, remaining_time;
    SELECT_TYPE orfds = *rfds, owfds, oefds;

    if (wfds)
      owfds = *wfds;
    if (efds)
      oefds = *efds;
    if (timeout)
      {
	remaining_time = *timeout;
	EMACS_GET_TIME (now);
	EMACS_ADD_TIME (end_time, now, remaining_time);
      }

    do
      {
	EMACS_SET_SECS_USECS (select_timeout, 0, SELECT_POLLING_PERIOD_USEC);
	if (timeout && EMACS_TIME_LT (remaining_time, select_timeout))
	  select_timeout = remaining_time;
	r = select_and_poll_event (n, rfds, wfds, efds, &select_timeout);
	if (r != 0)
	  return r;

	*rfds = orfds;
	if (wfds)
	  *wfds = owfds;
	if (efds)
	  *efds = oefds;

	if (timeout)
	  {
	    EMACS_GET_TIME (now);
	    EMACS_SUB_TIME (remaining_time, end_time, now);
	  }
      }
    while (!timeout || EMACS_TIME_LT (now, end_time));

    FD_ZERO (rfds);
    if (wfds)
      FD_ZERO (wfds);
    if (efds)
      FD_ZERO (efds);
    return 0;
  }
}

/* Set up environment variables so that Emacs can correctly find its
   support files when packaged as an application bundle.  Directories
   placed in /usr/local/share/emacs/<emacs-version>/, /usr/local/bin,
   and /usr/local/libexec/emacs/<emacs-version>/<system-configuration>
   by `make install' by default can instead be placed in
   .../Emacs.app/Contents/Resources/ and
   .../Emacs.app/Contents/MacOS/.  Each of these environment variables
   is changed only if it is not already set.  Presumably if the user
   sets an environment variable, he will want to use files in his path
   instead of ones in the application bundle.  */
void
init_mac_osx_environment ()
{
  CFBundleRef bundle;
  CFURLRef bundleURL;
  CFStringRef cf_app_bundle_pathname;
  int app_bundle_pathname_len;
  char *app_bundle_pathname;
  char *p, *q;
  struct stat st;

  /* Fetch the pathname of the application bundle as a C string into
     app_bundle_pathname.  */

  bundle = CFBundleGetMainBundle ();
  if (!bundle)
    return;

  bundleURL = CFBundleCopyBundleURL (bundle);
  if (!bundleURL)
    return;

  cf_app_bundle_pathname = CFURLCopyFileSystemPath (bundleURL,
						    kCFURLPOSIXPathStyle);
  app_bundle_pathname_len = CFStringGetLength (cf_app_bundle_pathname);
  app_bundle_pathname = (char *) alloca (app_bundle_pathname_len + 1);

  if (!CFStringGetCString (cf_app_bundle_pathname,
			   app_bundle_pathname,
			   app_bundle_pathname_len + 1,
			   kCFStringEncodingISOLatin1))
    {
      CFRelease (cf_app_bundle_pathname);
      return;
    }

  CFRelease (cf_app_bundle_pathname);

  /* P should have sufficient room for the pathname of the bundle plus
     the subpath in it leading to the respective directories.  Q
     should have three times that much room because EMACSLOADPATH can
     have the value "<path to lisp dir>:<path to leim dir>:<path to
     site-lisp dir>".  */
  p = (char *) alloca (app_bundle_pathname_len + 50);
  q = (char *) alloca (3 * app_bundle_pathname_len + 150);
  if (!getenv ("EMACSLOADPATH"))
    {
      q[0] = '\0';

      strcpy (p, app_bundle_pathname);
      strcat (p, "/Contents/Resources/lisp");
      if (stat (p, &st) == 0 && (st.st_mode & S_IFMT) == S_IFDIR)
	strcat (q, p);

      strcpy (p, app_bundle_pathname);
      strcat (p, "/Contents/Resources/leim");
      if (stat (p, &st) == 0 && (st.st_mode & S_IFMT) == S_IFDIR)
	{
	  if (q[0] != '\0')
	    strcat (q, ":");
	  strcat (q, p);
	}

      strcpy (p, app_bundle_pathname);
      strcat (p, "/Contents/Resources/site-lisp");
      if (stat (p, &st) == 0 && (st.st_mode & S_IFMT) == S_IFDIR)
	{
	  if (q[0] != '\0')
	    strcat (q, ":");
	  strcat (q, p);
	}

      if (q[0] != '\0')
	setenv ("EMACSLOADPATH", q, 1);
    }

  if (!getenv ("EMACSPATH"))
    {
      q[0] = '\0';

      strcpy (p, app_bundle_pathname);
      strcat (p, "/Contents/MacOS/libexec");
      if (stat (p, &st) == 0 && (st.st_mode & S_IFMT) == S_IFDIR)
	strcat (q, p);

      strcpy (p, app_bundle_pathname);
      strcat (p, "/Contents/MacOS/bin");
      if (stat (p, &st) == 0 && (st.st_mode & S_IFMT) == S_IFDIR)
	{
	  if (q[0] != '\0')
	    strcat (q, ":");
	  strcat (q, p);
	}

      if (q[0] != '\0')
	setenv ("EMACSPATH", q, 1);
    }

  if (!getenv ("EMACSDATA"))
    {
      strcpy (p, app_bundle_pathname);
      strcat (p, "/Contents/Resources/etc");
      if (stat (p, &st) == 0 && (st.st_mode & S_IFMT) == S_IFDIR)
	setenv ("EMACSDATA", p, 1);
    }

  if (!getenv ("EMACSDOC"))
    {
      strcpy (p, app_bundle_pathname);
      strcat (p, "/Contents/Resources/etc");
      if (stat (p, &st) == 0 && (st.st_mode & S_IFMT) == S_IFDIR)
	setenv ("EMACSDOC", p, 1);
    }

  if (!getenv ("INFOPATH"))
    {
      strcpy (p, app_bundle_pathname);
      strcat (p, "/Contents/Resources/info");
      if (stat (p, &st) == 0 && (st.st_mode & S_IFMT) == S_IFDIR)
	setenv ("INFOPATH", p, 1);
    }
}
#endif /* MAC_OSX */


static Lisp_Object
mac_get_system_locale ()
{
  OSErr err;
  LangCode lang;
  RegionCode region;
  LocaleRef locale;
  Str255 str;

  lang = GetScriptVariable (smSystemScript, smScriptLang);
  region = GetScriptManagerVariable (smRegionCode);
  err = LocaleRefFromLangOrRegionCode (lang, region, &locale);
  if (err == noErr)
    err = LocaleRefGetPartString (locale, kLocaleAllPartsMask,
				  sizeof (str), str);
  if (err == noErr)
    return build_string (str);
  else
    return Qnil;
}


void
syms_of_mac ()
{
#if TARGET_API_MAC_CARBON
  Qstring  = intern ("string");		staticpro (&Qstring);
  Qnumber  = intern ("number");		staticpro (&Qnumber);
  Qboolean = intern ("boolean");	staticpro (&Qboolean);
  Qdate	   = intern ("date");		staticpro (&Qdate);
  Qdata    = intern ("data");		staticpro (&Qdata);
  Qarray   = intern ("array");		staticpro (&Qarray);
  Qdictionary = intern ("dictionary");	staticpro (&Qdictionary);

  Qxml = intern ("xml");
  staticpro (&Qxml);

  Qmime_charset = intern ("mime-charset");
  staticpro (&Qmime_charset);

  QNFD  = intern ("NFD");		staticpro (&QNFD);
  QNFKD = intern ("NFKD");		staticpro (&QNFKD);
  QNFC  = intern ("NFC");		staticpro (&QNFC);
  QNFKC = intern ("NFKC");		staticpro (&QNFKC);
  QHFS_plus_D = intern ("HFS+D");	staticpro (&QHFS_plus_D);
  QHFS_plus_C = intern ("HFS+C");	staticpro (&QHFS_plus_C);
#endif

#if TARGET_API_MAC_CARBON
  defsubr (&Smac_get_preference);
  defsubr (&Smac_code_convert_string);
#endif
  defsubr (&Smac_clear_font_name_table);

  defsubr (&Smac_set_file_creator);
  defsubr (&Smac_set_file_type);
  defsubr (&Smac_get_file_creator);
  defsubr (&Smac_get_file_type);
  defsubr (&Sdo_applescript);
  defsubr (&Smac_file_name_to_posix);
  defsubr (&Sposix_file_name_to_mac);

  DEFVAR_INT ("mac-system-script-code", &mac_system_script_code,
    doc: /* The system script code.  */);
  mac_system_script_code = (ScriptCode) GetScriptManagerVariable (smSysScript);

  DEFVAR_LISP ("mac-system-locale", &Vmac_system_locale,
    doc: /* The system locale identifier string.
This is not a POSIX locale ID, but an ICU locale ID.  So encoding
information is not included.  */);
  Vmac_system_locale = mac_get_system_locale ();
}

/* arch-tag: 29d30c1f-0c6b-4f88-8a6d-0558d7f9dbff
   (do not change this comment) */
