/* Interface to libxml2.
   Copyright (C) 2010-2015 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

#ifdef HAVE_LIBXML2

#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/HTMLparser.h>

#include "lisp.h"
#include "character.h"
#include "buffer.h"


#ifdef WINDOWSNT

# include <windows.h>
# include "w32.h"

DEF_DLL_FN (htmlDocPtr, htmlReadMemory,
	     (const char *, int, const char *, const char *, int));
DEF_DLL_FN (xmlDocPtr, xmlReadMemory,
	     (const char *, int, const char *, const char *, int));
DEF_DLL_FN (xmlNodePtr, xmlDocGetRootElement, (xmlDocPtr));
DEF_DLL_FN (void, xmlFreeDoc, (xmlDocPtr));
DEF_DLL_FN (void, xmlCleanupParser, (void));
DEF_DLL_FN (void, xmlCheckVersion, (int));

static bool
libxml2_loaded_p (void)
{
  Lisp_Object found = Fassq (Qlibxml2_dll, Vlibrary_cache);

  return CONSP (found) && EQ (XCDR (found), Qt);
}

# undef htmlReadMemory
# undef xmlCheckVersion
# undef xmlCleanupParser
# undef xmlDocGetRootElement
# undef xmlFreeDoc
# undef xmlReadMemory

# define htmlReadMemory fn_htmlReadMemory
# define xmlCheckVersion fn_xmlCheckVersion
# define xmlCleanupParser fn_xmlCleanupParser
# define xmlDocGetRootElement fn_xmlDocGetRootElement
# define xmlFreeDoc fn_xmlFreeDoc
# define xmlReadMemory fn_xmlReadMemory

static bool
load_dll_functions (HMODULE library)
{
  LOAD_DLL_FN (library, htmlReadMemory);
  LOAD_DLL_FN (library, xmlReadMemory);
  LOAD_DLL_FN (library, xmlDocGetRootElement);
  LOAD_DLL_FN (library, xmlFreeDoc);
  LOAD_DLL_FN (library, xmlCleanupParser);
  LOAD_DLL_FN (library, xmlCheckVersion);
  return true;
}

#else  /* !WINDOWSNT */

static bool
libxml2_loaded_p (void)
{
  return true;
}

#endif	/* !WINDOWSNT */

static bool
init_libxml2_functions (void)
{
#ifdef WINDOWSNT
  if (libxml2_loaded_p ())
    return true;
  else
    {
      HMODULE library;

      if (!(library = w32_delayed_load (Qlibxml2_dll)))
	{
	  message1 ("libxml2 library not found");
	  return false;
	}

      if (! load_dll_functions (library))
	goto bad_library;

      Vlibrary_cache = Fcons (Fcons (Qlibxml2_dll, Qt), Vlibrary_cache);
      return true;
    }

 bad_library:
  Vlibrary_cache = Fcons (Fcons (Qlibxml2_dll, Qnil), Vlibrary_cache);

  return false;
#else  /* !WINDOWSNT */
  return true;
#endif	/* !WINDOWSNT */
}

static Lisp_Object
make_dom (xmlNode *node)
{
  if (node->type == XML_ELEMENT_NODE)
    {
      Lisp_Object result = list1 (intern ((char *) node->name));
      xmlNode *child;
      xmlAttr *property;
      Lisp_Object plist = Qnil;

      /* First add the attributes. */
      property = node->properties;
      while (property != NULL)
	{
	  if (property->children &&
	      property->children->content)
	    {
	      char *content = (char *) property->children->content;
	      plist = Fcons (Fcons (intern ((char *) property->name),
				    build_string (content)),
			     plist);
	    }
	  property = property->next;
	}
      result = Fcons (Fnreverse (plist), result);

      /* Then add the children of the node. */
      child = node->children;
      while (child != NULL)
	{
	  result = Fcons (make_dom (child), result);
	  child = child->next;
	}

      return Fnreverse (result);
    }
  else if (node->type == XML_TEXT_NODE || node->type == XML_CDATA_SECTION_NODE)
    {
      if (node->content)
	return build_string ((char *) node->content);
      else
	return Qnil;
    }
  else if (node->type == XML_COMMENT_NODE)
    {
      if (node->content)
	return list3 (intern ("comment"), Qnil,
		      build_string ((char *) node->content));
      else
	return Qnil;
    }
  else
    return Qnil;
}

static Lisp_Object
parse_region (Lisp_Object start, Lisp_Object end, Lisp_Object base_url,
	      Lisp_Object discard_comments, bool htmlp)
{
  xmlDoc *doc;
  Lisp_Object result = Qnil;
  const char *burl = "";
  ptrdiff_t istart, iend, istart_byte, iend_byte;

  xmlCheckVersion (LIBXML_VERSION);

  validate_region (&start, &end);

  istart = XINT (start);
  iend = XINT (end);
  istart_byte = CHAR_TO_BYTE (istart);
  iend_byte = CHAR_TO_BYTE (iend);

  if (istart < GPT && GPT < iend)
    move_gap_both (iend, iend_byte);

  if (! NILP (base_url))
    {
      CHECK_STRING (base_url);
      burl = SSDATA (base_url);
    }

  if (htmlp)
    doc = htmlReadMemory ((char *) BYTE_POS_ADDR (istart_byte),
			  iend_byte - istart_byte, burl, "utf-8",
			  HTML_PARSE_RECOVER|HTML_PARSE_NONET|
			  HTML_PARSE_NOWARNING|HTML_PARSE_NOERROR|
			  HTML_PARSE_NOBLANKS);
  else
    doc = xmlReadMemory ((char *) BYTE_POS_ADDR (istart_byte),
			 iend_byte - istart_byte, burl, "utf-8",
			 XML_PARSE_NONET|XML_PARSE_NOWARNING|
			 XML_PARSE_NOBLANKS |XML_PARSE_NOERROR);

  if (doc != NULL)
    {
      Lisp_Object r = Qnil;
      if (NILP(discard_comments))
        {
          /* If the document has toplevel comments, then this should
             get us the nodes and the comments. */
          xmlNode *n = doc->children;

          while (n) {
            if (!NILP (r))
              result = Fcons (r, result);
            r = make_dom (n);
            n = n->next;
          }
        }

      if (NILP (result)) {
	/* The document doesn't have toplevel comments or we discarded
	   them.  Get the tree the proper way. */
	xmlNode *node = xmlDocGetRootElement (doc);
	if (node != NULL)
	  result = make_dom (node);
      } else
	result = Fcons (Qtop, Fcons (Qnil, Fnreverse (Fcons (r, result))));

      xmlFreeDoc (doc);
    }

  return result;
}

void
xml_cleanup_parser (void)
{
  if (libxml2_loaded_p ())
    xmlCleanupParser ();
}

DEFUN ("libxml-parse-html-region", Flibxml_parse_html_region,
       Slibxml_parse_html_region,
       2, 4, 0,
       doc: /* Parse the region as an HTML document and return the parse tree.
If BASE-URL is non-nil, it is used to expand relative URLs.
If DISCARD-COMMENTS is non-nil, all HTML comments are discarded. */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object base_url, Lisp_Object discard_comments)
{
  if (init_libxml2_functions ())
    return parse_region (start, end, base_url, discard_comments, true);
  return Qnil;
}

DEFUN ("libxml-parse-xml-region", Flibxml_parse_xml_region,
       Slibxml_parse_xml_region,
       2, 4, 0,
       doc: /* Parse the region as an XML document and return the parse tree.
If BASE-URL is non-nil, it is used to expand relative URLs.
If DISCARD-COMMENTS is non-nil, all HTML comments are discarded. */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object base_url, Lisp_Object discard_comments)
{
  if (init_libxml2_functions ())
    return parse_region (start, end, base_url, discard_comments, false);
  return Qnil;
}


/***********************************************************************
			    Initialization
 ***********************************************************************/
void
syms_of_xml (void)
{
  defsubr (&Slibxml_parse_html_region);
  defsubr (&Slibxml_parse_xml_region);
}

#endif /* HAVE_LIBXML2 */
