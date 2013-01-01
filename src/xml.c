/* Interface to libxml2.
   Copyright (C) 2010-2013 Free Software Foundation, Inc.

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


static Lisp_Object Qlibxml2_dll;

#ifdef WINDOWSNT

#include <windows.h>
#include "w32.h"

/* Macro for defining functions that will be loaded from the libxml2 DLL.  */
#define DEF_XML2_FN(rettype,func,args) static rettype (FAR CDECL *fn_##func)args

/* Macro for loading libxml2 functions from the library.  */
#define LOAD_XML2_FN(lib,func) {					\
    fn_##func = (void *) GetProcAddress (lib, #func);			\
    if (!fn_##func) goto bad_library;					\
  }

DEF_XML2_FN (htmlDocPtr, htmlReadMemory,
	     (const char *, int, const char *, const char *, int));
DEF_XML2_FN (xmlDocPtr, xmlReadMemory,
	     (const char *, int, const char *, const char *, int));
DEF_XML2_FN (xmlNodePtr, xmlDocGetRootElement, (xmlDocPtr));
DEF_XML2_FN (void, xmlFreeDoc, (xmlDocPtr));
DEF_XML2_FN (void, xmlCleanupParser, (void));
DEF_XML2_FN (void, xmlCheckVersion, (int));

static int
libxml2_loaded_p (void)
{
  Lisp_Object found = Fassq (Qlibxml2_dll, Vlibrary_cache);

  if (CONSP (found))
    return EQ (XCDR (found), Qt) ? 1 : 0;
  return 0;
}

#else  /* !WINDOWSNT */

#define fn_htmlReadMemory       htmlReadMemory
#define fn_xmlReadMemory        xmlReadMemory
#define fn_xmlDocGetRootElement xmlDocGetRootElement
#define fn_xmlFreeDoc           xmlFreeDoc
#define fn_xmlCleanupParser     xmlCleanupParser
#define fn_xmlCheckVersion      xmlCheckVersion

static int
libxml2_loaded_p (void)
{
  return 1;
}

#endif	/* !WINDOWSNT */

static int
init_libxml2_functions (void)
{
#ifdef WINDOWSNT
  if (libxml2_loaded_p ())
    return 1;
  else
    {
      HMODULE library;

      if (!(library = w32_delayed_load (Qlibxml2_dll)))
	{
	  message ("%s", "libxml2 library not found");
	  return 0;
	}

      /* LOAD_XML2_FN jumps to bad_library if it fails to find the
	 named function.  */
      LOAD_XML2_FN (library, htmlReadMemory);
      LOAD_XML2_FN (library, xmlReadMemory);
      LOAD_XML2_FN (library, xmlDocGetRootElement);
      LOAD_XML2_FN (library, xmlFreeDoc);
      LOAD_XML2_FN (library, xmlCleanupParser);
      LOAD_XML2_FN (library, xmlCheckVersion);

      Vlibrary_cache = Fcons (Fcons (Qlibxml2_dll, Qt), Vlibrary_cache);
      return 1;
    }

 bad_library:
  Vlibrary_cache = Fcons (Fcons (Qlibxml2_dll, Qnil), Vlibrary_cache);

  return 0;
#else  /* !WINDOWSNT */
  return 1;
#endif	/* !WINDOWSNT */
}

static Lisp_Object
make_dom (xmlNode *node)
{
  if (node->type == XML_ELEMENT_NODE)
    {
      Lisp_Object result = Fcons (intern ((char *) node->name), Qnil);
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
parse_region (Lisp_Object start, Lisp_Object end, Lisp_Object base_url, int htmlp)
{
  xmlDoc *doc;
  Lisp_Object result = Qnil;
  const char *burl = "";
  ptrdiff_t bytes;
  ptrdiff_t istart, iend;

  fn_xmlCheckVersion (LIBXML_VERSION);

  validate_region (&start, &end);

  istart = XINT (start);
  iend = XINT (end);

  if (istart < GPT && GPT < iend)
    move_gap (iend);

  if (! NILP (base_url))
    {
      CHECK_STRING (base_url);
      burl = SSDATA (base_url);
    }

  bytes = CHAR_TO_BYTE (iend) - CHAR_TO_BYTE (istart);

  if (htmlp)
    doc = fn_htmlReadMemory ((char *) BYTE_POS_ADDR (CHAR_TO_BYTE (istart)),
			     bytes, burl, "utf-8",
			     HTML_PARSE_RECOVER|HTML_PARSE_NONET|
			     HTML_PARSE_NOWARNING|HTML_PARSE_NOERROR|
			     HTML_PARSE_NOBLANKS);
  else
    doc = fn_xmlReadMemory ((char *) BYTE_POS_ADDR (CHAR_TO_BYTE (istart)),
			    bytes, burl, "utf-8",
			    XML_PARSE_NONET|XML_PARSE_NOWARNING|
			    XML_PARSE_NOBLANKS |XML_PARSE_NOERROR);

  if (doc != NULL)
    {
      /* If the document is just comments, then this should get us the
	 nodes anyway. */
      xmlNode *n = doc->children->next;
      Lisp_Object r = Qnil;

      while (n) {
	if (!NILP (r))
	  result = Fcons (r, result);
	r = make_dom (n);
	n = n->next;
      }

      if (NILP (result)) {
	/* The document isn't just comments, so get the tree the
	   proper way. */
	xmlNode *node = fn_xmlDocGetRootElement (doc);
	if (node != NULL)
	  result = make_dom (node);
      } else
	result = Fcons (intern ("top"),
			Fcons (Qnil, Fnreverse (Fcons (r, result))));

      fn_xmlFreeDoc (doc);
    }

  return result;
}

void
xml_cleanup_parser (void)
{
  if (libxml2_loaded_p ())
    fn_xmlCleanupParser ();
}

DEFUN ("libxml-parse-html-region", Flibxml_parse_html_region,
       Slibxml_parse_html_region,
       2, 3, 0,
       doc: /* Parse the region as an HTML document and return the parse tree.
If BASE-URL is non-nil, it is used to expand relative URLs.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object base_url)
{
  if (init_libxml2_functions ())
    return parse_region (start, end, base_url, 1);
  return Qnil;
}

DEFUN ("libxml-parse-xml-region", Flibxml_parse_xml_region,
       Slibxml_parse_xml_region,
       2, 3, 0,
       doc: /* Parse the region as an XML document and return the parse tree.
If BASE-URL is non-nil, it is used to expand relative URLs.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object base_url)
{
  if (init_libxml2_functions ())
    return parse_region (start, end, base_url, 0);
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

  DEFSYM (Qlibxml2_dll, "libxml2");
}

#endif /* HAVE_LIBXML2 */
