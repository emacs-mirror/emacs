/* Interface to libxml2.
   Copyright (C) 2010 Free Software Foundation, Inc.

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

#include <setjmp.h>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/HTMLparser.h>

#include "lisp.h"
#include "buffer.h"

Lisp_Object make_dom (xmlNode *node)
{
  if (node->type == XML_ELEMENT_NODE)
    {
      Lisp_Object result = Fcons (intern (node->name), Qnil);
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
	      plist = Fcons (Fcons (intern (property->name),
				    build_string (property->children->content)),
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
  else if (node->type == XML_TEXT_NODE)
    {
      if (node->content)
	return build_string (node->content);
      else
	return Qnil;
    }
  else
    return Qnil;
}

static Lisp_Object
parse_string (Lisp_Object string, Lisp_Object base_url, int htmlp)
{
  xmlDoc *doc;
  xmlNode *node;
  Lisp_Object result = Qnil;
  int ibeg, iend;
  char *burl = "";

  LIBXML_TEST_VERSION;

  CHECK_STRING (string);

  if (! NILP (base_url))
    {
      CHECK_STRING (base_url);
      burl = SDATA (base_url);
    }

  doc = htmlp
    ? htmlReadMemory (SDATA (string), SBYTES (string), burl, "utf-8",
		      HTML_PARSE_RECOVER|HTML_PARSE_NONET|
		      HTML_PARSE_NOWARNING|HTML_PARSE_NOERROR)
    : xmlReadMemory (SDATA (string), SBYTES (string), burl, "utf-8",
		     XML_PARSE_NONET|XML_PARSE_NOWARNING|
		     XML_PARSE_NOERROR);

  if (doc != NULL)
    {
      node = xmlDocGetRootElement (doc);
      if (node != NULL)
	result = make_dom (node);
      xmlFreeDoc (doc);
      xmlCleanupParser ();
    }

  return result;
}

DEFUN ("xml-parse-html-string-internal", Fxml_parse_html_string_internal,
       Sxml_parse_html_string_internal,
       1, 2, 0,
       doc: /* Parse STRING as an HTML document and return the parse tree.
If BASE-URL is non-nil, it is used to expand relative URLs.  */)
  (Lisp_Object string, Lisp_Object base_url)
{
  return parse_string (string, base_url, 1);
}

DEFUN ("xml-parse-string-internal", Fxml_parse_string_internal,
       Sxml_parse_string_internal,
       1, 2, 0,
       doc: /* Parse STRING as an XML document and return the parse tree.
If BASE-URL is non-nil, it is used to expand relative URLs.  */)
  (Lisp_Object string, Lisp_Object base_url)
{
  return parse_string (string, base_url, 0);
}


/***********************************************************************
			    Initialization
 ***********************************************************************/
void
syms_of_xml (void)
{
  defsubr (&Sxml_parse_html_string_internal);
  defsubr (&Sxml_parse_string_internal);
}

#endif /* HAVE_LIBXML2 */
