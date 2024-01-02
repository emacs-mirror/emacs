/* Header file for the tree-sitter integration.

Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

#ifndef EMACS_TREESIT_H
#define EMACS_TREESIT_H

#include <config.h>

#ifdef HAVE_TREE_SITTER

#include <tree_sitter/api.h>
#include "lisp.h"

INLINE_HEADER_BEGIN

/* A wrapper for a tree-sitter parser, but also contains a parse tree
   and other goodies for convenience.  */
struct Lisp_TS_Parser
{
  union vectorlike_header header;
  /* A symbol representing the language this parser uses.  See the
     manual for more explanation.  */
  Lisp_Object language_symbol;
  /* A list of functions to call after re-parse.  Every function is
     called with the changed ranges and the parser.  The changed
     ranges is a list of (BEG . END).  */
  Lisp_Object after_change_functions;
  /* The buffer associated with this parser.  */
  Lisp_Object buffer;
  /* The pointer to the tree-sitter parser.  Never NULL.  */
  TSParser *parser;
  /* Pointer to the syntax tree.  Initially is NULL, so check for NULL
     before use.  */
  TSTree *tree;
  /* Teaches tree-sitter how to read an Emacs buffer.  */
  TSInput input;
  /* Re-parsing an unchanged buffer is not free for tree-sitter, so we
     only make it re-parse when need_reparse == true.  That usually
     means some change is made in the buffer.  But others could set
     this field to true to force tree-sitter to re-parse.  When you
     set this to true, you should _always_ also increment
     timestamp.  */
  bool need_reparse;
  /* These two positions record the buffer byte position (1-based) of
     the "visible region" that tree-sitter sees.  Before re-parse, we
     move these positions to match BUF_BEGV_BYTE and BUF_ZV_BYTE.
     Note that we don't need to synchronize these positions when
     retrieving them in a function that involves a node: if the node
     is not outdated, these positions are synchronized.  See comment
     (ref:visible-beg-null) in treesit.c for more explanation.  */
  ptrdiff_t visible_beg;
  ptrdiff_t visible_end;
  /* This counter is incremented every time a change is made to the
     buffer in treesit_record_change.  The node retrieved from this parser
     inherits this timestamp.  This way we can make sure the node is
     not outdated when we access its information.  */
  ptrdiff_t timestamp;
  /* If this field is true, parser functions raises
     treesit-parser-deleted signal.  */
  bool deleted;
  /* If this field is true, the parser has ranges set.  See
     Ftreesit_parser_included_ranges for why we need this.  */
  bool has_range;
};

/* A wrapper around a tree-sitter node.  */
struct Lisp_TS_Node
{
  union vectorlike_header header;
  /* This prevents gc from collecting the tree before the node is done
     with it.  TSNode contains a pointer to the tree it belongs to,
     and the parser object, when collected by gc, will free that
     tree.  */
  Lisp_Object parser;
  TSNode node;
  /* A node inherits its parser's timestamp at creation time.  The
     parser's timestamp increments as the buffer changes.  This way we
     can make sure the node is not outdated when we access its
     information.  */
  ptrdiff_t timestamp;
};

/* A compiled tree-sitter query.

   When we create a query object by treesit-compile-query, it is not
   immediately compiled, because that would require the language
   definition to be loaded.  For example, python.el contains

   (defvar xxx (treesit-compile-query ...))

   and (require 'python.el) requires python's language definition to
   be available.  In the case of python.el, Emacs requires it when
   building, so that breaks the build.  */
struct Lisp_TS_Query
{
  union vectorlike_header header;
  /* Language symbol for the query.  */
  Lisp_Object language;
  /* Source lisp (sexp or string) query.  */
  Lisp_Object source;
  /* Pointer to the query object.  This can be NULL, meaning this
     query is not initialized/compiled.  We compile the query when
     it is used the first time (in treesit-query-capture).  */
  TSQuery *query;
  /* Pointer to a cursor.  If we are storing the query object, we
     might as well store a cursor, too.  */
  TSQueryCursor *cursor;
};

INLINE bool
TS_PARSERP (Lisp_Object x)
{
  return PSEUDOVECTORP (x, PVEC_TS_PARSER);
}

INLINE struct Lisp_TS_Parser *
XTS_PARSER (Lisp_Object a)
{
  eassert (TS_PARSERP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_TS_Parser);
}

INLINE bool
TS_NODEP (Lisp_Object x)
{
  return PSEUDOVECTORP (x, PVEC_TS_NODE);
}

INLINE struct Lisp_TS_Node *
XTS_NODE (Lisp_Object a)
{
  eassert (TS_NODEP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_TS_Node);
}

INLINE bool
TS_COMPILED_QUERY_P (Lisp_Object x)
{
  return PSEUDOVECTORP (x, PVEC_TS_COMPILED_QUERY);
}

INLINE struct Lisp_TS_Query *
XTS_COMPILED_QUERY (Lisp_Object a)
{
  eassert (TS_COMPILED_QUERY_P (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_TS_Query);
}

INLINE void
CHECK_TS_PARSER (Lisp_Object parser)
{
  CHECK_TYPE (TS_PARSERP (parser), Qtreesit_parser_p, parser);
}

INLINE void
CHECK_TS_NODE (Lisp_Object node)
{
  CHECK_TYPE (TS_NODEP (node), Qtreesit_node_p, node);
}

INLINE void
CHECK_TS_COMPILED_QUERY (Lisp_Object query)
{
  CHECK_TYPE (TS_COMPILED_QUERY_P (query),
	      Qtreesit_compiled_query_p, query);
}

INLINE_HEADER_END

extern void treesit_record_change (ptrdiff_t, ptrdiff_t, ptrdiff_t);
extern Lisp_Object make_treesit_parser (Lisp_Object, TSParser *, TSTree *,
					Lisp_Object);
extern Lisp_Object make_treesit_node (Lisp_Object, TSNode);

extern bool treesit_node_uptodate_p (Lisp_Object);

extern void treesit_delete_parser (struct Lisp_TS_Parser *);
extern void treesit_delete_query (struct Lisp_TS_Query *);
extern bool treesit_named_node_p (TSNode);
extern bool treesit_node_eq (Lisp_Object, Lisp_Object);

#endif	/* HAVE_TREE_SITTER */

extern void syms_of_treesit (void);

#endif /* EMACS_TREESIT_H */
