/* A Bison parser, made from parse.y
   by GNU bison 1.32.  */

#define YYBISON 1  /* Identify Bison output.  */

# define	NE	257
# define	LE	258
# define	GE	259
# define	NEG	260
# define	L_CELL	261
# define	L_RANGE	262
# define	L_VAR	263
# define	L_CONST	264
# define	L_FN0	265
# define	L_FN1	266
# define	L_FN2	267
# define	L_FN3	268
# define	L_FN4	269
# define	L_FNN	270
# define	L_FN1R	271
# define	L_FN2R	272
# define	L_FN3R	273
# define	L_FN4R	274
# define	L_FNNR	275
# define	L_LE	276
# define	L_NE	277
# define	L_GE	278

#line 1 "y-src/parse.y"

/*	Copyright (C) 1990, 1992-1993, 2016-2020 Free Software Foundation,
 *	Inc.

This file is part of Oleo, the GNU Spreadsheet.

Oleo is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Oleo is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Oleo; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */
#line 41 "y-src/parse.y"

#include "funcdef.h"

#include <ctype.h>

#define obstack_chunk_alloc ck_malloc
#define obstack_chunk_free free
#include "obstack.h"
#include "sysdef.h"

#include "global.h"
#include "errors.h"
#include "node.h"
#include "eval.h"
#include "ref.h"

int yylex ();
#ifdef __STDC__
void yyerror (char *);
#else
void yyerror ();
#endif
VOIDSTAR parse_hash;
extern VOIDSTAR hash_find();

/* This table contains a list of the infix single-char functions */
unsigned char fnin[] = {
	SUM, DIFF, DIV, PROD, MOD, /* AND, OR, */ POW, EQUAL, IF, CONCAT, 0
};

#define YYSTYPE _y_y_s_t_y_p_e
typedef struct node *YYSTYPE;
YYSTYPE parse_return;
#ifdef __STDC__
YYSTYPE make_list (YYSTYPE, YYSTYPE);
#else
YYSTYPE make_list ();
#endif

char *instr;
int parse_error = 0;
extern struct obstack tmp_mem;

#ifndef YYSTYPE
#define YYSTYPE int
#endif
#ifndef YYDEBUG
# define YYDEBUG 0
#endif



#define	YYFINAL		131
#define	YYFLAG		-32768
#define	YYNTBASE	41

/* YYTRANSLATE(YYLEX) -- Bison token number corresponding to YYLEX. */
#define YYTRANSLATE(x) ((unsigned)(x) <= 278 ? yytranslate[x] : 47)

/* YYTRANSLATE[YYLEX] -- Bison token number corresponding to YYLEX. */
static const char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    19,     2,     2,     2,    16,     5,     2,
      38,    39,    14,    12,    40,    13,     2,    15,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     4,     2,
       8,     6,    10,     3,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    17,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     7,     9,    11,
      18,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37
};

#if YYDEBUG
static const short yyprhs[] =
{
       0,     0,     2,     4,     6,     8,    12,    17,    24,    33,
      44,    49,    54,    59,    66,    73,    82,    91,   100,   109,
     114,   120,   124,   128,   132,   136,   140,   144,   148,   152,
     156,   160,   164,   168,   172,   175,   178,   182,   186,   189,
     191,   195,   197,   199,   201,   205,   207
};
static const short yyrhs[] =
{
      42,     0,     1,     0,    23,     0,    46,     0,    24,    38,
      39,     0,    25,    38,    42,    39,     0,    26,    38,    42,
      40,    42,    39,     0,    27,    38,    42,    40,    42,    40,
      42,    39,     0,    28,    38,    42,    40,    42,    40,    42,
      40,    42,    39,     0,    29,    38,    43,    39,     0,    30,
      38,    21,    39,     0,    30,    38,    22,    39,     0,    31,
      38,    21,    40,    42,    39,     0,    31,    38,    22,    40,
      42,    39,     0,    31,    38,    21,    40,    42,    40,    42,
      39,     0,    31,    38,    22,    40,    42,    40,    42,    39,
       0,    32,    38,    21,    40,    42,    40,    42,    39,     0,
      32,    38,    22,    40,    42,    40,    42,    39,     0,    34,
      38,    45,    39,     0,    42,     3,    42,     4,    42,     0,
      42,     5,    42,     0,    42,     8,    42,     0,    42,     9,
      42,     0,    42,     6,    42,     0,    42,     7,    42,     0,
      42,    10,    42,     0,    42,    11,    42,     0,    42,    12,
      42,     0,    42,    13,    42,     0,    42,    14,    42,     0,
      42,    15,    42,     0,    42,    16,    42,     0,    42,    17,
      42,     0,    13,    42,     0,    19,    42,     0,    38,    42,
      39,     0,    38,    42,     1,     0,    38,     1,     0,    42,
       0,    43,    40,    42,     0,    21,     0,    42,     0,    44,
       0,    45,    40,    44,     0,    20,     0,    22,     0
};

#endif

#if YYDEBUG
/* YYRLINE[YYN] -- source line where rule number YYN was defined. */
static const short yyrline[] =
{
       0,    86,    88,    94,    95,    96,    98,   102,   106,   110,
     114,   118,   121,   125,   129,   135,   142,   150,   154,   159,
     163,   174,   178,   182,   186,   190,   194,   198,   202,   206,
     210,   214,   218,   222,   226,   241,   245,   247,   255,   262,
     264,   268,   269,   272,   274,   278,   280
};
#endif


#if (YYDEBUG) || defined YYERROR_VERBOSE

/* YYTNAME[TOKEN_NUM] -- String name of the token TOKEN_NUM. */
static const char *const yytname[] =
{
  "$", "error", "$undefined.", "'?'", "':'", "'&'", "'='", "NE", "'<'", 
  "LE", "'>'", "GE", "'+'", "'-'", "'*'", "'/'", "'%'", "'^'", "NEG", 
  "'!'", "L_CELL", "L_RANGE", "L_VAR", "L_CONST", "L_FN0", "L_FN1", 
  "L_FN2", "L_FN3", "L_FN4", "L_FNN", "L_FN1R", "L_FN2R", "L_FN3R", 
  "L_FN4R", "L_FNNR", "L_LE", "L_NE", "L_GE", "'('", "')'", "','", "line", 
  "exp", "exp_list", "range_exp", "range_exp_list", "cell", NULL
};
#endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives. */
static const short yyr1[] =
{
       0,    41,    41,    42,    42,    42,    42,    42,    42,    42,
      42,    42,    42,    42,    42,    42,    42,    42,    42,    42,
      42,    42,    42,    42,    42,    42,    42,    42,    42,    42,
      42,    42,    42,    42,    42,    42,    42,    42,    42,    43,
      43,    44,    44,    45,    45,    46,    46
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN. */
static const short yyr2[] =
{
       0,     1,     1,     1,     1,     3,     4,     6,     8,    10,
       4,     4,     4,     6,     6,     8,     8,     8,     8,     4,
       5,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     2,     2,     3,     3,     2,     1,
       3,     1,     1,     1,     3,     1,     1
};

/* YYDEFACT[S] -- default rule to reduce with in state S when YYTABLE
   doesn't specify something else to do.  Zero means the default is an
   error. */
static const short yydefact[] =
{
       0,     2,     0,     0,    45,    46,     3,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     1,     4,
      34,    35,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    38,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     0,
       0,     0,     0,    39,     0,     0,     0,     0,     0,     0,
       0,    41,    42,    43,     0,    37,    36,     0,    21,    24,
      25,    22,    23,    26,    27,    28,    29,    30,    31,    32,
      33,     6,     0,     0,     0,    10,     0,    11,    12,     0,
       0,     0,     0,    19,     0,     0,     0,     0,     0,    40,
       0,     0,     0,     0,    44,    20,     7,     0,     0,    13,
       0,    14,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     8,     0,    15,    16,    17,    18,     0,     9,     0,
       0,     0
};

static const short yydefgoto[] =
{
     129,    62,    54,    63,    64,    19
};

static const short yypact[] =
{
     104,-32768,   486,   486,-32768,-32768,-32768,   -37,   -22,   -16,
      10,    12,    14,    29,    43,    47,    50,   124,   537,-32768,
  -32768,-32768,    59,   486,   486,   486,   486,   486,     7,     9,
      11,   464,-32768,    48,   486,   486,   486,   486,   486,   486,
     486,   486,   486,   486,   486,   486,   486,   486,-32768,   332,
     173,   209,   224,   537,    54,    60,    61,    64,    66,    69,
      71,-32768,   537,-32768,    57,-32768,-32768,   522,    -2,   193,
     193,   547,   547,   547,   547,     4,     4,    84,    84,    84,
      84,-32768,   486,   486,   486,-32768,   486,-32768,-32768,   486,
     486,   486,   486,-32768,   464,   486,   353,   245,   260,   537,
      63,   158,   281,   296,-32768,   537,-32768,   486,   486,-32768,
     486,-32768,   486,   486,   486,   369,   317,   388,   404,   423,
     439,-32768,   486,-32768,-32768,-32768,-32768,   458,-32768,   115,
     116,-32768
};

static const short yypgoto[] =
{
  -32768,     0,-32768,    24,-32768,-32768
};


#define	YYLAST		564


static const short yytable[] =
{
      18,    22,    20,    21,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    23,    33,    44,    45,
      46,    47,    24,    49,    50,    51,    52,    53,    55,    56,
      57,    58,    59,    60,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    25,    65,
      26,    34,    27,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    34,    28,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    29,    96,    97,    98,    30,    99,    66,    31,   100,
     101,   102,   103,    85,    86,   105,    93,    94,    48,    87,
      88,    47,   109,   110,    89,     1,    90,   115,   116,    91,
     117,    92,   118,   119,   120,   130,   131,     2,   104,     0,
       0,     0,   127,     3,     4,    32,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,     2,    16,     0,
       0,     0,    17,     3,     4,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,     0,    16,     0,
       0,    34,    17,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    34,     0,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,   111,   112,-32768,
  -32768,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,     0,    34,    82,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    34,     0,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,     0,     0,     0,     0,     0,     0,    34,    83,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    34,    84,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,    34,   107,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    34,
     108,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,     0,     0,     0,     0,     0,     0,
      34,   113,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    34,   114,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
       0,     0,     0,     0,     0,     0,    34,   122,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    81,    34,     0,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,     0,     0,     0,
       0,    34,   106,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,     0,    34,   121,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,     0,     0,     0,     0,    34,   123,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,     0,    34,   124,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,     0,     0,     0,
       0,    34,   125,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,     0,     2,   126,     0,
       0,     0,     0,     3,     4,    61,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,   128,    16,     2,
       0,     0,    17,     0,     0,     3,     4,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,     0,
      16,     0,     0,     0,    17,    34,    95,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      34,     0,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,-32768,-32768,-32768,-32768,    42,
      43,    44,    45,    46,    47
};

static const short yycheck[] =
{
       0,    38,     2,     3,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    38,    17,    14,    15,
      16,    17,    38,    23,    24,    25,    26,    27,    21,    22,
      21,    22,    21,    22,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    38,     1,
      38,     3,    38,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,     3,    38,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    38,    82,    83,    84,    38,    86,    39,    38,    89,
      90,    91,    92,    39,    40,    95,    39,    40,    39,    39,
      39,    17,    39,    40,    40,     1,    40,   107,   108,    40,
     110,    40,   112,   113,   114,     0,     0,    13,    94,    -1,
      -1,    -1,   122,    19,    20,     1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    13,    34,    -1,
      -1,    -1,    38,    19,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    -1,    34,    -1,
      -1,     3,    38,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,     3,    -1,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    39,    40,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,     3,    40,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     3,    -1,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,     3,    40,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     3,    40,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      -1,    -1,    -1,    -1,     3,    40,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,     3,
      40,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,
       3,    40,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     3,    40,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,     3,    40,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    39,     3,    -1,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    -1,
      -1,     3,    39,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,     3,    39,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,     3,    39,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,     3,    39,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    -1,
      -1,     3,    39,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    13,    39,    -1,
      -1,    -1,    -1,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    39,    34,    13,
      -1,    -1,    38,    -1,    -1,    19,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    -1,
      34,    -1,    -1,    -1,    38,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
       3,    -1,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/share/bison/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser when
   the %semantic_parser declaration is not specified in the grammar.
   It was written by Richard Stallman by simplifying the hairy parser
   used when %semantic_parser is specified.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

#ifdef __cplusplus
# define YYSTD(x) std::x
#else
# define YYSTD(x) x
#endif

#if ! defined (yyoverflow) || defined (YYERROR_VERBOSE)

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# if YYSTACK_USE_ALLOCA
#  define YYSTACK_ALLOC alloca
#  define YYSIZE_T YYSTD (size_t)
# else
#  ifndef YYSTACK_USE_ALLOCA
#   if defined (alloca) || defined (_ALLOCA_H)
#    define YYSTACK_ALLOC alloca
#    define YYSIZE_T YYSTD (size_t)
#   else
#    ifdef __GNUC__
#     define YYSTACK_ALLOC __builtin_alloca
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  ifdef __cplusplus
#   include <cstdlib> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T std::size_t
#  else
#   ifdef __STDC__
#    include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#    define YYSIZE_T size_t
#   endif
#  endif
#  define YYSTACK_ALLOC YYSTD (malloc)
#  define YYSTACK_FREE YYSTD (free)
# endif

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
# if YYLSP_NEEDED
  YYLTYPE yyls;
# endif
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAX (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# if YYLSP_NEEDED
#  define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE) + sizeof (YYLTYPE))	\
      + 2 * YYSTACK_GAP_MAX)
# else
#  define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAX)
# endif

/* Relocate the TYPE STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Type, Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	yymemcpy ((char *) yyptr, (char *) (Stack),			\
		  yysize * (YYSIZE_T) sizeof (Type));			\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (Type) + YYSTACK_GAP_MAX;	\
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif /* ! defined (yyoverflow) || defined (YYERROR_VERBOSE) */


#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# ifdef __cplusplus
#  include <cstddef> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T std::size_t
# else
#  ifdef __STDC__
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");			\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).

   When YYLLOC_DEFAULT is run, CURRENT is set the location of the
   first token.  By default, to implement support for ranges, extend
   its range to the last symbol.  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)       	\
   Current.last_line   = Rhs[N].last_line;	\
   Current.last_column = Rhs[N].last_column;
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#if YYPURE
# if YYLSP_NEEDED
#  ifdef YYLEX_PARAM
#   define YYLEX		yylex (&yylval, &yylloc, YYLEX_PARAM)
#  else
#   define YYLEX		yylex (&yylval, &yylloc)
#  endif
# else /* !YYLSP_NEEDED */
#  ifdef YYLEX_PARAM
#   define YYLEX		yylex (&yylval, YYLEX_PARAM)
#  else
#   define YYLEX		yylex (&yylval)
#  endif
# endif /* !YYLSP_NEEDED */
#else /* !YYPURE */
# define YYLEX			yylex ()
#endif /* !YYPURE */


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  ifdef __cplusplus
#   include <cstdio>  /* INFRINGES ON USER NAME SPACE */
#  else
#   include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYFPRINTF YYSTD (fprintf)
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)
/* Nonzero means print parse trace. [The following comment makes no
   sense to me.  Could someone clarify it?  --akim] Since this is
   uninitialized, it does not stop multiple parsers from coexisting.
   */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
#endif /* !YYDEBUG */

/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif

#if ! defined (yyoverflow) && ! defined (yymemcpy)
# if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#  define yymemcpy __builtin_memcpy
# else				/* not GNU C or C++ */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
#  if defined (__STDC__) || defined (__cplusplus)
yymemcpy (char *yyto, const char *yyfrom, YYSIZE_T yycount)
#  else
yymemcpy (yyto, yyfrom, yycount)
     char *yyto;
     const char *yyfrom;
     YYSIZE_T yycount;
#  endif
{
  register const char *yyf = yyfrom;
  register char *yyt = yyto;
  register YYSIZE_T yyi = yycount;

  while (yyi-- != 0)
    *yyt++ = *yyf++;
}
# endif
#endif

#ifdef YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif
#endif

#line 341 "/usr/share/bison/bison.simple"


/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
# ifdef __cplusplus
#  define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#  define YYPARSE_PARAM_DECL
# else /* !__cplusplus */
#  define YYPARSE_PARAM_ARG YYPARSE_PARAM
#  define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
# endif /* !__cplusplus */
#else /* !YYPARSE_PARAM */
# define YYPARSE_PARAM_ARG
# define YYPARSE_PARAM_DECL
#endif /* !YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
# ifdef YYPARSE_PARAM
int yyparse (void *);
# else
int yyparse (void);
# endif
#endif

/* YY_DECL_VARIABLES -- depending whether we use a pure parser,
   variables are global, or local to YYPARSE.  */

#define YY_DECL_NON_LSP_VARIABLES			\
/* The lookahead symbol.  */				\
int yychar;						\
							\
/* The semantic value of the lookahead symbol. */	\
YYSTYPE yylval;						\
							\
/* Number of parse errors so far.  */			\
int yynerrs;

#if YYLSP_NEEDED
# define YY_DECL_VARIABLES			\
YY_DECL_NON_LSP_VARIABLES			\
						\
/* Location data for the lookahead symbol.  */	\
YYLTYPE yylloc;
#else
# define YY_DECL_VARIABLES			\
YY_DECL_NON_LSP_VARIABLES
#endif


/* If nonreentrant, generate the variables here. */

#if !YYPURE
YY_DECL_VARIABLES
#endif  /* !YYPURE */

int
yyparse (YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  /* If reentrant, generate the variables here. */
#if YYPURE
  YY_DECL_VARIABLES
#endif  /* !YYPURE */

  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yychar1 = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack. */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;

#if YYLSP_NEEDED
  /* The location stack.  */
  YYLTYPE yylsa[YYINITDEPTH];
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
#endif

#if YYLSP_NEEDED
# define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
# define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  YYSIZE_T yystacksize = YYINITDEPTH;


  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
#if YYLSP_NEEDED
  YYLTYPE yyloc;
#endif

  /* When reducing, the number of symbols on the RHS of the reduced
     rule. */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;
#if YYLSP_NEEDED
  yylsp = yyls;
#endif
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  */
# if YYLSP_NEEDED
	YYLTYPE *yyls1 = yyls;
	/* This used to be a conditional around just the two extra args,
	   but that might be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yyls1, yysize * sizeof (*yylsp),
		    &yystacksize);
	yyls = yyls1;
# else
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);
# endif
	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (short, yyss);
	YYSTACK_RELOCATE (YYSTYPE, yyvs);
# if YYLSP_NEEDED
	YYSTACK_RELOCATE (YYLTYPE, yyls);
# endif
# undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
#if YYLSP_NEEDED
      yylsp = yyls + yysize - 1;
#endif

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yychar1 = YYTRANSLATE (yychar);

#if YYDEBUG
     /* We have to keep this `#if YYDEBUG', since we use variables
	which are defined only if `YYDEBUG' is set.  */
      if (yydebug)
	{
	  YYFPRINTF (stderr, "Next token is %d (%s",
		     yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise
	     meaning of a token, for further debugging info.  */
# ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
# endif
	  YYFPRINTF (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %d (%s), ",
	      yychar, yytname[yychar1]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#if YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to the semantic value of
     the lookahead token.  This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

#if YYLSP_NEEDED
  /* Similarly for the default location.  Let the user run additional
     commands if for instance locations are ranges.  */
  yyloc = yylsp[1-yylen];
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
#endif

#if YYDEBUG
  /* We have to keep this `#if YYDEBUG', since we use variables which
     are defined only if `YYDEBUG' is set.  */
  if (yydebug)
    {
      int yyi;

      YYFPRINTF (stderr, "Reducing via rule %d (line %d), ",
		 yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (yyi = yyprhs[yyn]; yyrhs[yyi] > 0; yyi++)
	YYFPRINTF (stderr, "%s ", yytname[yyrhs[yyi]]);
      YYFPRINTF (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif

  switch (yyn) {

case 1:
#line 87 "y-src/parse.y"
{ parse_return=yyvsp[0]; }
    break;
case 2:
#line 88 "y-src/parse.y"
{
		if(!parse_error)
			parse_error=PARSE_ERR;
		parse_return=0; }
    break;
case 5:
#line 96 "y-src/parse.y"
{
		yyval=yyvsp[-2]; }
    break;
case 6:
#line 98 "y-src/parse.y"
{
		(yyvsp[-3])->n_x.v_subs[0]=yyvsp[-1];
		(yyvsp[-3])->n_x.v_subs[1]=(struct node *)0;
		yyval=yyvsp[-3]; }
    break;
case 7:
#line 102 "y-src/parse.y"
{
		(yyvsp[-5])->n_x.v_subs[0]=yyvsp[-3];
		(yyvsp[-5])->n_x.v_subs[1]=yyvsp[-1];
		yyval=yyvsp[-5]; }
    break;
case 8:
#line 106 "y-src/parse.y"
{
		(yyvsp[-7])->n_x.v_subs[0]=make_list(yyvsp[-5],yyvsp[-3]);
 		(yyvsp[-7])->n_x.v_subs[1]=yyvsp[-1];
 		yyval=yyvsp[-7];}
    break;
case 9:
#line 110 "y-src/parse.y"
{
		(yyvsp[-9])->n_x.v_subs[0]=make_list(yyvsp[-7],yyvsp[-5]);
 		(yyvsp[-9])->n_x.v_subs[1]=make_list(yyvsp[-3],yyvsp[-1]);
 		yyval=yyvsp[-9];}
    break;
case 10:
#line 114 "y-src/parse.y"
{
		(yyvsp[-3])->n_x.v_subs[0]=(struct node *)0;
		(yyvsp[-3])->n_x.v_subs[1]=yyvsp[-1];
		yyval=yyvsp[-3]; }
    break;
case 11:
#line 118 "y-src/parse.y"
{
		yyvsp[-3]->n_x.v_subs[0]=yyvsp[-1];
		yyval=yyvsp[-3]; }
    break;
case 12:
#line 121 "y-src/parse.y"
{
		yyvsp[-3]->n_x.v_subs[0]=yyvsp[-1];
		yyval=yyvsp[-3]; }
    break;
case 13:
#line 125 "y-src/parse.y"
{
		yyvsp[-5]->n_x.v_subs[0]=yyvsp[-3];
		yyvsp[-5]->n_x.v_subs[1]=yyvsp[-1];
		yyval=yyvsp[-5]; }
    break;
case 14:
#line 129 "y-src/parse.y"
{
		yyvsp[-5]->n_x.v_subs[0]=yyvsp[-3];
		yyvsp[-5]->n_x.v_subs[1]=yyvsp[-1];
		yyval=yyvsp[-5]; }
    break;
case 15:
#line 135 "y-src/parse.y"
{
		if(yyvsp[-7]->comp_value!=F_INDEX)
			parse_error=PARSE_ERR;
		yyvsp[-7]->comp_value=F_INDEX2;
		yyvsp[-7]->n_x.v_subs[0]=make_list(yyvsp[-5],yyvsp[-3]);
		yyvsp[-7]->n_x.v_subs[1]=yyvsp[-1];
		yyval=yyvsp[-7]; }
    break;
case 16:
#line 142 "y-src/parse.y"
{
		if(yyvsp[-7]->comp_value!=F_INDEX)
			parse_error=PARSE_ERR;
		yyvsp[-7]->comp_value=F_INDEX2;
		yyvsp[-7]->n_x.v_subs[0]=make_list(yyvsp[-5],yyvsp[-3]);
		yyvsp[-7]->n_x.v_subs[1]=yyvsp[-1];
		yyval=yyvsp[-7]; }
    break;
case 17:
#line 150 "y-src/parse.y"
{
		(yyvsp[-7])->n_x.v_subs[0]=make_list(yyvsp[-5],yyvsp[-3]);
 		(yyvsp[-7])->n_x.v_subs[1]=yyvsp[-1];
 		yyval=yyvsp[-7];}
    break;
case 18:
#line 154 "y-src/parse.y"
{
		(yyvsp[-7])->n_x.v_subs[0]=make_list(yyvsp[-5],yyvsp[-3]);
 		(yyvsp[-7])->n_x.v_subs[1]=yyvsp[-1];
 		yyval=yyvsp[-7];}
    break;
case 19:
#line 159 "y-src/parse.y"
{
		(yyvsp[-3])->n_x.v_subs[0]=(struct node *)0;
		(yyvsp[-3])->n_x.v_subs[1]=yyvsp[-1];
		yyval=yyvsp[-3]; }
    break;
case 20:
#line 163 "y-src/parse.y"
{
		yyvsp[-3]->comp_value=IF;
		yyvsp[-3]->n_x.v_subs[0]=yyvsp[-1];
		yyvsp[-3]->n_x.v_subs[1]=yyvsp[0];
		yyvsp[-1]->n_x.v_subs[0]=yyvsp[-4];
		yyvsp[-1]->n_x.v_subs[1]=yyvsp[-2];
		yyval=yyvsp[-3]; }
    break;
case 21:
#line 174 "y-src/parse.y"
{
		yyvsp[-1]->n_x.v_subs[0]=yyvsp[-2];
		yyvsp[-1]->n_x.v_subs[1]=yyvsp[0];
		yyval = yyvsp[-1]; }
    break;
case 22:
#line 178 "y-src/parse.y"
{
		yyvsp[-1]->n_x.v_subs[0]=yyvsp[-2];
		yyvsp[-1]->n_x.v_subs[1]=yyvsp[0];
		yyval = yyvsp[-1]; }
    break;
case 23:
#line 182 "y-src/parse.y"
{
		yyvsp[-1]->n_x.v_subs[0]=yyvsp[-2];
		yyvsp[-1]->n_x.v_subs[1]=yyvsp[0];
		yyval = yyvsp[-1]; }
    break;
case 24:
#line 186 "y-src/parse.y"
{
		yyvsp[-1]->n_x.v_subs[0]=yyvsp[-2];
		yyvsp[-1]->n_x.v_subs[1]=yyvsp[0];
		yyval = yyvsp[-1]; }
    break;
case 25:
#line 190 "y-src/parse.y"
{
		yyvsp[-1]->n_x.v_subs[0]=yyvsp[-2];
		yyvsp[-1]->n_x.v_subs[1]=yyvsp[0];
		yyval = yyvsp[-1]; }
    break;
case 26:
#line 194 "y-src/parse.y"
{
		yyvsp[-1]->n_x.v_subs[0]=yyvsp[-2];
		yyvsp[-1]->n_x.v_subs[1]=yyvsp[0];
		yyval = yyvsp[-1]; }
    break;
case 27:
#line 198 "y-src/parse.y"
{
		yyvsp[-1]->n_x.v_subs[0]=yyvsp[-2];
		yyvsp[-1]->n_x.v_subs[1]=yyvsp[0];
		yyval = yyvsp[-1]; }
    break;
case 28:
#line 202 "y-src/parse.y"
{
		yyvsp[-1]->n_x.v_subs[0]=yyvsp[-2];
		yyvsp[-1]->n_x.v_subs[1]=yyvsp[0];
		yyval = yyvsp[-1]; }
    break;
case 29:
#line 206 "y-src/parse.y"
{
		yyvsp[-1]->n_x.v_subs[0]=yyvsp[-2];
		yyvsp[-1]->n_x.v_subs[1]=yyvsp[0];
		yyval = yyvsp[-1]; }
    break;
case 30:
#line 210 "y-src/parse.y"
{
		yyvsp[-1]->n_x.v_subs[0]=yyvsp[-2];
		yyvsp[-1]->n_x.v_subs[1]=yyvsp[0];
		yyval = yyvsp[-1]; }
    break;
case 31:
#line 214 "y-src/parse.y"
{
		yyvsp[-1]->n_x.v_subs[0]=yyvsp[-2];
		yyvsp[-1]->n_x.v_subs[1]=yyvsp[0];
		yyval = yyvsp[-1]; }
    break;
case 32:
#line 218 "y-src/parse.y"
{
		yyvsp[-1]->n_x.v_subs[0]=yyvsp[-2];
		yyvsp[-1]->n_x.v_subs[1]=yyvsp[0];
		yyval = yyvsp[-1]; }
    break;
case 33:
#line 222 "y-src/parse.y"
{
		yyvsp[-1]->n_x.v_subs[0]=yyvsp[-2];
		yyvsp[-1]->n_x.v_subs[1]=yyvsp[0];
		yyval = yyvsp[-1]; }
    break;
case 34:
#line 226 "y-src/parse.y"
{
		if(yyvsp[0]->comp_value==CONST_FLT) {
			yyvsp[0]->n_x.v_float= -(yyvsp[0]->n_x.v_float);
			/* free($1); */
			yyval=yyvsp[0];
		} else if(yyvsp[0]->comp_value==CONST_INT) {
			yyvsp[0]->n_x.v_int= -(yyvsp[0]->n_x.v_int);
			/* free($1); */
			yyval=yyvsp[0];
		} else {
			yyvsp[-1]->comp_value = NEGATE;
			yyvsp[-1]->n_x.v_subs[0]=yyvsp[0];
			yyvsp[-1]->n_x.v_subs[1]=(struct node *)0;
			yyval = yyvsp[-1];
		} }
    break;
case 35:
#line 241 "y-src/parse.y"
{
		yyvsp[-1]->n_x.v_subs[0]=yyvsp[0];
		yyvsp[-1]->n_x.v_subs[1]=(struct node *)0;
		yyval = yyvsp[-1]; }
    break;
case 36:
#line 246 "y-src/parse.y"
{ yyval = yyvsp[-1]; }
    break;
case 37:
#line 247 "y-src/parse.y"
{
		if(!parse_error)
			parse_error=NO_CLOSE;
		}
    break;
case 38:
#line 255 "y-src/parse.y"
{
		if(!parse_error)
			parse_error=NO_CLOSE;
		}
    break;
case 39:
#line 263 "y-src/parse.y"
{ yyval = make_list(yyvsp[0], 0); }
    break;
case 40:
#line 265 "y-src/parse.y"
{ yyval = make_list(yyvsp[0], yyvsp[-2]); }
    break;
case 43:
#line 273 "y-src/parse.y"
{ yyval=make_list(yyvsp[0], 0); }
    break;
case 44:
#line 275 "y-src/parse.y"
{ yyval=make_list(yyvsp[0],yyvsp[-2]); }
    break;
case 45:
#line 279 "y-src/parse.y"
{ yyval=yyvsp[0]; }
    break;
}

#line 727 "/usr/share/bison/bison.simple"


  yyvsp -= yylen;
  yyssp -= yylen;
#if YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG
  if (yydebug)
    {
      short *yyssp1 = yyss - 1;
      YYFPRINTF (stderr, "state stack now");
      while (yyssp1 != yyssp)
	YYFPRINTF (stderr, " %d", *++yyssp1);
      YYFPRINTF (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;
#if YYLSP_NEEDED
  *++yylsp = yyloc;
#endif

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  char *yymsg;
	  int yyx, yycount;

	  yycount = 0;
	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  for (yyx = yyn < 0 ? -yyn : 0;
	       yyx < (int) (sizeof (yytname) / sizeof (char *)); yyx++)
	    if (yycheck[yyx + yyn] == yyx)
	      yysize += yystrlen (yytname[yyx]) + 15, yycount++;
	  yysize += yystrlen ("parse error, unexpected ") + 1;
	  yysize += yystrlen (yytname[YYTRANSLATE (yychar)]);
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "parse error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[YYTRANSLATE (yychar)]);

	      if (yycount < 5)
		{
		  yycount = 0;
		  for (yyx = yyn < 0 ? -yyn : 0;
		       yyx < (int) (sizeof (yytname) / sizeof (char *));
		       yyx++)
		    if (yycheck[yyx + yyn] == yyx)
		      {
			const char *yyq = ! yycount ? ", expecting " : " or ";
			yyp = yystpcpy (yyp, yyq);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yycount++;
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exhausted");
	}
      else
#endif /* defined (YYERROR_VERBOSE) */
	yyerror ("parse error");
    }
  goto yyerrlab1;


/*--------------------------------------------------.
| yyerrlab1 -- error raised explicitly by an action |
`--------------------------------------------------*/
yyerrlab1:
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;
      YYDPRINTF ((stderr, "Discarding token %d (%s).\n",
		  yychar, yytname[yychar1]));
      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;


/*-------------------------------------------------------------------.
| yyerrdefault -- current state does not do anything special for the |
| error token.                                                       |
`-------------------------------------------------------------------*/
yyerrdefault:
#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */

  /* If its default is to accept any token, ok.  Otherwise pop it.  */
  yyn = yydefact[yystate];
  if (yyn)
    goto yydefault;
#endif


/*---------------------------------------------------------------.
| yyerrpop -- pop the current state because it cannot handle the |
| error token                                                    |
`---------------------------------------------------------------*/
yyerrpop:
  if (yyssp == yyss)
    YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#if YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG
  if (yydebug)
    {
      short *yyssp1 = yyss - 1;
      YYFPRINTF (stderr, "Error: state stack now");
      while (yyssp1 != yyssp)
	YYFPRINTF (stderr, " %d", *++yyssp1);
      YYFPRINTF (stderr, "\n");
    }
#endif

/*--------------.
| yyerrhandle.  |
`--------------*/
yyerrhandle:
  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;
#if YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

/*---------------------------------------------.
| yyoverflowab -- parser overflow comes here.  |
`---------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}
#line 282 "y-src/parse.y"


void
yyerror FUN1(char *, s)
{
	if(!parse_error)
		parse_error=PARSE_ERR;
}

YYSTYPE
make_list FUN2(YYSTYPE, car, YYSTYPE, cdr)
{
	YYSTYPE ret;

	ret=(YYSTYPE)obstack_alloc(&tmp_mem,sizeof(*ret));
	ret->comp_value = 0;
	ret->n_x.v_subs[0]=car;
	ret->n_x.v_subs[1]=cdr;
	return ret;
}

#define ERROR -1

extern struct node *yylval;

#ifdef __STDC__
unsigned char parse_cell_or_range (char **,struct rng *);
#else
unsigned char parse_cell_or_range ();
#endif

int
yylex FUN0()
{
	int ch;
	struct node *new;
	int isflt;
	char *begin;
	char *tmp_str;
	unsigned char byte_value;
	int n;

	/* unsigned char *ptr; */
	int nn;
	struct function *fp;
	int tmp_ch;

#ifdef TEST
	if(!instr)
		return ERROR;
#endif
	while(isspace(*instr))
		instr++;
	ch = *instr++;
	if(ch=='(' || ch==',' || ch==')')
		return ch;

	new=(struct node *)obstack_alloc(&tmp_mem,sizeof(struct node));
	new->add_byte=0;
	new->sub_value=0;
	switch(ch) {
	case 0:
		return 0;

	case '0': case '1': case '2': case '3': case '4': case '5': case '6':
	case '7': case '8': case '9': case '.':
		isflt = (ch=='.');

		begin=instr-1;
		tmp_str=instr;

		while(isdigit(*tmp_str) || (!isflt && *tmp_str=='.' && ++isflt))
			tmp_str++;
		if(*tmp_str=='e' || *tmp_str=='E') {
			isflt=1;
			tmp_str++;
			if(*tmp_str=='-' || *tmp_str=='+')
				tmp_str++;
			while(isdigit(*tmp_str))
				tmp_str++;
		}
		if(isflt) {
			new->n_x.v_float=astof((char **)(&begin));
			byte_value=CONST_FLT;
		} else {
			new->n_x.v_int=astol((char **)(&begin));
			if(begin!=tmp_str) {
				begin=instr-1;
				new->n_x.v_float=astof((char **)(&begin));
				byte_value=CONST_FLT;
			} else
				byte_value=CONST_INT;
		}
		ch=L_CONST;
		instr=begin;
		break;

	case '"':
		begin=instr;
		while(*instr && *instr!='"') {
			if(*instr=='\\' && instr[1])
				instr++;
			instr++;
		}
		if(!*instr) {
			parse_error=NO_QUOTE;
			return ERROR;
		}
		tmp_str=new->n_x.v_string=(char *)ck_malloc(1+instr-begin);
		while(begin!=instr) {
			unsigned char n;

			if(*begin=='\\') {
				begin++;
				if(begin[0]>='0' && begin[0]<='7') {
					if(begin[1]>='0' && begin[1]<='7') {
						if(begin[2]>='0' && begin[2]<='7') {
							n=(begin[2]-'0') + (010 * (begin[1]-'0')) + ( 0100 * (begin[0]-'0'));
							begin+=3;
						} else {
							n=(begin[1]-'0') + (010 * (begin[0]-'0'));
							begin+=2;
						}
					} else {
						n=begin[0]-'0';
						begin++;
					}
				} else
					n= *begin++;
				*tmp_str++= n;
			} else
				*tmp_str++= *begin++;
		}
		*tmp_str='\0';
		instr++;
		byte_value=CONST_STR;
		ch=L_CONST;
		break;

	case '+':	case '-':

	case '*':	case '/':	case '%':	case '&':
	/* case '|': */	case '^':	case '=':

	case '?':
	{
		unsigned char *ptr;

		for(ptr= fnin;*ptr;ptr++)
			if(the_funs[*ptr].fn_str[0]==ch)
				break;
#ifdef TEST
		if(!*ptr)
			panic("Can't find fnin[] entry for '%c'",ch);
#endif
		byte_value= *ptr;
	}
		break;

	case ':':
		byte_value=IF;
		break;

	case '!':
	case '<':
	case '>':
		if(*instr!='=') {
			byte_value = (ch=='<') ? LESS : (ch=='>') ? GREATER : NOT;
			break;
		}
		instr++;
		byte_value = (ch=='<') ? LESSEQ : (ch=='>') ? GREATEQ : NOTEQUAL;
		ch = (ch=='<') ? LE : (ch=='>') ? GE : NE;
		break;

	case '\'':
	case ';':
	case '[':
	case '\\':
	case ']':
	case '`':
	case '{':
	case '}':
	case '~':
	bad_chr:
		parse_error=BAD_CHAR;
		return ERROR;

	case '#':
		begin=instr-1;
		while(*instr && (isalnum(*instr) || *instr=='_'))
			instr++;
		ch= *instr;
		*instr=0;
		if(!stricmp(begin,tname))
			byte_value=F_TRUE;
		else if(!stricmp(begin,fname))
			byte_value=F_FALSE;
		else if(!stricmp(begin,iname) && (begin[4]==0 || !stricmp(begin+4,"inity")))
			byte_value=CONST_INF;
		else if(!stricmp(begin,mname) ||
			!stricmp(begin,"#ninf"))
			byte_value=CONST_NINF;
		else if(!stricmp(begin,nname) ||
			!stricmp(begin,"#nan"))
			byte_value=CONST_NAN;
		else {
			for(n=1;n<=ERR_MAX;n++)
				if(!stricmp(begin,ename[n]))
					break;
			if(n>ERR_MAX)
				n=BAD_CHAR;
			new->n_x.v_int=n;
			byte_value=CONST_ERR;
		}
		*instr=ch;
		ch=L_CONST;
		break;

	default:
		if(!a0 && (ch=='@' || ch=='$'))
		   goto bad_chr;

		if(a0 && ch=='@') {
			begin=instr;
			while(*instr && (isalpha(*instr) || isdigit(*instr) || *instr=='_'))
				instr++;
			n=instr-begin;
		} else {
			begin=instr-1;
			byte_value=parse_cell_or_range(&begin,&(new->n_x.v_rng));
			if(byte_value) {
				if((byte_value& ~0x3)==R_CELL)
					ch=L_CELL;
				else
					ch=L_RANGE;
				instr=begin;
				break;
			}

			while(*instr && (isalpha(*instr) || isdigit(*instr) || *instr=='_'))
				instr++;

			n=instr-begin;
			while(isspace(*instr))
				instr++;

			if(*instr!='(') {
				ch=L_VAR;
				byte_value=VAR;
				new->n_x.v_var=find_or_make_var(begin,n);
				break;
			}
		}
		tmp_ch=begin[n];
		begin[n]='\0';
		fp=hash_find(parse_hash,begin);
		begin[n]=tmp_ch;
		byte_value= ERROR;
		if(!fp) {
			parse_error=BAD_FUNC;
			return ERROR;
		}

		if(fp>=the_funs && fp<=&the_funs[USR1])
			byte_value=fp-the_funs;
		else {
			for(nn=0;nn<n_usr_funs;nn++) {
				if(fp>=&usr_funs[nn][0] && fp<=&usr_funs[nn][usr_n_funs[nn]]) {
					byte_value=USR1+nn;
					new->sub_value=fp-&usr_funs[nn][0];
					break;
				}
			}
#ifdef TEST
			if(nn==n_usr_funs) {
				io_error_msg("Couln't turn fp into a ##");
				parse_error=BAD_FUNC;
				return ERROR;
			}
#endif
		}

		if(fp->fn_argn&X_J)
			ch= byte_value==F_IF ? L_FN3 : L_FN2;
		else if(fp->fn_argt[0]=='R' || fp->fn_argt[0]=='E')
			ch=L_FN1R-1+fp->fn_argn-X_A0;
		else
			ch=L_FN0 + fp->fn_argn-X_A0;

		break;
	}
	/* new->node_type=ch; */
	new->comp_value=byte_value;
	yylval=new;
	return ch;
}

/* Return value is
	0 if it doesn't look like a cell or a range,
	R_CELL if it is a cell (ptr now points past the characters, lr and lc hold the row and col of the cell)
	RANGE if it is a range (ptr points past the chars)
 */
unsigned char
parse_cell_or_range FUN2(char **,ptr, struct rng *,retp)
{
	if(a0) {
		unsigned tmpc,tmpr;
		char *p;
		int abz = ROWREL|COLREL;

		p= *ptr;
		tmpc=0;
		if(*p=='$') {
			abz-=COLREL;
			p++;
		}
		if(!isalpha(*p))
			return 0;
		tmpc=str_to_col(&p);
		if(tmpc<MIN_COL || tmpc>MAX_COL)
			return 0;
		if(*p=='$') {
			abz-=ROWREL;
			p++;
		}
		if(!isdigit(*p))
			return 0;
		for(tmpr=0;isdigit(*p);p++)
			tmpr=tmpr*10 + *p - '0';

		if(tmpr<MIN_ROW || tmpr>MAX_ROW)
			return 0;

		if(*p==':' || *p=='.') {
			unsigned tmpc1,tmpr1;

			abz = ((abz&COLREL) ? LCREL : 0)|((abz&ROWREL) ? LRREL : 0)|HRREL|HCREL;
			p++;
			if(*p=='$') {
				abz-=HCREL;
				p++;
			}
			if(!isalpha(*p))
				return 0;
			tmpc1=str_to_col(&p);
			if(tmpc1<MIN_COL || tmpc1>MAX_COL)
				return 0;
			if(*p=='$') {
				abz-=HRREL;
				p++;
			}
			if(!isdigit(*p))
				return 0;
			for(tmpr1=0;isdigit(*p);p++)
				tmpr1=tmpr1*10 + *p - '0';
			if(tmpr1<MIN_ROW || tmpr1>MAX_ROW)
				return 0;

			if(tmpr<tmpr1) {
				retp->lr=tmpr;
				retp->hr=tmpr1;
			} else {
				retp->lr=tmpr1;
				retp->hr=tmpr;
			}
			if(tmpc<tmpc1) {
				retp->lc=tmpc;
				retp->hc=tmpc1;
			} else {
				retp->lc=tmpc1;
				retp->hc=tmpc;
			}
			*ptr= p;
			return RANGE | abz;
		}
		retp->lr = retp->hr = tmpr;
		retp->lc = retp->hc = tmpc;
		*ptr=p;
		return R_CELL | abz;
	} else {
		char *p;
		unsigned char retr;
		unsigned char retc;
		int ended;
		long num;
		CELLREF tmp;

#define CK_ABS_R(x)	if((x)<MIN_ROW || (x)>MAX_ROW)	\
				return 0;		\
			else

#define CK_REL_R(x)	if(   ((x)>0 && MAX_ROW-(x)<cur_row)	\
			   || ((x)<0 && MIN_ROW-(x)>cur_row))	\
				return 0;			\
			else

#define CK_ABS_C(x)	if((x)<MIN_COL || (x)>MAX_COL)	\
				return 0;		\
			else

#define CK_REL_C(x)	if(   ((x)>0 && MAX_COL-(x)<cur_col)	\
			   || ((x)<0 && MIN_COL-(x)>cur_col))	\
				return 0;			\
			else

#define MAYBEREL(p) (*(p)=='[' && (isdigit((p)[1]) || (((p)[1]=='+' || (p)[1]=='-') && isdigit((p)[2]))))

		p= *ptr;
		retr=0;
		retc=0;
		ended=0;
		while(ended==0) {
			switch(*p) {
			case 'r':
			case 'R':
				if(retr) {
					ended++;
					break;
				}
				p++;
				retr=R_CELL;
				if(isdigit(*p)) {
					num=astol(&p);
					CK_ABS_R(num);
					retp->lr= retp->hr=num;
				} else if(MAYBEREL(p)) {
					p++;
					num=astol(&p);
					CK_REL_R(num);
					retp->lr= retp->hr=num+cur_row;
					retr|=ROWREL;
					if(*p==':') {
						retr=RANGE|LRREL|HRREL;
						p++;
						num=astol(&p);
						CK_REL_R(num);
						retp->hr=num+cur_row;
					}
					if(*p++!=']')
						return 0;
				} else if(retc || *p=='c' || *p=='C') {
					retr|=ROWREL;
					retp->lr= retp->hr=cur_row;
				} else
					return 0;
				if(*p==':' && retr!=(RANGE|LRREL|HRREL)) {
					retr= (retr&ROWREL) ? RANGE|LRREL : RANGE;
					p++;
					if(isdigit(*p)) {
						num=astol(&p);
						CK_ABS_R(num);
			 			retp->hr=num;
					} else if(MAYBEREL(p)) {
						p++;
						num=astol(&p);
						CK_REL_R(num);
						retp->hr=num+cur_row;
						retr|=HRREL;
						if(*p++!=']')
							return 0;
					} else
						return 0;
				}

				if(retc)
					ended++;
				break;

			case 'c':
			case 'C':
				if(retc) {
					ended++;
					break;
				}
				p++;
				retc=R_CELL;
				if(isdigit(*p)) {
					num=astol(&p);
					CK_ABS_C(num);
					retp->lc= retp->hc=num;
				} else if(MAYBEREL(p)) {
					p++;
					num=astol(&p);
					CK_REL_C(num);
					retp->lc= retp->hc=num+cur_col;
					retc|=COLREL;
					if(*p==':') {
						retc=RANGE|LCREL|HCREL;
						p++;
						num=astol(&p);
						CK_REL_C(num);
						retp->hc=num+cur_col;
					}
					if(*p++!=']')
						return 0;
				} else if(retr || *p=='r' || *p=='R') {
					retc|=COLREL;
					retp->lc= retp->hc=cur_col;
				} else
					return 0;
				if(*p==':' && retc!=(RANGE|LCREL|HCREL)) {
					retc= (retc&COLREL) ? RANGE|LCREL : RANGE;
					p++;
					if(isdigit(*p)) {
						num=astol(&p);
						CK_ABS_C(num);
			 			retp->hc=num;
					} else if(MAYBEREL(p)) {
						p++;
						num=astol(&p);
						CK_REL_C(num);
						retp->hc=num+cur_col;
						retc|=HCREL;
						if(*p++!=']')
							return 0;
					} else
						return 0;
				}

				if(retr)
					ended++;
				break;
			default:
				if(retr) {
					*ptr=p;
					retp->lc=MIN_COL;
					retp->hc=MAX_COL;
					if((retr|ROWREL)==(R_CELL|ROWREL))
						return (retr&ROWREL) ? (RANGE|LRREL|HRREL) : RANGE;
					else
						return retr;
				} else if(retc) {
					*ptr=p;
					retp->lr=MIN_ROW;
					retp->hr=MAX_COL;
					if((retc|COLREL)==(R_CELL|COLREL))
						return (retc&COLREL) ? (RANGE|LCREL|HCREL) : RANGE;
					else
						return retc;
				}
				return 0;
			}
		}
		if(!retr || !retc)
			return 0;
		*ptr=p;
		if(retp->lr>retp->hr)
			tmp=retp->lr,retp->lr=retp->hr,retp->hr=tmp;
		if(retp->lc>retp->hc)
			tmp=retp->lc,retp->lc=retp->hc,retp->hc=tmp;

		if((retr|ROWREL)==(R_CELL|ROWREL)) {
			if((retc|COLREL)==(R_CELL|COLREL))
				return retr|retc;
			return (retr&ROWREL) ? (retc|LRREL|HRREL) : retc;
		}
		if((retc|COLREL)==(R_CELL|COLREL))
			return (retc&COLREL) ? (retr|LCREL|HCREL) : retr;
		return retr|retc;
	}
}

int
str_to_col FUN1(char **,str)
{
	int ret;
	char c,cc,ccc;
#if MAX_COL>702
	char cccc;
#endif

	ret=0;
	c=str[0][0];
	if(!isalpha((cc=str[0][1]))) {
		(*str)++;
		return MIN_COL + (isupper(c) ? c-'A' : c-'a');
	}
	if(!isalpha((ccc=str[0][2]))) {
		(*str)+=2;
		return MIN_COL+26 + (isupper(c) ? c-'A' : c-'a')*26 + (isupper(cc) ? cc-'A' : cc-'a');
	}
#if MAX_COL>702
	if(!isalpha((cccc=str[0][3]))) {
		(*str)+=3;
		return MIN_COL+702 + (isupper(c) ? c-'A' : c-'a')*26*26 + (isupper(cc) ? cc-'A' : cc-'a')*26 + (isupper(ccc) ? ccc-'A' : ccc-'a');
	}
	if(!isalpha(str[0][4])) {
		(*str)+=4;
		return MIN_COL+18278 + (isupper(c) ? c-'A' : c-'a')*26*26*26 + (isupper(cc) ? cc-'A' : cc-'a')*26*26 + (isupper(ccc) ? ccc-'A' : ccc-'a')*26 + (isupper(cccc) ? cccc-'A' : cccc-'a');
	}
#endif
	return 0;
}
