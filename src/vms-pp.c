/* vms_pp - preprocess emacs files in such a way that they can be
 *          compiled on VMS without warnings.
 * Copyright (C) 1986 Free Software Foundation, Inc.
   
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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

 *
 * Usage:
 *	vms_pp infile outfile
 * implicit inputs:
 *	The file "vms_pp.trans" has the names and their translations.
 * description:
 *	Vms_pp takes the input file and scans it, replacing the long
 *	names with shorter names according to the table read in from
 *	vms_pp.trans. The line is then written to the output file.
 *
 *	Additionally, the "#undef foo" construct is replaced with:
 *		#ifdef foo
 *		#undef foo
 *		#endif
 *
 *	The construct #if defined(foo) is replaced with
 *		#ifdef foo
 *		#define foo_VAL 1
 *		#else
 *		#define foo_VAL 0
 *		#endif
 *		#define defined(XX) XX_val
 *		#if defined(foo)
 *
 *	This last contruction only works on single line #if's and takes
 *	advantage of a questionable C pre-processor trick. If there are
 *	comments within the #if, that contain "defined", then this will
 *	bomb.
 */
#include <stdio.h>

#define Max_table 100
#define Table_name "vms_pp.trans"
#define Word_member \
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$"

static   FILE *in,*out;			/* read from, write to */
struct item {				/* symbol table entries */
  char *name;
  char *value;
};
static struct item name_table[Max_table]; /* symbol table */
static int defined_defined = 0;		/* small optimization */

main(argc,argv) int argc; char **argv; {
  char buffer[1024];

  if(argc != 3) {			/* check argument count */
    fprintf(stderr,"usage: vms_pp infile outfile");
    exit();
  }
  init_table();				/* read in translation table */

/* open input and output files
 */
  if((in = fopen(argv[1],"r")) == NULL) {
    fprintf(stderr,"vms_pp: unable to open file '%s'",argv[1]);
    exit();
  }
  if((out = fopen(argv[2],"w")) == NULL) {
    fprintf(stderr,"vms_pp: unable to create file '%s'",argv[2]);
    exit();
  }

  while(fgets(buffer,1023,in) != NULL) { /* loop through buffer until end */
    process_line(buffer);		/* process the line */
    fputs(buffer,out);			/* write out the line */
  }
}

/* buy - allocate and copy a string
 */
static char *buy(str) char *str; {
  char *temp;

  if(!(temp = malloc(strlen(str)+1))) {
    fprintf(stderr,"vms_pp: can't allocate memory");
    exit();
  }
  strcpy(temp,str);
  return temp;
}

/* gather_word - return a buffer full of the next word
 */
static char *gather_word(ptr,word) char *ptr, *word;{
  for(; strchr(Word_member,*ptr); ptr++,word++)
    *word = *ptr;
  *word = 0;
  return ptr;
}

/* skip_white - skip white space
 */
static char *skip_white(ptr) char *ptr; {
  while(*ptr == ' ' || *ptr == '\t')
    ptr++;
  return ptr;
}

/* init_table - initialize translation table.
 */
init_table() {
  char buf[256],*ptr,word[128];
  FILE *in;
  int i;

  if((in = fopen(Table_name,"r")) == NULL) { /* open file */
    fprintf(stderr,"vms_pp: can't open '%s'",Table_name);
    exit();
  }
  for(i = 0; fgets(buf,255,in) != NULL;) { /* loop through lines */
    ptr = skip_white(buf);
    if(*ptr == '!')			/* skip comments */
      continue;
    ptr = gather_word(ptr,word);	/* get long word */
    if(*word == 0) {			/* bad entry */
      fprintf(stderr,"vms_pp: bad input line '%s'\n",buf);
      continue;
    }
    name_table[i].name = buy(word);	/* set up the name */
    ptr = skip_white(ptr);		/* skip white space */
    ptr = gather_word(ptr,word);	/* get equivalent name */
    if(*word == 0) {			/* bad entry */
      fprintf(stderr,"vms_pp: bad input line '%s'\n",buf);
      continue;
    }
    name_table[i].value = buy(word);	/* and the equivalent name */
    i++;				/* increment to next position */
  }
  for(; i < Max_table; i++)		/* mark rest as unused */
    name_table[i].name = 0;
}

/* process_line - do actual line processing
 */
process_line(buf) char *buf; {
  char *in_ptr,*out_ptr;
  char word[128],*ptr;
  int len;

  check_pp(buf);			/* check for preprocessor lines */

  for(in_ptr = out_ptr = buf; *in_ptr;) {
    if(!strchr(Word_member,*in_ptr))	/* non alpha-numeric? just copy */
      *out_ptr++ = *in_ptr++;
    else {
      in_ptr = gather_word(in_ptr,word); /* get the 'word' */
      if(strlen(word) > 31)		/* length is too long */
	replace_word(word);		/* replace the word */
      for(ptr = word; *ptr; ptr++,out_ptr++) /* copy out the word */
	  *out_ptr = *ptr;
    }
  }
  *out_ptr = 0;
}

/* check_pp - check for preprocessor lines
 */
check_pp(buf) char *buf; {
  char *ptr,*p;
  char word[128];

  ptr = skip_white(buf);		/* skip white space */
  if(*ptr != '#')			/* is this a preprocessor line? */
    return;				/* no, just return */

  ptr = skip_white(++ptr);		/* skip white */
  ptr = gather_word(ptr,word);		/* get command word */
  if(!strcmp("undef",word)) {		/* undef? */
    ptr = skip_white(ptr);
    ptr = gather_word(ptr,word);	/* get the symbol to undef */
    fprintf(out,"#ifdef %s\n",word);
    fputs(buf,out);
    strcpy(buf,"#endif");
    return;
  }
  if(!strcmp("if",word)) {		/* check for if */
    for(;;) {
      ptr = strchr(ptr,'d');		/* look for d in defined */
      if(!ptr)				/* are we done? */
	return;
      if(strchr(Word_member,*(ptr-1))){	/* at beginning of word? */
	ptr++; continue;		/* no, continue looking */
      }
      ptr = gather_word(ptr,word);	/* get the word */
      if(strcmp(word,"defined"))	/* skip if not defined */
	continue;
      ptr = skip_white(ptr);		/* skip white */
      if(*ptr != '(')			/* look for open paren */
	continue;			/* error, continue */
      ptr++;				/* skip paren */
      ptr = skip_white(ptr);		/* more white skipping */
      ptr = gather_word(ptr,word);	/* get the thing to test */
      if(!*word)			/* null word is bad */
	continue;
      fprintf(out,"#ifdef %s\n",word);	/* generate the code */
      fprintf(out,"#define %s_VAL 1\n",word);
      fprintf(out,"#else\n");
      fprintf(out,"#define %s_VAL 0\n",word);
      fprintf(out,"#endif\n");
      if(!defined_defined) {
	fprintf(out,"#define defined(XXX) XXX/**/_VAL\n");
	defined_defined = 1;
      }
    }
  }
}

/* replace_word - look the word up in the table, and replace it
 *		  if a match is found.
 */
replace_word(word) char *word; {
  int i;

  for(i = 0; i < Max_table && name_table[i].name; i++)
    if(!strcmp(word,name_table[i].name)) {
      strcpy(word,name_table[i].value);
      return;
    }
  fprintf(stderr,"couldn't find '%s'\n",word);
}
