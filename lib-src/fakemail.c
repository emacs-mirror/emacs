/* sendmail-like interface to /bin/mail for system V,
   Copyright (C) 1985, 1994 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#define NO_SHORTNAMES
#include <../src/config.h>

#if defined (BSD) && !defined (BSD4_1) && !defined (USE_FAKEMAIL)
/* This program isnot used in BSD, so just avoid loader complaints.  */
void
main ()
{
}
#else /* not BSD 4.2 (or newer) */
#ifdef MSDOS
void
main ()
{
}
#else /* not MSDOS */
/* This conditional contains all the rest of the file.  */

/* These are defined in config in some versions. */

#ifdef static
#undef static
#endif

#ifdef read
#undef read
#undef write
#undef open
#undef close
#endif

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <pwd.h>

/* Type definitions */

#define boolean int
#define true 1
#define false 0

/* Various lists */

struct line_record
{
  char *string;
  struct line_record *continuation;
};
typedef struct line_record *line_list;

struct header_record
{
  line_list text;
  struct header_record *next;
  struct header_record *previous;
};
typedef struct header_record *header;
			
struct stream_record
{
  FILE *handle;
  int (*action)();
  struct stream_record *rest_streams;
};
typedef struct stream_record *stream_list;

/* A `struct linebuffer' is a structure which holds a line of text.
 * `readline' reads a line from a stream into a linebuffer
 * and works regardless of the length of the line.
 */

struct linebuffer
{
  long size;
  char *buffer;
};

struct linebuffer lb;

#define new_list()					\
  ((line_list) xmalloc (sizeof (struct line_record)))
#define new_header()				\
  ((header) xmalloc (sizeof (struct header_record)))
#define new_stream()				\
  ((stream_list) xmalloc (sizeof (struct stream_record)))
#define alloc_string(nchars)				\
  ((char *) xmalloc ((nchars) + 1))

/* Global declarations */

#define BUFLEN 1024
#define KEYWORD_SIZE 256
#define FROM_PREFIX "From"
#define MY_NAME "fakemail"
#define NIL ((line_list) NULL)
#define INITIAL_LINE_SIZE 200

#ifndef MAIL_PROGRAM_NAME
#define MAIL_PROGRAM_NAME "/bin/mail"
#endif

static char *my_name;
static char *the_date;
static char *the_user;
static line_list file_preface;
static stream_list the_streams;
static boolean no_problems = true;

extern FILE *popen ();
extern int fclose (), pclose ();

#ifdef CURRENT_USER
extern struct passwd *getpwuid ();
extern unsigned short geteuid ();
static struct passwd *my_entry;
#define cuserid(s)				\
(my_entry = getpwuid (((int) geteuid ())),	\
 my_entry->pw_name)
#endif

/* Utilities */

/* Print error message.  `s1' is printf control string, `s2' is arg for it. */

static void
error (s1, s2)
     char *s1, *s2;
{
  printf ("%s: ", my_name);
  printf (s1, s2);
  printf ("\n");
  no_problems = false;
}

/* Print error message and exit.  */

static void
fatal (s1, s2)
     char *s1, *s2;
{
  error (s1, s2);
  exit (1);
}

/* Like malloc but get fatal error if memory is exhausted.  */

static char *
xmalloc (size)
     int size;
{
  char *result = (char *) malloc (((unsigned) size));
  if (result == ((char *) NULL))
    fatal ("virtual memory exhausted", 0);
  return result;
}

static char *
xrealloc (ptr, size)
     char *ptr;
     int size;
{
  char *result = (char *) realloc (ptr, ((unsigned) size));
  if (result == ((char *) NULL))
    fatal ("virtual memory exhausted");
  return result;
}

/* Initialize a linebuffer for use */

void
init_linebuffer (linebuffer)
     struct linebuffer *linebuffer;
{
  linebuffer->size = INITIAL_LINE_SIZE;
  linebuffer->buffer = ((char *) xmalloc (INITIAL_LINE_SIZE));
}

/* Read a line of text from `stream' into `linebuffer'.
 * Return the length of the line.  
 */

long
readline (linebuffer, stream)
     struct linebuffer *linebuffer;
     FILE *stream;
{
  char *buffer = linebuffer->buffer;
  char *p = linebuffer->buffer;
  char *end = p + linebuffer->size;

  while (true)
    {
      int c = getc (stream);
      if (p == end)
	{
	  linebuffer->size *= 2;
	  buffer = ((char *) xrealloc (buffer, linebuffer->size));
	  p = buffer + (p - linebuffer->buffer);
	  end = buffer + linebuffer->size;
	  linebuffer->buffer = buffer;
	}
      if (c < 0 || c == '\n')
	{
	  *p = 0;
	  break;
	}
      *p++ = c;
    }

  return p - buffer;
}

/* Extract a colon-terminated keyword from the string FIELD.
   Return that keyword as a string stored in a static buffer.
   Store the address of the rest of the string into *REST.

   If there is no keyword, return NULL and don't alter *REST.  */

char *
get_keyword (field, rest)
     register char *field;
     char **rest;
{
  static char keyword[KEYWORD_SIZE];
  register char *ptr;
  register char c;

  ptr = &keyword[0];
  c = *field++;
  if (isspace (c) || c == ':')
    return ((char *) NULL);
  *ptr++ = (islower (c) ? toupper (c) : c);
  while (((c = *field++) != ':') && ! isspace (c))
    *ptr++ = (islower (c) ? toupper (c) : c);
  *ptr++ = '\0';
  while (isspace (c))
    c = *field++;
  if (c != ':')
    return ((char *) NULL);
  *rest = field;
  return &keyword[0];
}

/* Nonzero if the string FIELD starts with a colon-terminated keyword.  */

boolean
has_keyword (field)
     char *field;
{
  char *ignored;
  return (get_keyword (field, &ignored) != ((char *) NULL));
}

/* Store the string FIELD, followed by any lines in THE_LIST,
   into the buffer WHERE.
   Concatenate lines, putting just a space between them.
   Delete everything contained in parentheses.
   When a recipient name contains <...>, we discard
   everything except what is inside the <...>.

   We don't pay attention to overflowing WHERE;
   the caller has to make it big enough.  */

char *
add_field (the_list, field, where)
     line_list the_list;
     register char *field, *where;
{
  register char c;
  while (true)
    {
      char *this_recipient_where;
      int in_quotes = 0;

      *where++ = ' ';
      this_recipient_where = where;

      while ((c = *field++) != '\0')
	{
	  if (c == '\\')
	    *where++ = c;
	  else if (c == '"')
	    {
	      in_quotes = ! in_quotes;
	      *where++ = c;
	    }
	  else if (in_quotes)
	    *where++ = c;
	  else if (c == '(')
	    {
	      while (*field && *field != ')') ++field;
	      if (! (*field++)) break; /* no close */
	      continue;
	    }
	  else if (c == ',')
	    {
	      *where++ = ' ';
	      /* When we get to the end of one recipient,
		 don't discard it if the next one has <...>.  */
	      this_recipient_where = where;
	    }
	  else if (c == '<')
	    /* Discard everything we got before the `<'.  */
	    where = this_recipient_where;
	  else if (c == '>')
	    /* Discard the rest of this name that follows the `>'.  */
	    {
	      while (*field && *field != ',') ++field;
	      if (! (*field++)) break; /* no comma */
	      continue;
	    }
	  else
	    *where++ = c;
	}
      if (the_list == NIL) break;
      field = the_list->string;
      the_list = the_list->continuation;
    }
  return where;
}

line_list
make_file_preface ()
{
  char *the_string, *temp;
  long idiotic_interface;
  long prefix_length;
  long user_length;
  long date_length;
  line_list result;

  prefix_length = strlen (FROM_PREFIX);
  time (&idiotic_interface);
  the_date = ctime (&idiotic_interface);
  /* the_date has an unwanted newline at the end */
  date_length = strlen (the_date) - 1;
  the_date[date_length] = '\0';
  temp = cuserid ((char *) NULL);
  user_length = strlen (temp);
  the_user = alloc_string (user_length + 1);
  strcpy (the_user, temp);
  the_string = alloc_string (3 + prefix_length +
			     user_length +
			     date_length);
  temp = the_string;
  strcpy (temp, FROM_PREFIX);
  temp = &temp[prefix_length];
  *temp++ = ' ';
  strcpy (temp, the_user);
  temp = &temp[user_length];
  *temp++ = ' ';
  strcpy (temp, the_date);
  result = new_list ();
  result->string = the_string;
  result->continuation = ((line_list) NULL);
  return result;
}

void
write_line_list (the_list, the_stream)
     register line_list the_list;
     FILE *the_stream;
{
  for ( ;
      the_list != ((line_list) NULL) ;
      the_list = the_list->continuation)
    {
      fputs (the_list->string, the_stream);
      putc ('\n', the_stream);
    }
  return;
}

int
close_the_streams ()
{
  register stream_list rem;
  for (rem = the_streams;
       rem != ((stream_list) NULL);
       rem = rem->rest_streams)
    no_problems = (no_problems &&
		   ((*rem->action) (rem->handle) == 0));
  the_streams = ((stream_list) NULL);
  return (no_problems ? 0 : 1);
}

void
add_a_stream (the_stream, closing_action)
     FILE *the_stream;
     int (*closing_action)();
{
  stream_list old = the_streams;
  the_streams = new_stream ();
  the_streams->handle = the_stream;
  the_streams->action = closing_action;
  the_streams->rest_streams = old;
  return;
}

int
my_fclose (the_file)
     FILE *the_file;
{
  putc ('\n', the_file);
  fflush (the_file);
  return fclose (the_file);
}

boolean
open_a_file (name)
     char *name;
{
  FILE *the_stream = fopen (name, "a");
  if (the_stream != ((FILE *) NULL))
    {
      add_a_stream (the_stream, my_fclose);
      if (the_user == ((char *) NULL))
	file_preface = make_file_preface ();
      write_line_list (file_preface, the_stream);
      return true;
    }
  return false;
}

void
put_string (s)
     char *s;
{
  register stream_list rem;
  for (rem = the_streams;
       rem != ((stream_list) NULL);
       rem = rem->rest_streams)
    fputs (s, rem->handle);
  return;
}

void
put_line (string)
     char *string;
{
  register stream_list rem;
  for (rem = the_streams;
       rem != ((stream_list) NULL);
       rem = rem->rest_streams)
    {
      char *s = string;
      int column = 0;

      /* Divide STRING into lines.  */
      while (*s != 0)
	{
	  char *breakpos;

	  /* Find the last char that fits.  */
	  for (breakpos = s; *breakpos && column < 78; ++breakpos)
	    {
	      if (*breakpos == '\t')
		column += 8;
	      else
		column++;
	    }
	  /* If we didn't reach end of line, break the line.  */
	  if (*breakpos)
	    {
	      /* Back up to just after the last comma that fits.  */
	      while (breakpos != s && breakpos[-1] != ',') --breakpos;

	      if (breakpos == s)
		{
		  /* If no comma fits, move past the first address anyway.  */
		  while (*breakpos != 0 && *breakpos != ',') ++breakpos;
		  if (*breakpos != 0)
		    /* Include the comma after it.  */
		    ++breakpos;
		}
	    }
	  /* Output that much, then break the line.  */
	  fwrite (s, 1, breakpos - s, rem->handle);
	  column = 8;

	  /* Skip whitespace and prepare to print more addresses.  */
	  s = breakpos;
	  while (*s == ' ' || *s == '\t') ++s;
          if (*s != 0)
	    fputs ("\n\t", rem->handle);
	}
      putc ('\n', rem->handle);
    }
  return;
}

#define mail_error error

/* Handle an FCC field.  FIELD is the text of the first line (after
   the header name), and THE_LIST holds the continuation lines if any.
   Call open_a_file for each file.  */

void
setup_files (the_list, field)
     register line_list the_list;
     register char *field;
{
  register char *start;
  register char c;
  while (true)
    {
      while (((c = *field) != '\0')
	     && (c == ' '
		 || c == '\t'
		 || c == ','))
	field += 1;
      if (c != '\0')
	{
	  start = field;
	  while (((c = *field) != '\0')
		 && c != ' '
		 && c != '\t'
		 && c != ',')
	    field += 1;
	  *field = '\0';
	  if (!open_a_file (start))
	    mail_error ("Could not open file %s", start);
	  *field = c;
	  if (c != '\0') continue;
	}
      if (the_list == ((line_list) NULL))
	return;
      field = the_list->string;
      the_list = the_list->continuation;
    }
}

/* Compute the total size of all recipient names stored in THE_HEADER.
   The result says how big to make the buffer to pass to parse_header.  */

int
args_size (the_header)
     header the_header;
{
  register header old = the_header;
  register line_list rem;
  register int size = 0;
  do
    {
      char *field;
      register char *keyword = get_keyword (the_header->text->string, &field);
      if ((strcmp (keyword, "TO") == 0)
	  || (strcmp (keyword, "CC") == 0)
	  || (strcmp (keyword, "BCC") == 0))
	{
	  size += 1 + strlen (field);
	  for (rem = the_header->text->continuation;
	       rem != NIL;
	       rem = rem->continuation)
	    size += 1 + strlen (rem->string);
	}
      the_header = the_header->next;
    } while (the_header != old);
  return size;
}

/* Scan the header described by the lists THE_HEADER,
   and put all recipient names into the buffer WHERE.
   Precede each recipient name with a space.

   Also, if the header has any FCC fields, call setup_files for each one.  */

parse_header (the_header, where)
     header the_header;
     register char *where;
{
  register header old = the_header;
  do
    {
      char *field;
      register char *keyword = get_keyword (the_header->text->string, &field);
      if (strcmp (keyword, "TO") == 0)
	where = add_field (the_header->text->continuation, field, where);
      else if (strcmp (keyword, "CC") == 0)
	where = add_field (the_header->text->continuation, field, where);
      else if (strcmp (keyword, "BCC") == 0)
	{
	  where = add_field (the_header->text->continuation, field, where);
	  the_header->previous->next = the_header->next;
	  the_header->next->previous = the_header->previous;
	}
      else if (strcmp (keyword, "FCC") == 0)
	setup_files (the_header->text->continuation, field);
      the_header = the_header->next;
    } while (the_header != old);
  *where = '\0';
  return;
}
    
/* Read lines from the input until we get a blank line.
   Create a list of `header' objects, one for each header field,
   each of which points to a list of `line_list' objects,
   one for each line in that field.
   Continuation lines are grouped in the headers they continue.  */
   
header
read_header ()
{
  register header the_header = ((header) NULL);
  register line_list *next_line = ((line_list *) NULL);

  init_linebuffer (&lb);

  do
    {
      long length;
      register char *line;

      readline (&lb, stdin);
      line = lb.buffer;
      length = strlen (line);
      if (length == 0) break;

      if (has_keyword (line))
	{
	  register header old = the_header;
	  the_header = new_header ();
	  if (old == ((header) NULL))
	    {
	      the_header->next = the_header;
	      the_header->previous = the_header;
	    }
	  else
	    {
	      the_header->previous = old;
	      the_header->next = old->next;
	      old->next = the_header;
	    }
	  next_line = &(the_header->text);
	}

      if (next_line == ((line_list *) NULL))
	{
	  /* Not a valid header */
	  exit (1);
	}
      *next_line = new_list ();
      (*next_line)->string = alloc_string (length);
      strcpy (((*next_line)->string), line);
      next_line = &((*next_line)->continuation);
      *next_line = NIL;

    } while (true);

  return the_header->next;
}

void
write_header (the_header)
     header the_header;
{
  register header old = the_header;
  do
    {
      register line_list the_list;
      for (the_list = the_header->text;
	   the_list != NIL;
	   the_list = the_list->continuation)
	put_line (the_list->string);
      the_header = the_header->next;
    } while (the_header != old);
  put_line ("");
  return;
}

void
main (argc, argv)
     int argc;
     char **argv;
{
  char *command_line;
  header the_header;
  long name_length;
  char *mail_program_name;
  char buf[BUFLEN + 1];
  register int size;
  FILE *the_pipe;

  extern char *getenv ();

  mail_program_name = getenv ("FAKEMAILER");
  if (!(mail_program_name && *mail_program_name))
    mail_program_name = MAIL_PROGRAM_NAME;
  name_length = strlen (mail_program_name);

  my_name = MY_NAME;
  the_streams = ((stream_list) NULL);
  the_date = ((char *) NULL);
  the_user = ((char *) NULL);

  the_header = read_header ();
  command_line = alloc_string (name_length + args_size (the_header));
  strcpy (command_line, mail_program_name);
  parse_header (the_header, &command_line[name_length]);
  
  the_pipe = popen (command_line, "w");
  if (the_pipe == ((FILE *) NULL))
    fatal ("cannot open pipe to real mailer");

  add_a_stream (the_pipe, pclose);

  write_header (the_header);

  /* Dump the message itself */

  while (!feof (stdin))
    {
      size = fread (buf, 1, BUFLEN, stdin);
      buf[size] = '\0';
      put_string (buf);
    }

  exit (close_the_streams ());
}

#endif /* not MSDOS */
#endif /* not BSD 4.2 (or newer) */
