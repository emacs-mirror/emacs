%Start ErrorText ErrorMessage OtherText

EC	[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9]
D	[0-9]
D3	[0-9 ][0-9 ][0-9]
D4	[0-9 ][0-9 ][0-9 ][0-9]
D5	[0-9 ][0-9 ][0-9 ][0-9 ][0-9]
DS	[0-9 ]

%{
/* moore@wilma.cs.utk.edu

 * Hack to work around the AIX C compiler's brain-damaged error messages
 * so that emacs can parse them.  It runs /bin/cc as a subprocess, and
 * tries to rearrange the error messages so that (a) each message contains
 * both the filename and line number where the error occurred, and (b)
 * the error message(s) for a particular line get displayed *before* the
 * line itself.
 *
 * to compile: 
 * lex aixcc.lex
 * cc -o aixcc lex.yy.c
 *
 *
 * Copyright December 1991 by Keith Moore
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 * TODO: figure out how the compiler counts file numbers for included
 * files, keep track of which file corresponds to which number, and
 * always output the right file name.
 */

#include <stdio.h>
#include <string.h>

char *current_file;
int line;
int debug = 0;
char bigbuf[10240];
char *bufptr = bigbuf;
int last_line_was_error = 0;

spaces (s)
char *s;
{
    while (*s++)
	*bufptr++ = ' ';
}

char *
strsave (s)
char *s;
{
    char *ptr = malloc (strlen (s) + 1);
    strcpy (ptr, s);
    return ptr;
}

yywrap ()
{
    *bufptr = '\0';
    bufptr = bigbuf;
    while (*bufptr)
	putc (*bufptr++, yyout);
    return 1;
}

%}
%%
^File\ Line\ Column\ Message\ text[^\n]*	{
   /*
    * ignore this.  don't treat it as error text
    */
}

^{DS}{DS}{DS}\ {D5}\ \|	{
    /*
     * (optional) nesting level, followed by line number, followed
     * by the source code fragment that caused the error
     */

    /*
     * save the line number for later
     */
    line = atoi (yytext+4);

    if (debug) {
	fprintf (yyout, "line <= %d\n", line);
	fprintf (yyout, "%s\n", yytext);
    }

    /*
     * if the last line was an error message, to flush out all of
     * the old source text before starting to save the new source text.
     */
    if (last_line_was_error) {
	*bufptr = '\0';
	bufptr = bigbuf;
	while (*bufptr)
	    putc (*bufptr++, yyout);
	bufptr = bigbuf;
        last_line_was_error = 0;
    }
    /*
     * stuff enough spaces in the text buffer so that the
     * saved text will line up properly when displayed.
     */
    spaces (yytext);

    BEGIN ErrorText;	/* continue below */
}

<ErrorText>[^\n]*$	{
    char *ptr;

    /* 
     * Save the text until we see the error message(s), then print it.
     * This because emacs puts the error message at the top of the
     * window, and it's nice to be able to see the text below it.
     */

    ptr = yytext;
    while (*ptr)
	*bufptr++ = *ptr++;
    *bufptr++ = '\n';

    BEGIN 0;
}

^Processing\ include\ file\ .*$	{
    /*
     * name of a new include file being processed.  Increment file number
     * and remember the file name corresponding to this file number.
     */

    current_file = strsave (yytext+24);
    
    if (debug) {
	fprintf (yyout, "current_file <= %s\n", current_file);
	fprintf (yyout, "%s\n", yytext);
    }
}

^([a-z]\ -)?\ *{EC}:	{
    /* 
     * error message (which we print immediately) preceded by an
     * error code (which we ignore)
     */

    fprintf (yyout, "\"%s\", line %d: %c -", current_file, line, *yytext);
    last_line_was_error = 1;
    BEGIN ErrorMessage;
}

^{D3}\ {D5}\ {D4}\ {EC}:	{
    /*
     * (optional) nesting level, followed by line number, followed
     * by column number, followed by error message text.
     */

    /*
     * save the line number for later
     */
    line = atoi (yytext+4);

    if (debug) {
	fprintf (yyout, "line <= %d\n", line);
	fprintf (yyout, "%s\n", yytext);
    }

    /*
     * if the last line was an error message, flush out all of
     * the old source text before printing this error message.
     */
    if (last_line_was_error) {
	*bufptr = '\0';
	bufptr = bigbuf;
	while (*bufptr)
	    putc (*bufptr++, yyout);
	bufptr = bigbuf;
        last_line_was_error = 0;
    }
    fprintf (yyout, "\"%s\", line %d:", current_file, line);
    last_line_was_error = 1;
    BEGIN ErrorMessage;
}

<ErrorMessage>[^\n]*$	{
    fprintf (yyout, "%s\n", yytext);
    BEGIN 0;
}


^[^ :]+".c:"\ *$	{
    /* name of new source file being processed */

    char *ptr;

    if (current_file)
	free (current_file);
    ptr = strchr (yytext, ':');
    *ptr = '\0';
    current_file = strsave (yytext);
}

^[^\n]	{
    /*
     * other text starting with a newline.  We have to break it up this
     * way to keep this rule from matching any of the above patterns
     */

    if (last_line_was_error) {
	*bufptr = '\0';
	bufptr = bigbuf;
	while (*bufptr)
	    putc (*bufptr++, yyout);
	bufptr = bigbuf;
        last_line_was_error = 0;
    }

    *bufptr++ = *yytext;
    BEGIN OtherText;
}

<OtherText>[^\n]*$	{
    char *ptr;

    ptr = yytext;
    while (*ptr)
	*bufptr++ = *ptr++;
    *bufptr++ = '\n';

    BEGIN 0;
}

\n	;

%%

main (argc, argv)
char **argv;
{
    int pfd[2];
    int child_pid;
    int i;

    current_file = strsave ("/dev/null");

    line = 0;

    for (i = 1; i < argc; ++i) {
	char *ptr = strrchr (argv[i], '.');
	if (ptr && ptr[1] == 'c' && ptr[2] == '\0') {
	    current_file = strsave (argv[i]);
	    break;
	}
    }

    if (pipe (pfd) < 0) {
	perror ("pipe");
	exit (1);
    }
    if ((child_pid = fork()) > 0) {
	int status;

	close (pfd[1]);
	yyin = fdopen (pfd[0], "r");
	yyout = stderr;
	yylex();

	wait (&status);
	exit ((status >> 8) & 0xff);
    }
    else if (child_pid == 0) {
	dup2 (pfd[1], 2);
	close (pfd[0]);
	close (pfd[1]);
	argv[0] = "cc";
	execv ("/bin/cc", argv);
	perror ("/bin/cc");
	exit (1);
    }
    else {
	perror ("fork");
	exit (1);
    }
}
