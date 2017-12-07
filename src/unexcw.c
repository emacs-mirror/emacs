/* unexec() support for Cygwin;
   complete rewrite of xemacs Cygwin unexec() code

   Copyright (C) 2004-2017 Free Software Foundation, Inc.

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

#include <config.h>
#include "unexec.h"
#include "lisp.h"
#include <stdio.h>
#include <fcntl.h>
#include <a.out.h>
#include <unistd.h>
#include <assert.h>

#define DOTEXE ".exe"

/*
** header for Windows executable files
*/
typedef struct
{
  FILHDR file_header;
  PEAOUTHDR file_optional_header;
  SCNHDR section_header[32];
} exe_header_t;

int debug_unexcw = 0;

/*
** Read the header from the executable into memory so we can more easily access it.
*/
static exe_header_t *
read_exe_header (int fd, exe_header_t * exe_header_buffer)
{
  int i;
  int ret;

  assert (fd >= 0);
  assert (exe_header_buffer != 0);

  ret = lseek (fd, 0L, SEEK_SET);
  assert (ret != -1);

  ret =
    read (fd, &exe_header_buffer->file_header,
	  sizeof (exe_header_buffer->file_header));
  assert (ret == sizeof (exe_header_buffer->file_header));

  assert (exe_header_buffer->file_header.e_magic == 0x5a4d);
  assert (exe_header_buffer->file_header.nt_signature == 0x4550);
#ifdef __x86_64__
  assert (exe_header_buffer->file_header.f_magic == 0x8664);
#else
  assert (exe_header_buffer->file_header.f_magic == 0x014c);
#endif
  assert (exe_header_buffer->file_header.f_nscns > 0);
  assert (exe_header_buffer->file_header.f_nscns <=
          ARRAYELTS (exe_header_buffer->section_header));
  assert (exe_header_buffer->file_header.f_opthdr > 0);

  ret =
    read (fd, &exe_header_buffer->file_optional_header,
	  sizeof (exe_header_buffer->file_optional_header));
  assert (ret == sizeof (exe_header_buffer->file_optional_header));

#ifdef __x86_64__
  assert (exe_header_buffer->file_optional_header.magic == 0x020b);
#else
  assert (exe_header_buffer->file_optional_header.magic == 0x010b);
#endif

  for (i = 0; i < exe_header_buffer->file_header.f_nscns; ++i)
    {
      ret =
	read (fd, &exe_header_buffer->section_header[i],
	      sizeof (exe_header_buffer->section_header[i]));
      assert (ret == sizeof (exe_header_buffer->section_header[i]));
    }

  return (exe_header_buffer);
}

/*
** Fix the dumped emacs executable:
**
** - copy .data section data of interest from running executable into
**   output .exe file
**
** - convert .bss section into an initialized data section (like
**   .data) and copy .bss section data of interest from running
**   executable into output .exe file
*/
static void
fixup_executable (int fd)
{
  exe_header_t exe_header_buffer;
  exe_header_t *exe_header;
  int i;
  int ret;
  int found_data = 0;
  int found_bss = 0;

  exe_header = read_exe_header (fd, &exe_header_buffer);
  assert (exe_header != 0);

  assert (exe_header->file_header.f_nscns > 0);
  for (i = 0; i < exe_header->file_header.f_nscns; ++i)
    {
      unsigned long start_address =
	exe_header->section_header[i].s_vaddr +
	exe_header->file_optional_header.ImageBase;
      unsigned long end_address =
	exe_header->section_header[i].s_vaddr +
	exe_header->file_optional_header.ImageBase +
	exe_header->section_header[i].s_paddr;
      if (debug_unexcw)
	printf ("%8s start %#lx end %#lx\n",
		exe_header->section_header[i].s_name,
		start_address, end_address);
      if (my_edata >= (char *) start_address
	  && my_edata < (char *) end_address)
	{
	  /* data section */
	  ret =
	    lseek (fd, (long) (exe_header->section_header[i].s_scnptr),
		   SEEK_SET);
	  assert (ret != -1);
	  ret =
	    write (fd, (char *) start_address,
		   my_edata - (char *) start_address);
	  assert (ret == my_edata - (char *) start_address);
	  ++found_data;
	  if (debug_unexcw)
	    printf ("         .data, mem start %#lx mem length %td\n",
		    start_address, my_edata - (char *) start_address);
	  if (debug_unexcw)
	    printf ("         .data, file start %d file length %d\n",
		    (int) exe_header->section_header[i].s_scnptr,
		    (int) exe_header->section_header[i].s_paddr);
	}
      else if (my_endbss >= (char *) start_address
	       && my_endbss < (char *) end_address)
	{
	  /* bss section */
	  ++found_bss;
	  if (exe_header->section_header[i].s_flags & 0x00000080)
	    {
	      /* convert uninitialized data section to initialized data section */
	      struct stat statbuf;
	      ret = fstat (fd, &statbuf);
	      assert (ret != -1);

	      exe_header->section_header[i].s_flags &= ~0x00000080;
	      exe_header->section_header[i].s_flags |= 0x00000040;

	      exe_header->section_header[i].s_scnptr =
		(statbuf.st_size +
		 exe_header->file_optional_header.FileAlignment) /
		exe_header->file_optional_header.FileAlignment *
		exe_header->file_optional_header.FileAlignment;

	      exe_header->section_header[i].s_size =
		(exe_header->section_header[i].s_paddr +
		 exe_header->file_optional_header.FileAlignment) /
		exe_header->file_optional_header.FileAlignment *
		exe_header->file_optional_header.FileAlignment;

              /* Make sure the generated bootstrap binary isn't
               * sparse.  NT doesn't use a file cache for sparse
               * executables, so if we bootstrap Emacs using a sparse
               * bootstrap-emacs.exe, bootstrap takes about twenty
               * times longer than it would otherwise.  */

              ret = posix_fallocate (fd,
                                     ( exe_header->section_header[i].s_scnptr +
                                       exe_header->section_header[i].s_size ),
                                     1);

              assert (ret != -1);

	      ret =
		lseek (fd,
		       (long) (exe_header->section_header[i].s_scnptr +
			       exe_header->section_header[i].s_size - 1),
		       SEEK_SET);
	      assert (ret != -1);
	      ret = write (fd, "", 1);
	      assert (ret == 1);

	      ret =
		lseek (fd,
		       (long) ((char *) &exe_header->section_header[i] -
			       (char *) exe_header), SEEK_SET);
	      assert (ret != -1);
	      ret =
		write (fd, &exe_header->section_header[i],
		       sizeof (exe_header->section_header[i]));
	      assert (ret == sizeof (exe_header->section_header[i]));
	      if (debug_unexcw)
		printf ("         seek to %ld, write %zu\n",
			(long) ((char *) &exe_header->section_header[i] -
				(char *) exe_header),
			sizeof (exe_header->section_header[i]));
	    }
	  /* write initialized data section */
	  ret =
	    lseek (fd, (long) (exe_header->section_header[i].s_scnptr),
		   SEEK_SET);
	  assert (ret != -1);
	  ret =
	    write (fd, (char *) start_address,
		   my_endbss - (char *) start_address);
	  assert (ret == (my_endbss - (char *) start_address));
	  if (debug_unexcw)
	    printf ("         .bss, mem start %#lx mem length %td\n",
		    start_address, my_endbss - (char *) start_address);
	  if (debug_unexcw)
	    printf ("         .bss, file start %d file length %d\n",
		    (int) exe_header->section_header[i].s_scnptr,
		    (int) exe_header->section_header[i].s_paddr);
	}
    }
  assert (found_bss == 1);
  assert (found_data == 1);
}

/*
** Windows likes .exe suffixes on executables.
*/
static char *
add_exe_suffix_if_necessary (const char *name, char *modified)
{
  int i = strlen (name);
  if (i <= (sizeof (DOTEXE) - 1))
    {
      sprintf (modified, "%s%s", name, DOTEXE);
    }
  else if (!strcasecmp (name + i - (sizeof (DOTEXE) - 1), DOTEXE))
    {
      strcpy (modified, name);
    }
  else
    {
      sprintf (modified, "%s%s", name, DOTEXE);
    }
  return (modified);
}

void
unexec (const char *outfile, const char *infile)
{
  char infile_buffer[FILENAME_MAX];
  char outfile_buffer[FILENAME_MAX];
  int fd_in;
  int fd_out;
  int ret;
  int ret2;

  infile = add_exe_suffix_if_necessary (infile, infile_buffer);
  outfile = add_exe_suffix_if_necessary (outfile, outfile_buffer);

  fd_in = emacs_open (infile, O_RDONLY, 0);
  assert (fd_in >= 0);
  fd_out = emacs_open (outfile, O_RDWR | O_TRUNC | O_CREAT, 0755);
  assert (fd_out >= 0);
  for (;;)
    {
      char buffer[4096];
      ret = read (fd_in, buffer, sizeof (buffer));
      if (ret == 0)
	{
	  /* eof */
	  break;
	}
      assert (ret > 0);
      /* data */
      ret2 = write (fd_out, buffer, ret);
      assert (ret2 == ret);
    }
  ret = emacs_close (fd_in);
  assert (ret == 0);

  fixup_executable (fd_out);

  ret = emacs_close (fd_out);
  assert (ret == 0);
}
