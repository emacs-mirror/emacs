/* Unexec for Sunos 4 using shared libraries.
   Copyright (C) 1990, 1994 Free Software Foundation, Inc.

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

/* Contributed by Viktor Dukhovni.  */
/*
 * Unexec for Berkeley a.out format + SUNOS shared libraries
 * The unexeced executable contains the __DYNAMIC area from the
 * original text file,  and then the rest of data + bss + malloced area of
 * the current process.  (The __DYNAMIC area is at the top of the process
 * data segment,  we use "data_start" defined externally to mark the start
 * of the "real" data segment.)
 *
 * For programs that want to remap some of the data segment read only
 * a run_time_remap is provided.  This attempts to remap largest area starting
 * and ending on page boundaries between "data_start" and "bndry"
 * For this it to figure out where the text file is located.  A path search
 * is attempted after trying argv[0] and if all fails we simply do not remap
 *
 * One feature of run_time_remap () is mandatory:  reseting the break.
 *
 *  Note that we can no longer map data into the text segment,  as this causes
 *  the __DYNAMIC struct to become read only,  breaking the runtime loader.
 *  Thus we no longer need to mess with a private crt0.c,  the standard one
 *  will do just fine,  since environ can live in the writable area between
 *  __DYNAMIC and data_start,  just make sure that pre-crt0.o (the name
 *  is somewhat abused here) is loaded first!
 *
 */
#include <sys/param.h>
#include <sys/mman.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <string.h>
#include <stdio.h>
#include <a.out.h>

/* Do this after the above #include's in case a configuration file wants
   to define things for this file based on what <a.out.h> defines.  */
#ifdef emacs
#include <config.h>
#endif

#if defined (SUNOS4) || defined (__FreeBSD__)
#define UNDO_RELOCATION
#endif

#ifdef UNDO_RELOCATION
#include <link.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* NetBSD needs this bit, but SunOS does not have it.  */
#ifndef MAP_FILE
#define MAP_FILE 0
#endif


/*
 * for programs other than emacs
 * define data_start + initialized here,  and make sure
 * this object is loaded first!
 * emacs will define these elsewhere,  and load the object containing
 * data_start (pre-crt0.o or firstfile.o?) first!
 * The custom crt0.o *must not* be loaded!
 */
#ifndef emacs
  static int data_start = 0;
  static int initialized = 0;
#else
  extern int initialized;
  extern unsigned data_start;
  extern int pureptr;
#endif

extern char *getenv ();
static unsigned brk_value;
static struct exec nhdr;
static int rd_only_len;
static long cookie;


unexec (new_name, a_name, bndry, bss_start, entry) 
     char *new_name, *a_name;
     unsigned bndry, bss_start, entry;
{
  int fd, new;
  char *old;
  struct exec ohdr;		/* Allocate on the stack,  not needed in the next life */
  struct stat stat;

  if ((fd = open (a_name, O_RDONLY)) < 0)
    {
      fprintf (stderr, "%s: open: ", a_name);
      perror (a_name);
      exit (1);
    }
  if ((new = open (new_name, O_WRONLY | O_CREAT, 0666)) == -1)
    {
      fprintf (stderr, "%s: open: ", a_name);
      perror (new_name);
      exit (1);
    }

  if ((fstat (fd, &stat) == -1))
    {
      fprintf (stderr, "%s: ", a_name);
      perror ("fstat");
      exit (1);
    }

  old = (char *)mmap (0, stat.st_size, PROT_READ, MAP_FILE|MAP_SHARED, fd, 0);
  if (old == (char *)-1)
    {
      fprintf (stderr, "%s: ", a_name);
      perror ("mmap");
      exit (1);
    }
  close (fd);

  nhdr = ohdr = (*(struct exec *)old);


  /*
   * Remember a magic cookie so we know we've got the right binary
   * when remapping.
   */
  cookie = time (0);

  /* Save the break, it is reset to &_end (by ld.so?).  */
  brk_value = (unsigned) sbrk (0);

  /*
   * Round up data start to a page boundary (Lose if not a 2 power!)
   */
  data_start = ((((int)&data_start) - 1) & ~(N_PAGSIZ (nhdr) - 1)) + N_PAGSIZ (nhdr);

  /*
   * Round down read only pages to a multiple of the page size
   */
  if (bndry)
    rd_only_len = ((int)bndry & ~(N_PAGSIZ (nhdr) - 1)) - data_start;

#ifndef emacs
  /* Have to do this some time before dumping the data */
  initialized = 1;
#endif
  
  /* Handle new data and bss sizes and optional new entry point.
     No one actually uses bss_start and entry,  but tradition compels
     one to support them.
     Could complain if bss_start > brk_value,
     but the caller is *supposed* to know what she is doing.  */
  nhdr.a_data = (bss_start ? bss_start : brk_value) - N_DATADDR (nhdr);
  nhdr.a_bss  = bss_start ? brk_value - bss_start : 0;
  if (entry) 
    nhdr.a_entry = entry;

  /*
   * Write out the text segment with new header
   * Dynamic executables are ZMAGIC with N_TXTOFF==0 and the header
   * part of the text segment, but no need to rely on this.
   * So write the TEXT first,  then go back replace the header.
   * Doing it in the other order is less general!
   */
  lseek (new, N_TXTOFF (nhdr), L_SET);
  write (new, old + N_TXTOFF (ohdr), N_TXTOFF (ohdr) + ohdr.a_text);
  lseek (new, 0L, L_SET);
  write (new, &nhdr, sizeof (nhdr));

  /*
   * Write out the head of the old data segment from the file not
   * from core, this has the unresolved __DYNAMIC relocation data
   * we need to reload
   */
  lseek (new, N_DATOFF (nhdr), L_SET);
  write (new, old + N_DATOFF (ohdr), (int)&data_start - N_DATADDR (ohdr));

  /*
   * Copy the rest of the data from core
   */
  write (new, &data_start, N_BSSADDR (nhdr) - (int)&data_start);

  /*
   * Copy the symbol table and line numbers
   */
  lseek (new, N_TRELOFF (nhdr), L_SET);
  write (new, old + N_TRELOFF (ohdr), stat.st_size - N_TRELOFF (ohdr));

  /* Some other BSD systems use this file.
     We don't know whether this change is right for them.  */
#ifdef UNDO_RELOCATION
  /* Undo the relocations done at startup by ld.so.
     It will do these relocations again when we start the dumped Emacs.
     Doing them twice gives incorrect results.  */
  {
    unsigned long daddr = N_DATADDR (ohdr);
    unsigned long rel, erel;
#ifdef SUNOS4
    extern struct link_dynamic _DYNAMIC;

    /*  SunOS4.x's ld_rel is relative to N_TXTADDR. */
    if (_DYNAMIC.ld_version < 2)
      {
	rel = _DYNAMIC.ld_un.ld_1->ld_rel + N_TXTADDR (ohdr);
	erel = _DYNAMIC.ld_un.ld_1->ld_hash + N_TXTADDR (ohdr);
      }
    else
      {
	rel = _DYNAMIC.ld_un.ld_2->ld_rel + N_TXTADDR (ohdr);
	erel = _DYNAMIC.ld_un.ld_2->ld_hash + N_TXTADDR (ohdr);
      }
#ifdef sparc
#define REL_INFO_TYPE		struct reloc_info_sparc
#else
#define REL_INFO_TYPE		struct relocation_info
#endif /* sparc */
#define REL_TARGET_ADDRESS(r)	(((REL_INFO_TYPE *)(r))->r_address)
#endif /* SUNOS4 */
#ifdef __FreeBSD__
    extern struct _dynamic _DYNAMIC;

    /*  FreeBSD's LD_REL is a virtual address itself. */
    rel = LD_REL (&_DYNAMIC);
    erel = rel + LD_RELSZ (&_DYNAMIC);
#define REL_INFO_TYPE		struct relocation_info
#define REL_TARGET_ADDRESS(r)	(((REL_INFO_TYPE *)(r))->r_address)
#endif

    for (; rel < erel; rel += sizeof (REL_INFO_TYPE))
      {
	/*  This is the virtual address where ld.so will do relocation.  */
	unsigned long target = REL_TARGET_ADDRESS (rel);
	/*  This is the offset in the data segment.  */
	unsigned long segoffset = target - daddr;

	/*  If it is located below data_start, we have to do nothing here,
	    because the old data has been already written to the location. */
	if (target < (unsigned long)&data_start)
	    continue;

	lseek (new, N_DATOFF (nhdr) + segoffset, L_SET);
	write (new, old + N_DATOFF (ohdr) + segoffset, sizeof (unsigned long));
      }
  }
#endif /* UNDO_RELOCATION */

  fchmod (new, 0755);
}

void
run_time_remap (progname)
     char *progname;
{
  char aout[MAXPATHLEN];
  register char *path, *p;

  /* Just in case */
  if (!initialized)
    return;

  /* Restore the break */
  brk ((char *) brk_value);

  /*  If nothing to remap:  we are done! */
  if (rd_only_len == 0)
    return;

  /*
   * Attempt to find the executable
   * First try argv[0],  will almost always succeed as shells tend to give
   * the full path from the hash list rather than using execvp ()
   */
  if (is_it (progname)) 
    return;

  /*
   * If argv[0] is a full path and does not exist,  not much sense in
   * searching further
   */
  if (strchr (progname, '/')) 
    return;

  /*
   * Try to search for  argv[0] on the PATH
   */
  path = getenv ("PATH");
  if (path == NULL)
    return;

  while (*path)
    {
      /* copy through ':' or end */
      for (p = aout; *p = *path; ++p, ++path)
	if (*p == ':')
	  {
	    ++path;		/* move past ':' */
	    break;
	  }
      *p++ = '/';
      strcpy (p, progname);
      /*
       * aout is a candidate full path name
       */
      if (is_it (aout))
	return;
    }
}

is_it (filename)
  char *filename;
{
  int fd;
  long filenames_cookie;
  struct exec hdr;

  /*
   * Open an executable  and check for a valid header!
   * Can't bcmp the header with what we had,  it may have been stripped!
   * so we may save looking at non executables with the same name, mostly
   * directories.
   */
  fd = open (filename, O_RDONLY);
  if (fd != -1)
    {
      if (read (fd, &hdr, sizeof (hdr)) == sizeof (hdr)
	  && !N_BADMAG (hdr) && N_DATOFF (hdr) == N_DATOFF (nhdr)
	  && N_TRELOFF (hdr) == N_TRELOFF (nhdr))
	{
	  /* compare cookies */
	  lseek (fd, N_DATOFF (hdr) + (int)&cookie - N_DATADDR (hdr), L_SET);
	  read (fd, &filenames_cookie, sizeof (filenames_cookie));
	  if (filenames_cookie == cookie)
	    {			/* Eureka */

	      /*
	       * Do the mapping
	       * The PROT_EXEC may not be needed,  but it is safer this way.
	       * should the shared library decide to indirect through
	       * addresses in the data segment not part of __DYNAMIC
	       */
	      mmap ((char *) data_start, rd_only_len, PROT_READ | PROT_EXEC,
		    MAP_FILE | MAP_SHARED | MAP_FIXED, fd,
		    N_DATOFF (hdr) + data_start - N_DATADDR (hdr));
	      close (fd);
	      return 1;
	    }
	}
      close (fd);
    }
  return 0;
}
