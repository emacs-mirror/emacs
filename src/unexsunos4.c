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
#ifdef emacs
#include <config.h>
#endif

#include <sys/param.h>
#include <sys/mman.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <string.h>
#include <stdio.h>
#include <a.out.h>

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
static unsigned Brk;
static struct exec nhdr;
static int rd_only_len;
static long cookie;


unexec (new_name, a_name, bndry, bss_start, entry) 
     char *new_name, *a_name;
     unsigned bndry, bss_start, entry;
{
  char buf[PAGSIZ];
  int fd, new;
  char *old;
  struct exec ohdr;		/* Allocate on the stack,  not needed in the next life */
  struct stat stat;

#ifdef emacs
  fprintf (stderr, "Used %d bytes of Pure Storage\n", pureptr);
#endif

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

  old = (char *)mmap (0, stat.st_size, PROT_READ, MAP_SHARED, fd, 0);
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

  Brk = sbrk (0);		/* Save the break, it is reset to &_end (by ld.so?) */

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
  
  /* 
   * Handle new data and bss sizes and optional new entry point.
   * No one actually uses bss_start and entry,  but tradition compels
   * one to support them.
   * Could complain if bss_start > Brk,  but the caller is *supposed* to know
   * what she is doing.
   */
  nhdr.a_data = (bss_start ? bss_start : Brk) - N_DATADDR (nhdr);
  nhdr.a_bss  = bss_start ? Brk - bss_start : 0;
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
  brk (Brk);

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

is_it (path)
  char *path;
{
  int fd;
  long paths_cookie;
  struct exec hdr;

  /*
   * Open an executable  and check for a valid header!
   * Can't bcmp() the header with what we had,  it may have been stripped!
   * so we may save looking at non executables with the same name, mostly
   * directories.
   */
  fd = open (path, O_RDONLY);
  if (fd != -1)
    {
      if (read (fd, &hdr, sizeof (hdr)) == sizeof (hdr)
	  && !N_BADMAG (hdr) && N_DATOFF (hdr) == N_DATOFF (nhdr)
	  && N_TRELOFF (hdr) == N_TRELOFF (nhdr))
	{
	  /* compare cookies */
	  lseek (fd, N_DATOFF (hdr) + (int)&cookie - N_DATADDR (hdr), L_SET);
	  read (fd, &paths_cookie, sizeof (paths_cookie));
	  if (paths_cookie == cookie)
	    {			/* Eureka */

	      /*
	       * Do the mapping
	       * The PROT_EXEC may not be needed,  but it is safer this way.
	       * should the shared library decide to indirect through
	       * addresses in the data segment not part of __DYNAMIC
	       */
	      mmap (data_start, rd_only_len, PROT_READ | PROT_EXEC,
		    MAP_SHARED | MAP_FIXED, fd,
		    N_DATOFF (hdr) + data_start - N_DATADDR (hdr));
	      close (fd);
	      return 1;
	    }
	}
      close (fd);
    }
  return 0;
}
