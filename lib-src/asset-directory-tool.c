/* Android asset directory tool.

Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

#include <stdio.h>
#include <assert.h>
#include <fcntl.h>
#include <errno.h>
#include <byteswap.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>
#include <unistd.h>

#include <sys/stat.h>

/* This program takes a directory as input, and generates a
   ``directory-tree'' file suitable for inclusion in an Android
   application package.

   Such a file records the layout of the `assets' directory in the
   package, and, in packages targeting Android 2.2, the size of each of
   its members.  Emacs records this information itself and uses it in
   the Android emulation of readdir, because the system asset manager
   APIs are often unable to locate directories or files, or provide
   corresponding metadata.

   The file is packed, with no data alignment guarantees made.  The file
   starts with the bytes "EMACS", or EMACS____ on Android 2.2, following
   which is the name of the first file or directory, a NULL byte, an
   unsigned int holding its size (on Android 2.2), and an unsigned int
   indicating the offset from the start of the file to the start of the
   next sibling.  Following that is a list of subdirectories or files in
   the same format.  The long is stored LSB first.

   Directories can be distinguished from ordinary files through the
   last bytes of their file names (immediately previous to their
   terminating NULL bytes), which are set to the directory separator
   character `/'.  */



struct directory_tree
{
  /* The offset to the next sibling.  */
  size_t offset;

  /* The name of this directory or file.  */
  char *name;

  /* st_size of this entry.  */
  off_t st_size;

  /* Subdirectories and files inside this directory.  */
  struct directory_tree *children, *next;
};

/* Whether the size of each entry should be prepended to the start
   pointer.  */
static bool need_file_size;



/* Exit with EXIT_FAILURE, after printing a description of a failing
   function WHAT along with the details of the error.  */

static _Noreturn void
croak (const char *what)
{
  perror (what);
  exit (EXIT_FAILURE);
}

/* Like malloc, but aborts on failure.  */

static void *
xmalloc (size_t size)
{
  void *ptr;

  ptr = malloc (size);

  if (!ptr)
    croak ("malloc");

  return ptr;
}

/* Recursively build a struct directory_tree structure for each
   subdirectory or file in DIR, in preparation for writing it out to
   disk.  PARENT should be the directory tree associated with the
   parent directory, or else PARENT->offset must be initialized to
   5.  */

static void
main_1 (DIR *dir, struct directory_tree *parent)
{
  struct dirent *dirent;
  int dir_fd, fd;
  struct stat statb;
  struct directory_tree *this, **last;
  size_t length;
  DIR *otherdir;

  dir_fd = dirfd (dir);
  last = &parent->children;

  while ((dirent = readdir (dir)))
    {
      /* Determine what kind of file DIRENT is.  */

      if (fstatat (dir_fd, dirent->d_name, &statb,
		   AT_SYMLINK_NOFOLLOW) == -1)
	croak ("fstatat");

      /* Ignore . and ...  */

      if (!strcmp (dirent->d_name, ".")
	  || !strcmp (dirent->d_name, ".."))
	continue;

      length = strlen (dirent->d_name);

      if (statb.st_mode & S_IFDIR)
	{
	  /* This is a directory.  Write its name followed by a
	     trailing slash, then a NULL byte, and the offset to the
	     next sibling.  */
	  this = xmalloc (sizeof *this);
	  this->children = NULL;
	  this->next = NULL;
	  *last = this;
	  last = &this->next;
	  this->name = xmalloc (length + 2);
	  strcpy (this->name, dirent->d_name);
	  this->st_size = 0;

	  /* Now record the offset to the end of this directory.  This
	     is length + 1, for the file name, 5 more bytes for the
	     trailing NULL and long, and 4 further bytes if a file size
	     is required.  */
	  this->offset = (parent->offset
			  + length + 6 + (need_file_size ? 4 : 0));

	  /* Terminate that with a slash and trailing NULL byte.  */
	  this->name[length] = '/';
	  this->name[length + 1] = '\0';

	  /* Open and build that directory recursively.  */

	  fd = openat (dir_fd, dirent->d_name, O_DIRECTORY,
		       O_RDONLY);
	  if (fd < 0)
	    croak ("openat");
	  otherdir = fdopendir (fd);
	  if (!otherdir)
	    croak ("fdopendir");

	  main_1 (otherdir, this);

	  /* Close this directory.  */
	  closedir (otherdir);

	  /* Finally, set parent->offset to this->offset as well.  */
	  parent->offset = this->offset;
	}
      else if (statb.st_mode & S_IFREG)
	{
	  /* This is a regular file.  */
	  this = xmalloc (sizeof *this);
	  this->children = NULL;
	  this->next = NULL;
	  *last = this;
	  last = &this->next;
	  this->name = xmalloc (length + 1);
	  this->st_size = statb.st_size;
	  strcpy (this->name, dirent->d_name);

	  if (this->st_size >= 0x1ffffff)
	    {
	      fprintf (stderr,
		       "asset-directory-tool: file size exceeds maximum"
		       " representable in a directory-tree: %s\n",
		       dirent->d_name);
	      exit (EXIT_FAILURE);
	    }

	  /* This is one byte the shorter because there is no trailing
	     slash.  */
	  this->offset = (parent->offset + length + 5
			  + (need_file_size ? 4 : 0));
	  parent->offset = this->offset;
	}
    }
}

/* Write the struct directory_tree TREE and all of is children to the
   file descriptor FD.  OFFSET is the offset of TREE and may be
   modified; it is only used for checking purposes.  */

static void
main_2 (int fd, struct directory_tree *tree, size_t *offset)
{
  ssize_t size;
  struct directory_tree *child;
  unsigned int output[2];

  /* Write tree->name with the trailing NULL byte.  */
  size = strlen (tree->name) + 1;
  if (write (fd, tree->name, size) < size)
    croak ("write");

  /* Write the offset.  */
#ifdef WORDS_BIGENDIAN
  output[1] = bswap_32 (tree->offset);
  output[0] = bswap_32 ((unsigned int) tree->st_size);
#else /* !WORDS_BIGENDIAN */
  output[1] = tree->offset;
  output[0] = (unsigned int) tree->st_size;
#endif /* !WORDS_BIGENDIAN */

  static_assert (sizeof output == 8 && sizeof output[0] == 4);
  if (!need_file_size)
    {
      if (write (fd, output + 1, 4) < 1)
	croak ("write");
      size += 4;
    }
  else
    {
      if (write (fd, output, 8) < 1)
	croak ("write");
      size += 8;
    }

  /* Now update offset.  */
  *offset += size;

  /* Write out each child.  */
  for (child = tree->children; child; child = child->next)
    main_2 (fd, child, offset);

  /* Verify the offset is correct.  */
  if (tree->offset != *offset)
    {
      fprintf (stderr,
	       "asset-directory-tool: invalid offset: expected %tu, "
	       "got %tu.\n"
	       "Please report this bug to bug-gnu-emacs@gnu.org, along\n"
	       "with an archive containing the contents of the java/inst"
	       "all_temp directory.\n",
	       tree->offset, *offset);
      abort ();
    }
}

int
main (int argc, char **argv)
{
  int fd;
  DIR *indir;
  struct directory_tree tree;
  size_t offset;

  if (argc != 3 && argc != 4)
    {
      fprintf (stderr, "usage: %s directory output-file "
	       "[--api-8]\n", argv[0]);
      return EXIT_FAILURE;
    }

  if (argc == 4 && !strcmp (argv[3], "--api-8"))
    need_file_size = true;

  fd = open (argv[2], O_CREAT | O_TRUNC | O_RDWR,
	     S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP);

  if (fd < 0)
    {
      perror ("open");
      return EXIT_FAILURE;
    }

  indir = opendir (argv[1]);

  if (!indir)
    {
      perror ("opendir");
      return EXIT_FAILURE;
    }

  /* Write the first 5 byte header to FD.  */

  if (write (fd, "EMACS", 5) < 5)
    {
      perror ("write");
      return EXIT_FAILURE;
    }

  /* And a further 4 bytes of padding if need_file_size.  */
  if (need_file_size && write (fd, "____", 4) < 4)
    {
      perror ("write");
      return EXIT_FAILURE;
    }

  /* Now iterate through children of INDIR, building the directory
     tree.  */
  tree.offset = 5 + (need_file_size ? 4 : 0);
  tree.children = NULL;

  main_1 (indir, &tree);
  closedir (indir);

  /* Finally, write the directory tree to the output file.  */
  offset = 5 + (need_file_size ? 4 : 0);
  for (; tree.children; tree.children = tree.children->next)
    main_2 (fd, tree.children, &offset);

  return 0;
}
