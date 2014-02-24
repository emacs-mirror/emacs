/*
	<dirent.h> -- definitions for POSIX-compatible directory access

 * The code here is forced by the interface, and is not subject to
 * copyright, constituting the only possible expression of the
 * algorithm in this format.
 */

#define DIRBLKSIZ	512		/* size of directory block */
#ifdef WINDOWSNT
#define MAXNAMLEN	255
#else  /* not WINDOWSNT */
#define MAXNAMLEN	15		/* maximum filename length */
#endif /* not WINDOWSNT */
	/* NOTE:  MAXNAMLEN must be one less than a multiple of 4 */

struct dirent				/* data from readdir() */
	{
	long		d_ino;		/* inode number of entry */
	unsigned short	d_reclen;	/* length of this record */
	unsigned short	d_namlen;	/* length of string in d_name */
#if __MINGW_MAJOR_VERSION >= 4
	/* MinGW.org runtime 4.x introduces a modified layout of
	   'struct dirent', which makes it binary incompatible with
	   previous versions.  To add insult to injury, the MinGW
	   startup code calls 'readdir', which is implemented in
	   w32.c.  So we need to define the same layout of this struct
	   as the MinGW runtime does, or else command-line globbing
	   will be broken.  (Versions of MinGW runtime after 4.0 are
	   supposed not to call 'readdir' from startup code, but we
	   had better be safe than sorry.)  */
	unsigned	d_type;		/* File attributes */
	/* The next 3 fields are declared 'time_t' in the MinGW 4.0
	   headers, but 'time_t' is by default a 64-bit type in 4.x,
	   and presumably the libmingwex library was compiled using
	   that default definition.  So we must use 64-bit types here,
	   even though our time_t is a 32-bit type.  What a mess!  */
	__int64		d_time_create;
	__int64		d_time_access;	/* always midnight local time */
	__int64		d_time_write;
	_fsize_t	d_size;
#endif
	char		d_name[MAXNAMLEN * 4 + 1];	/* name of file */
	};

typedef struct
	{
	int	dd_fd;			/* file descriptor */
	int	dd_loc;			/* offset in block */
	int	dd_size;		/* amount of valid data */
	char	dd_buf[DIRBLKSIZ];	/* directory block */
	}	DIR;			/* stream data from opendir() */

extern DIR		*opendir (const char *);
extern struct dirent	*readdir (DIR *);
extern void		seekdir (DIR *, long);
extern void		closedir (DIR *);

#define rewinddir( dirp )	seekdir( dirp, 0L )
