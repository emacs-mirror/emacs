/* Define the symbols for various file types.
   These are not included in POSIX.  */

#ifndef S_IFMT
#define	S_IFMT	0170000		/* type of file */
#define S_IFDIR	0040000	/* directory */
#define S_IFCHR	0020000	/* character special */
#define S_IFBLK	0060000	/* block special */
#define S_IFREG	0100000	/* regular */
#define S_IFIFO	0010000	/* fifo */
#define S_IFNAM 0050000 /* special named file */
#endif /* not S_IFMT */

