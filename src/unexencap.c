/* Waiting for papers!  */

/*
 * Do an unexec() for coff encapsulation. Uses the approach I took
 * for AKCL, so don't be surprised if it doesn't look too much like
 * the other unexec() routines. Assumes NO_REMAP. Should be easy to
 * adapt to the emacs style unexec() if that is desired, but this works
 * just fine for me with GCC/GAS/GLD under System V.  - Jordan
 */

#include <sys/types.h>
#include <sys/fcntl.h>
#include <sys/file.h>
#include <stdio.h>
#include "/usr/gnu/lib/gcc/gcc-include/a.out.h"

filecpy(to, from, n)
FILE *to, *from;
register int n;
{
	char buffer[BUFSIZ];

	for (;;)
		if (n > BUFSIZ) {
			fread(buffer, BUFSIZ, 1, from);
			fwrite(buffer, BUFSIZ, 1, to);
			n -= BUFSIZ;
		} else if (n > 0) {
			fread(buffer, 1, n, from);
			fwrite(buffer, 1, n, to);
			break;
		} else
			break;
}
/* ****************************************************************
 * unexec
 *
 * driving logic.
 * ****************************************************************/
unexec (new_name, a_name, data_start, bss_start, entry_address)
char *new_name, *a_name;
unsigned data_start, bss_start, entry_address;
{	
	struct coffheader header1;
	struct coffscn *tp, *dp, *bp;
	struct exec header;
	int stsize;
	char *original_file = a_name;
	char *save_file = new_name;

	char *data_begin, *data_end;
	int original_data;
	FILE *original, *save;
	register int n;
	register char *p;
	extern char *sbrk();
	char stdin_buf[BUFSIZ], stdout_buf[BUFSIZ];


	fclose(stdin);
	original = fopen(original_file, "r");
	if (stdin != original || original->_file != 0) {
		fprintf(stderr, "unexec: Can't open the original file.\n");
		exit(1);
	}
	setbuf(original, stdin_buf);
	fclose(stdout);
	unlink(save_file);
	n = open(save_file, O_CREAT|O_WRONLY, 0777);
	if (n != 1 || (save = fdopen(n, "w")) != stdout) {
		fprintf(stderr, "unexec: Can't open the save file.\n");
		exit(1);
	}
	setbuf(save, stdout_buf);

	fread(&header1, sizeof(header1), 1, original);
        tp = &header1.scns[0];
        dp = &header1.scns[1];
        bp = &header1.scns[2];
        fread(&header, sizeof(header), 1, original);
        data_begin=(char *)N_DATADDR(header);
        data_end = sbrk(0);
        original_data = header.a_data;
        header.a_data = data_end - data_begin;
        header.a_bss = 0;
        dp->s_size = header.a_data;
        bp->s_paddr = dp->s_vaddr + dp->s_size;
        bp->s_vaddr = bp->s_paddr; 
        bp->s_size = 0; 
        header1.tsize = tp->s_size;
        header1.dsize = dp->s_size;
        header1.bsize = bp->s_size;
        fwrite(&header1, sizeof(header1), 1, save);
        fwrite(&header, sizeof(header), 1, save);

        filecpy(save, original, header.a_text);

	for (n = header.a_data, p = data_begin;  ;  n -= BUFSIZ, p += BUFSIZ)
		if (n > BUFSIZ)
			fwrite(p, BUFSIZ, 1, save);
		else if (n > 0) {
			fwrite(p, 1, n, save);
			break;
		} else
			break;

	fseek(original, original_data, 1);

        filecpy(save, original, header.a_syms+header.a_trsize+header.a_drsize);
        fread(&stsize, sizeof(stsize), 1, original);
        fwrite(&stsize, sizeof(stsize), 1, save);
        filecpy(save, original, stsize - sizeof(stsize));

	fclose(original);
	fclose(save);
}
