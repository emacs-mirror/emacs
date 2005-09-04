/* Dump Emacs in macho format.
   Copyright (C) 1990, 1993, 2002, 2003, 2004,
                 2005 Free Software Foundation, Inc.
   Written by Bradley Taylor (btaylor@next.com).

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


#undef __STRICT_BSD__

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <mach/mach.h>
#include <mach-o/loader.h>
#include <mach-o/reloc.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>
/* Instead of unistd.h, this used to include libc.h.
   "Nelson H. F. Beebe" <beebe@math.utah.edu> says that doesn't work
   in system version 3.3.  */


int malloc_cookie;

/*
 * Kludge: we don't expect any program data beyond VM_HIGHDATA
 * What is really needed is a way to find out from malloc() which
 * pages it vm_allocated and write only those out into the data segment.
 *
 * This kludge may break when we stop using fixed virtual address
 * shared libraries. Actually, emacs will probably continue working, but be
 * much larger on disk than it needs to be (because non-malloced data will
 * be in the file).
 */
static const unsigned VM_HIGHDATA = 0x2000000;

typedef struct region_t {
	vm_address_t address;
	vm_size_t size;
	vm_prot_t protection;
	vm_prot_t max_protection;
	vm_inherit_t inheritance;
	boolean_t shared;
	port_t object_name;
	vm_offset_t offset;
} region_t;


static void
grow(
     struct load_command ***the_commands,
     unsigned *the_commands_len
     )
{
	if (*the_commands == NULL) {
		*the_commands_len = 1;
		*the_commands = malloc(sizeof(*the_commands));
	} else {
		(*the_commands_len)++;
		*the_commands = realloc(*the_commands,
					(*the_commands_len *
					 sizeof(**the_commands)));
	}
}


static void
save_command(
	     struct load_command *command,
	     struct load_command ***the_commands,
	     unsigned *the_commands_len
	     )
{
	struct load_command **tmp;

	grow(the_commands, the_commands_len);
	tmp = &(*the_commands)[*the_commands_len - 1];
	*tmp = malloc(command->cmdsize);
	bcopy(command, *tmp, command->cmdsize);
}

static void
fatal_unexec(char *format, ...)
{
	va_list ap;

	va_start(ap, format);
	fprintf(stderr, "unexec: ");
	vfprintf(stderr, format, ap);
	fprintf(stderr, "\n");
	va_end(ap);
}

static int
read_macho(
	   int fd,
	   struct mach_header *the_header,
	   struct load_command ***the_commands,
	   unsigned *the_commands_len
	   )
{
	struct load_command command;
	struct load_command *buf;
	int i;
	int size;

	if (read(fd, the_header, sizeof(*the_header)) != sizeof(*the_header)) {
		fatal_unexec("cannot read macho header");
		return (0);
	}
	for (i = 0; i < the_header->ncmds; i++) {
		if (read(fd, &command, sizeof(struct load_command)) !=
		    sizeof(struct load_command)) {
		  	fatal_unexec("cannot read macho load command header");
			return (0);
		}
		size = command.cmdsize - sizeof(struct load_command);
		if (size < 0) {
		  	fatal_unexec("bogus load command size");
			return (0);
		}
		buf = malloc(command.cmdsize);
		buf->cmd = command.cmd;
		buf->cmdsize = command.cmdsize;
		if (read(fd, ((char *)buf +
			      sizeof(struct load_command)),
			 size) != size) {
		  	fatal_unexec("cannot read load command data");
			return (0);
		}
		save_command(buf, the_commands, the_commands_len);
	}
	return (1);
}

static int
filldatagap(
	    vm_address_t start_address,
	    vm_size_t *size,
	    vm_address_t end_address
	    )
{
	vm_address_t address;
	vm_size_t gapsize;

	address = (start_address + *size);
	gapsize = end_address - address;
	*size += gapsize;
	if (vm_allocate(task_self(), &address, gapsize,
			FALSE) != KERN_SUCCESS) {
		fatal_unexec("cannot vm_allocate");
	        return (0);
	}
	return (1);
}

static int
get_data_region(
		vm_address_t *address,
		vm_size_t *size
		)
{
	region_t region;
	kern_return_t ret;
	struct section *sect;

	sect = (struct section *) getsectbyname(SEG_DATA, SECT_DATA);
	region.address = 0;
	*address = 0;
	for (;;) {
		ret = vm_region(task_self(),
				&region.address,
				&region.size,
				&region.protection,
				&region.max_protection,
				&region.inheritance,
				&region.shared,
				&region.object_name,
				&region.offset);
		if (ret != KERN_SUCCESS || region.address >= VM_HIGHDATA) {
			break;
		}
		if (*address != 0) {
			if (region.address > *address + *size) {
				if (!filldatagap(*address, size,
						 region.address)) {
					return (0);
				}
			}
			*size += region.size;
		} else {
			if (region.address == sect->addr) {
				*address = region.address;
				*size = region.size;
			}
		}
		region.address += region.size;
	}
	return (1);
}

static char *
my_malloc(
	  vm_size_t size
	  )
{
	vm_address_t address;

	if (vm_allocate(task_self(), &address, size, TRUE) != KERN_SUCCESS) {
		return (NULL);
	}
	return ((char *)address);
}

static void
my_free(
	char *buf,
	vm_size_t size
	)
{
	vm_deallocate(task_self(), (vm_address_t)buf, size);
}

static int
unexec_doit(
	    int infd,
	    int outfd
	    )
{
	int i;
	struct load_command **the_commands = NULL;
	unsigned the_commands_len;
	struct mach_header the_header;
	int fgrowth = 0;
	int fdatastart;
	int fdatasize;
	int size;
	struct stat st;
	char *buf;
	vm_address_t data_address;
	vm_size_t data_size;
	vm_size_t vmaddr_growth = 0;
	vm_size_t dataseg_vmaddr, dataseg_vmend;

	struct segment_command *segment;

#ifdef NS_TARGET
	unsigned long extreloff = 0;
	unsigned long nextrel = 0;
	struct dysymtab_command *dysymtab;
	struct relocation_info reloc_info;
#endif

	if (!read_macho(infd, &the_header, &the_commands, &the_commands_len)) {
		return (0);
	}


	malloc_cookie = malloc_freezedry ();
	if (!get_data_region(&data_address, &data_size)) {
		return (0);
	}


	/*
	 * DO NOT USE MALLOC IN THIS SECTION
	 */
	{
		/*
		 * Fix offsets
		 */
		for (i = 0; i < the_commands_len; i++) {
			switch (the_commands[i]->cmd) {
			case LC_SEGMENT:
				segment = ((struct segment_command *)
					   the_commands[i]);
				if (strcmp(segment->segname, SEG_DATA) == 0) {
					fdatastart = segment->fileoff;
					fdatasize = segment->filesize;
					fgrowth = (data_size -
						   segment->filesize);
					segment->vmsize = data_size;
					segment->filesize = data_size;
					dataseg_vmaddr = segment->vmaddr;
					dataseg_vmend = segment->vmaddr + segment->vmsize;
					vmaddr_growth = segment->vmaddr + segment->vmsize;
				} else {
					((struct segment_command *)the_commands[i])->fileoff += fgrowth;
				}

				if( strcmp( segment->segname, SEG_LINKEDIT ) == 0 ) {
					segment->vmaddr = vmaddr_growth;
				}

				break;
			case LC_SYMTAB:
				((struct symtab_command *)
				 the_commands[i])->symoff += fgrowth;
				((struct symtab_command *)
				 the_commands[i])->stroff += fgrowth;
				break;
			case LC_SYMSEG:
				((struct symseg_command *)
				 the_commands[i])->offset += fgrowth;
				break;
#ifdef NS_TARGET
			case LC_DYSYMTAB:
				dysymtab = ((struct dysymtab_command *)the_commands[i]);
				extreloff = dysymtab->extreloff;
				nextrel = dysymtab->nextrel;
				dysymtab->indirectsymoff += fgrowth;
				dysymtab->extreloff += fgrowth;
				break;
#endif
			default:
				break;
			}
		}

		/*
		 * Write header
		 */
		if (write(outfd, &the_header,
			  sizeof(the_header)) != sizeof(the_header)) {
			fatal_unexec("cannot write output file");
			return (0);
		}

		/*
		 * Write commands
		 */
		for (i = 0; i < the_commands_len; i++) {
			if (write(outfd, the_commands[i],
				  the_commands[i]->cmdsize) !=
			    the_commands[i]->cmdsize) {
			  	fatal_unexec("cannot write output file");
				return (0);
			}
		}

		/*
		 * Write original text
		 */
		if (lseek(infd, the_header.sizeofcmds + sizeof(the_header),
			  L_SET) < 0) {
		  	fatal_unexec("cannot seek input file");
			return (0);
		}
		size = fdatastart - (sizeof(the_header) +
				     the_header.sizeofcmds);
		buf = my_malloc(size);
		if (read(infd, buf, size) != size) {
			my_free(buf, size);
		  	fatal_unexec("cannot read input file");
		}
		if (write(outfd, buf, size) != size) {
			my_free(buf, size);
			fatal_unexec("cannot write output file");
			return (0);
		}
		my_free(buf, size);


		/*
		 * Write new data
		 */
		if (write(outfd, (char *)data_address,
			  data_size) != data_size) {
			fatal_unexec("cannot write output file");
			return (0);
		}

	}

	/*
	 * OKAY TO USE MALLOC NOW
	 */

	/*
	 * Write rest of file
	 */
	fstat(infd, &st);
	if (lseek(infd, fdatasize, L_INCR) < 0) {
		fatal_unexec("cannot seek input file");
		return (0);
	}
	size = st.st_size - lseek(infd, 0, L_INCR);

	buf = malloc(size);
	if (read(infd, buf, size) != size) {
		free(buf);
		fatal_unexec("cannot read input file");
		return (0);
	}
	if (write(outfd, buf, size) != size) {
		free(buf);
		fatal_unexec("cannot write output file");
		return (0);
	}
	free(buf);

#ifdef NS_TARGET
        /*
         * Fix up relocation entries in the data segment.
         */

	if (lseek(infd, extreloff, L_SET) < 0) {
		fatal_unexec("cannot seek input file");
		return (0);
	}

        for (i = 0; i < nextrel; i++)
        {
          long zeroval = 0;

          if (read(infd, &reloc_info, sizeof (reloc_info)) != sizeof (reloc_info)) {
            fatal_unexec("cannot read input file");
            return (0);
          }
          if (reloc_info.r_address >= dataseg_vmaddr && reloc_info.r_address < dataseg_vmend)
          {
            if (lseek (outfd, fdatastart + reloc_info.r_address - dataseg_vmaddr, L_SET) < 0 ) {
              fatal_unexec("cannot seek input file");
              return (0);
            }
            switch (reloc_info.r_length) {
              case 0:
		if (write(outfd, &zeroval, 1) != 1) {
			fatal_unexec("cannot write output file");
			return (0);
		}
                break;
              case 1:
		if (write(outfd, &zeroval, 2) != 2) {
			fatal_unexec("cannot write output file");
			return (0);
		}
                break;
              case 2:
		if (write(outfd, &zeroval, 4) != 4) {
			fatal_unexec("cannot write output file");
			return (0);
		}
                break;
            }
          }
        }
#endif

	return (1);
}

void
unexec(
       char *outfile,
       char *infile
       )
{
	int infd;
	int outfd;
	char tmpbuf[L_tmpnam];
	char *tmpfile;

	infd = open(infile, O_RDONLY, 0);
	if (infd < 0) {
	  	fatal_unexec("cannot open input file `%s'", infile);
		exit(1);
	}

	tmpnam(tmpbuf);
	tmpfile = rindex(tmpbuf, '/');
	if (tmpfile == NULL) {
		tmpfile = tmpbuf;
	} else {
		tmpfile++;
	}
	outfd = open(tmpfile, O_WRONLY|O_TRUNC|O_CREAT, 0755);
	if (outfd < 0) {
		close(infd);
		fatal_unexec("cannot open tmp file `%s'", tmpfile);
		exit(1);
	}
	if (!unexec_doit(infd, outfd)) {
		close(infd);
		close(outfd);
		unlink(tmpfile);
		exit(1);
	}
	close(infd);
	close(outfd);
	if (rename(tmpfile, outfile) < 0) {
		unlink(tmpfile);
		fatal_unexec("cannot rename `%s' to `%s'", tmpfile, outfile);
		exit(1);
	}
}

/* arch-tag: 9796bdc3-c050-417a-b2f5-4cfd31032634
   (do not change this comment) */
