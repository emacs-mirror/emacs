/*
   unexec for GNU Emacs on Windows NT.

   Copyright (C) 1994 Free Software Foundation, Inc.

   This file is part of GNU Emacs.

   GNU Emacs is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any later
   version.

   GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with GNU Emacs; see the file COPYING.  If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Geoff Voelker (voelker@cs.washington.edu)                         8-12-94
*/

#include <stdlib.h> 	/* _fmode */
#include <stdio.h>
#include <fcntl.h>
#include <windows.h>

extern BOOL ctrl_c_handler (unsigned long type);

#include "ntheap.h"

/* A convenient type for keeping all the info about a mapped file together.  */
typedef struct file_data {
    char          *name;
    unsigned long  size;
    HANDLE         file;
    HANDLE         file_mapping;
    unsigned char *file_base;
} file_data;

/* Basically, our "initialized" flag.  */
BOOL need_to_recreate_heap = FALSE;

/* So we can find our heap in the file to recreate it.  */
unsigned long heap_index_in_executable = 0;

void open_input_file (file_data *p_file, char *name);
void open_output_file (file_data *p_file, char *name, unsigned long size);
void close_file_data (file_data *p_file);

void get_section_info (file_data *p_file);
void copy_executable_and_dump_data_section (file_data *, file_data *);
void dump_bss_and_heap (file_data *p_infile, file_data *p_outfile);

/* Cached info about the .data section in the executable.  */
PUCHAR data_start_va = 0;
DWORD  data_start_file = 0;
DWORD  data_size = 0;

/* Cached info about the .bss section in the executable.  */
PUCHAR bss_start = 0;
DWORD  bss_size = 0;

/* Startup code for running on NT.  When we are running as the dumped
   version, we need to bootstrap our heap and .bss section into our
   address space before we can actually hand off control to the startup
   code supplied by NT (primarily because that code relies upon malloc ()).  */
void
_start (void)
{
  extern void mainCRTStartup (void);

  /* Cache system info, e.g., the NT page size.  */
  cache_system_info ();

  /* If we're a dumped version of emacs then we need to recreate
     our heap and play tricks with our .bss section.  Do this before
     start up.  (WARNING:  Do not put any code before this section
     that relies upon malloc () and runs in the dumped version.  It
     won't work.)  */
  if (need_to_recreate_heap) 
    {
      char executable_path[MAX_PATH];

      if (GetModuleFileName (NULL, executable_path, MAX_PATH) == 0) 
	{
	  printf ("Failed to find path for executable.\n");
	  exit (1);
	}
      recreate_heap (executable_path);
      need_to_recreate_heap = FALSE;
    }

  /* The default behavior is to treat files as binary and patch up
     text files appropriately, in accordance with the MSDOS code.  */
  _fmode = O_BINARY;

  /* This prevents ctrl-c's in shells running while we're suspended from
     having us exit.  */
  SetConsoleCtrlHandler ((PHANDLER_ROUTINE) ctrl_c_handler, TRUE);

  /* Invoke the NT CRT startup routine now that our housecleaning
     is finished.  */
  mainCRTStartup ();
}

/* Dump out .data and .bss sections into a new exectubale.  */
void
unexec (char *new_name, char *old_name, void *start_data, void *start_bss,
	void *entry_address)
{
  file_data in_file, out_file;
  char out_filename[MAX_PATH], in_filename[MAX_PATH];
  unsigned long size;
  char *ptr;
  
  /* Make sure that the input and output filenames have the
     ".exe" extension...patch them up if they don't.  */
  strcpy (in_filename, old_name);
  ptr = in_filename + strlen (in_filename) - 4;
  if (strcmp (ptr, ".exe"))
    strcat (in_filename, ".exe");

  strcpy (out_filename, new_name);
  ptr = out_filename + strlen (out_filename) - 4;
  if (strcmp (ptr, ".exe"))
    strcat (out_filename, ".exe");

  printf ("Dumping from %s\n", in_filename);
  printf ("          to %s\n", out_filename);

  /* We need to round off our heap to NT's allocation unit (64KB).  */
  round_heap (get_allocation_unit ());

  /* Open the undumped executable file.  */
  open_input_file (&in_file, in_filename);

  /* Get the interesting section info, like start and size of .bss...  */
  get_section_info (&in_file);

  /* The size of the dumped executable is the size of the original
     executable plus the size of the heap and the size of the .bss section.  */
  heap_index_in_executable = (unsigned long)
    round_to_next ((unsigned char *) in_file.size, get_allocation_unit ());
  size = heap_index_in_executable + get_committed_heap_size () + bss_size;
  open_output_file (&out_file, out_filename, size);

  /* Set the flag (before dumping).  */
  need_to_recreate_heap = TRUE;

  copy_executable_and_dump_data_section (&in_file, &out_file);
  dump_bss_and_heap (&in_file, &out_file);

  close_file_data (&in_file);
  close_file_data (&out_file);
}


/* File handling.  */


void 
open_input_file (file_data *p_file, char *filename)
{
  HANDLE file;
  HANDLE file_mapping;
  void  *file_base;
  unsigned long size, upper_size;

  file = CreateFile (filename, GENERIC_READ, FILE_SHARE_READ, NULL,
		     OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE) 
      {
	printf ("Failed to open %s (%d)...bailing.\n", 
	       filename, GetLastError ());
	exit (1);
      }

  size = GetFileSize (file, &upper_size);
  file_mapping = CreateFileMapping (file, NULL, PAGE_READONLY, 
				    0, size, NULL);
  if (!file_mapping) 
    {
      printf ("Failed to create file mapping of %s (%d)...bailing.\n",
	     filename, GetLastError ());
      exit (1);
    }

  file_base = MapViewOfFile (file_mapping, FILE_MAP_READ, 0, 0, size);
  if (file_base == 0) 
    {
      printf ("Failed to map view of file of %s (%d)...bailing.\n",
	     filename, GetLastError ());
      exit (1);
    }

  p_file->name = filename;
  p_file->size = size;
  p_file->file = file;
  p_file->file_mapping = file_mapping;
  p_file->file_base = file_base;
}

void 
open_output_file (file_data *p_file, char *filename, unsigned long size)
{
  HANDLE file;
  HANDLE file_mapping;
  void  *file_base;
  
  file = CreateFile (filename, GENERIC_READ | GENERIC_WRITE, 0, NULL,
		     CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE) 
    {
      printf ("open_output_file: Failed to open %s (%d).\n", 
	     filename, GetLastError ());
      exit (1);
    }
  
  file_mapping = CreateFileMapping (file, NULL, PAGE_READWRITE, 
				    0, size, NULL);
  if (!file_mapping) 
    {
      printf ("open_output_file: Failed to create file mapping of %s (%d).\n",
	     filename, GetLastError ());
      exit (1);
    }
  
  file_base = MapViewOfFile (file_mapping, FILE_MAP_WRITE, 0, 0, size);
  if (file_base == 0) 
    {
      printf ("open_output_file: Failed to map view of file of %s (%d).\n",
	     filename, GetLastError ());
      exit (1);
    }
  
  p_file->name = filename;
  p_file->size = size;
  p_file->file = file;
  p_file->file_mapping = file_mapping;
  p_file->file_base = file_base;
}

/* Close the system structures associated with the given file.  */
static void
close_file_data (file_data *p_file)
{
    UnmapViewOfFile (p_file->file_base);
    CloseHandle (p_file->file_mapping);
    CloseHandle (p_file->file);
}


/* Routines to manipulate NT executable file sections.  */


static unsigned long
get_section_size (PIMAGE_SECTION_HEADER p_section)
{
  /* The section size is in different locations in the different versions.  */
  switch (get_nt_minor_version ()) 
    {
    case 10:
      return p_section->SizeOfRawData;
    default:
      return p_section->Misc.VirtualSize;
    }
}

/* Flip through the executable and cache the info necessary for dumping.  */
static void
get_section_info (file_data *p_infile)
{
  PIMAGE_DOS_HEADER dos_header;
  PIMAGE_NT_HEADERS nt_header;
  PIMAGE_SECTION_HEADER section;
  unsigned char *ptr;
  int i;
  
  dos_header = (PIMAGE_DOS_HEADER) p_infile->file_base;
  if (dos_header->e_magic != IMAGE_DOS_SIGNATURE) 
    {
      printf ("Unknown EXE header in %s...bailing.\n", p_infile->name);
      exit (1);
    }
  nt_header = (PIMAGE_NT_HEADERS) (((unsigned long) dos_header) + 
				   dos_header->e_lfanew);
  if (nt_header == NULL) 
    {
      printf ("Failed to find IMAGE_NT_HEADER in %s...bailing.\n", 
	     p_infile->name);
      exit (1);
    }

  /* Check the NT header signature ...  */
  if (nt_header->Signature != IMAGE_NT_SIGNATURE) 
    {
      printf ("Invalid IMAGE_NT_SIGNATURE 0x%x in %s...bailing.\n",
	      nt_header->Signature, p_infile->name);
    }

  /* Flip through the sections for .data and .bss ...  */
  section = (PIMAGE_SECTION_HEADER) IMAGE_FIRST_SECTION (nt_header);
  for (i = 0; i < nt_header->FileHeader.NumberOfSections; i++) 
    {
      if (!strcmp (section->Name, ".bss")) 
	{
	  /* The .bss section.  */
	  ptr = (char *) nt_header->OptionalHeader.ImageBase +
	    section->VirtualAddress;
	  bss_start = ptr;
	  bss_size = get_section_size (section);
	}
      if (!strcmp (section->Name, ".data")) 
	{
	  /* From lastfile.c  */
	  extern char my_edata[];

	  /* The .data section.  */
	  ptr  = (char *) nt_header->OptionalHeader.ImageBase +
	    section->VirtualAddress;
	  data_start_va = ptr;
	  data_start_file = section->PointerToRawData;

	  /* We want to only write Emacs data back to the executable,
	     not any of the library data (if library data is included,
	     then a dumped Emacs won't run on system versions other
	     than the one Emacs was dumped on).  */
	  data_size = my_edata - data_start_va;
	}
      section++;
    }
}


/* The dump routines.  */

static void
copy_executable_and_dump_data_section (file_data *p_infile, 
				       file_data *p_outfile)
{
  unsigned char *data_file, *data_va;
  unsigned long size, index;
  
  /* Get a pointer to where the raw data should go in the executable file.  */
  data_file = (char *) p_outfile->file_base + data_start_file;

  /* Get a pointer to the raw data in our address space.  */
  data_va = data_start_va;
    
  size = (DWORD) data_file - (DWORD) p_outfile->file_base;
  printf ("Copying executable up to data section...\n");
  printf ("\t0x%08x Offset in input file.\n", 0);
  printf ("\t0x%08x Offset in output file.\n", 0);
  printf ("\t0x%08x Size in bytes.\n", size);
  memcpy (p_outfile->file_base, p_infile->file_base, size);
  
  size = data_size;
  printf ("Dumping .data section...\n");
  printf ("\t0x%08x Address in process.\n", data_va);
  printf ("\t0x%08x Offset in output file.\n", 
	  data_file - p_outfile->file_base);
  printf ("\t0x%08x Size in bytes.\n", size);
  memcpy (data_file, data_va, size);
  
  index = (DWORD) data_file + size - (DWORD) p_outfile->file_base;
  size = p_infile->size - index;
  printf ("Copying rest of executable...\n");
  printf ("\t0x%08x Offset in input file.\n", index);
  printf ("\t0x%08x Offset in output file.\n", index);
  printf ("\t0x%08x Size in bytes.\n", size);
  memcpy ((char *) p_outfile->file_base + index, 
	  (char *) p_infile->file_base + index, size);
}

static void
dump_bss_and_heap (file_data *p_infile, file_data *p_outfile)
{
    unsigned char *heap_data, *bss_data;
    unsigned long size, index;

    printf ("Dumping heap into executable...\n");

    index = heap_index_in_executable;
    size = get_committed_heap_size ();
    heap_data = get_heap_start ();

    printf ("\t0x%08x Heap start in process.\n", heap_data);
    printf ("\t0x%08x Heap offset in executable.\n", index);
    printf ("\t0x%08x Heap size in bytes.\n", size);

    memcpy ((PUCHAR) p_outfile->file_base + index, heap_data, size);

    printf ("Dumping .bss into executable...\n");
    
    index += size;
    size = bss_size;
    bss_data = bss_start;
    
    printf ("\t0x%08x BSS start in process.\n", bss_data);
    printf ("\t0x%08x BSS offset in executable.\n", index);
    printf ("\t0x%08x BSS size in bytes.\n", size);
    memcpy ((char *) p_outfile->file_base + index, bss_data, size);
}


/* Reload and remap routines.  */


/* Load the dumped .bss section into the .bss area of our address space.  */
void
read_in_bss (char *filename)
{
  HANDLE file;
  unsigned long size, index, n_read, total_read;
  char   buffer[512], *bss;
  int    i;

  file = CreateFile (filename, GENERIC_READ, FILE_SHARE_READ, NULL,
		     OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE) 
    {
      i = GetLastError ();
      exit (1);
    }

  /* Seek to where the .bss section is tucked away after the heap...  */
  index = heap_index_in_executable + get_committed_heap_size ();
  if (SetFilePointer (file, index, NULL, FILE_BEGIN) == 0xFFFFFFFF) 
    {
      i = GetLastError ();
      exit (1);
    }

  
  /* Ok, read in the saved .bss section and initialize all 
     uninitialized variables.  */
  if (!ReadFile (file, bss_start, bss_size, &n_read, NULL))
    {
      i = GetLastError ();
      exit (1);
    }

  CloseHandle (file);
}

/* Map the heap dumped into the executable file into our address space.  */
void 
map_in_heap (char *filename)
{
  HANDLE file;
  HANDLE file_mapping;
  void  *file_base;
  unsigned long size, upper_size, n_read;
  int    i;

  file = CreateFile (filename, GENERIC_READ, FILE_SHARE_READ, NULL,
		     OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE) 
    {
      i = GetLastError ();
      exit (1);
    }
  
  size = GetFileSize (file, &upper_size);
  file_mapping = CreateFileMapping (file, NULL, PAGE_WRITECOPY, 
				    0, size, NULL);
  if (!file_mapping) 
    {
      i = GetLastError ();
      exit (1);
    }
    
  size = get_committed_heap_size ();
  file_base = MapViewOfFileEx (file_mapping, FILE_MAP_COPY, 0, 
			       heap_index_in_executable, size,
			       get_heap_start ());
  if (file_base != 0) 
    {
      return;
    }

  /* If we don't succeed with the mapping, then copy from the 
     data into the heap.  */

  CloseHandle (file_mapping);

  if (VirtualAlloc (get_heap_start (), get_committed_heap_size (),
		    MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE) == NULL)
    {
      i = GetLastError ();
      exit (1);
    }

  /* Seek to the location of the heap data in the executable.  */
  i = heap_index_in_executable;
  if (SetFilePointer (file, i, NULL, FILE_BEGIN) == 0xFFFFFFFF)
    {
      i = GetLastError ();
      exit (1);
    }

  /* Read in the data.  */
  if (!ReadFile (file, get_heap_start (), 
		 get_committed_heap_size (), &n_read, NULL))
    {
      i = GetLastError ();
      exit (1);
    }

  CloseHandle (file);
}
