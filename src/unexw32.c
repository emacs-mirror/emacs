/* unexec for GNU Emacs on Windows NT.
   Copyright (C) 1994 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

   Geoff Voelker (voelker@cs.washington.edu)                         8-12-94
*/

#include <config.h>

#include <stdlib.h> 	/* _fmode */
#include <stdio.h>
#include <fcntl.h>
#include <time.h>
#include <windows.h>

/* Include relevant definitions from IMAGEHLP.H, which can be found
   in \\win32sdk\mstools\samples\image\include\imagehlp.h. */

PIMAGE_NT_HEADERS
(__stdcall * pfnCheckSumMappedFile) (LPVOID BaseAddress,
				    DWORD FileLength,
				    LPDWORD HeaderSum,
				    LPDWORD CheckSum);

extern BOOL ctrl_c_handler (unsigned long type);

extern char my_begdata[];
extern char my_edata[];
extern char my_begbss[];
extern char my_endbss[];

#include "w32heap.h"

/* Basically, our "initialized" flag.  */
BOOL need_to_recreate_heap = FALSE;

/* So we can find our heap in the file to recreate it.  */
unsigned long heap_index_in_executable = 0;

int open_input_file (file_data *p_file, char *name);
int open_output_file (file_data *p_file, char *name, unsigned long size);
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

#ifdef HAVE_NTGUI
HINSTANCE hinst = NULL;
HINSTANCE hprevinst = NULL;
LPSTR lpCmdLine = "";
int nCmdShow = 0;
#endif /* HAVE_NTGUI */

/* Startup code for running on NT.  When we are running as the dumped
   version, we need to bootstrap our heap and .bss section into our
   address space before we can actually hand off control to the startup
   code supplied by NT (primarily because that code relies upon malloc ()).  */
void
_start (void)
{
  extern void mainCRTStartup (void);

#if 0
  /* Give us a way to debug problems with crashes on startup when
     running under the MSVC profiler. */
  if (GetEnvironmentVariable ("EMACS_DEBUG", NULL, 0) > 0)
    DebugBreak ();
#endif

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

#if 1
      /* To allow profiling, make sure executable_path names the .exe
	 file, not the ._xe file created by the profiler which contains
	 extra code that makes the stored exe offsets incorrect.  (This
	 will not be necessary when unexec properly extends the .bss (or
	 .data as appropriate) section to include the dumped bss data,
	 and dumps the heap into a proper section of its own.)  */
      {
	char * p = strrchr (executable_path, '.');
	if (p && p[1] == '_')
	  p[1] = 'e';
      }

      /* Using HiProf profiler, exe name is different still. */
      {
	char * p = strrchr (executable_path, '\\');
	strcpy (p, "\\emacs.exe");
      }
#endif

      recreate_heap (executable_path);
      need_to_recreate_heap = FALSE;
    }
  else
    {
      /* Grab our malloc arena space now, before CRT starts up. */
      sbrk (0);
    }

  /* The default behavior is to treat files as binary and patch up
     text files appropriately, in accordance with the MSDOS code.  */
  _fmode = O_BINARY;

  /* This prevents ctrl-c's in shells running while we're suspended from
     having us exit.  */
  SetConsoleCtrlHandler ((PHANDLER_ROUTINE) ctrl_c_handler, TRUE);

  /* Invoke the NT CRT startup routine now that our housecleaning
     is finished.  */
#ifdef HAVE_NTGUI
  /* determine WinMain args like crt0.c does */
  hinst = GetModuleHandle(NULL);
  lpCmdLine = GetCommandLine();
  nCmdShow = SW_SHOWDEFAULT;
#endif
  mainCRTStartup ();
}

/* Dump out .data and .bss sections into a new executable.  */
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
  if (!open_input_file (&in_file, in_filename))
    {
      printf ("Failed to open %s (%d)...bailing.\n", 
	      in_filename, GetLastError ());
      exit (1);
    }

  /* Get the interesting section info, like start and size of .bss...  */
  get_section_info (&in_file);

  /* The size of the dumped executable is the size of the original
     executable plus the size of the heap and the size of the .bss section.  */
  heap_index_in_executable = (unsigned long)
    round_to_next ((unsigned char *) in_file.size, get_allocation_unit ());
  size = heap_index_in_executable + get_committed_heap_size () + bss_size;
  if (!open_output_file (&out_file, out_filename, size))
    {
      printf ("Failed to open %s (%d)...bailing.\n", 
	      out_filename, GetLastError ());
      exit (1);
    }

  /* Set the flag (before dumping).  */
  need_to_recreate_heap = TRUE;

  copy_executable_and_dump_data_section (&in_file, &out_file);
  dump_bss_and_heap (&in_file, &out_file);

  /* Patch up header fields; profiler is picky about this. */
  {
    PIMAGE_DOS_HEADER dos_header;
    PIMAGE_NT_HEADERS nt_header;
    HANDLE hImagehelp = LoadLibrary ("imagehlp.dll");
    DWORD  headersum;
    DWORD  checksum;

    dos_header = (PIMAGE_DOS_HEADER) out_file.file_base;
    nt_header = (PIMAGE_NT_HEADERS) ((char *) dos_header + dos_header->e_lfanew);

    nt_header->OptionalHeader.CheckSum = 0;
//    nt_header->FileHeader.TimeDateStamp = time (NULL);
//    dos_header->e_cp = size / 512;
//    nt_header->OptionalHeader.SizeOfImage = size;

    pfnCheckSumMappedFile = (void *) GetProcAddress (hImagehelp, "CheckSumMappedFile");
    if (pfnCheckSumMappedFile)
      {
//	nt_header->FileHeader.TimeDateStamp = time (NULL);
	pfnCheckSumMappedFile (out_file.file_base,
			       out_file.size,
			       &headersum,
			       &checksum);
	nt_header->OptionalHeader.CheckSum = checksum;
      }
    FreeLibrary (hImagehelp);
  }

  close_file_data (&in_file);
  close_file_data (&out_file);
}


/* File handling.  */


int
open_input_file (file_data *p_file, char *filename)
{
  HANDLE file;
  HANDLE file_mapping;
  void  *file_base;
  unsigned long size, upper_size;

  file = CreateFile (filename, GENERIC_READ, FILE_SHARE_READ, NULL,
		     OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE) 
    return FALSE;

  size = GetFileSize (file, &upper_size);
  file_mapping = CreateFileMapping (file, NULL, PAGE_READONLY, 
				    0, size, NULL);
  if (!file_mapping) 
    return FALSE;

  file_base = MapViewOfFile (file_mapping, FILE_MAP_READ, 0, 0, size);
  if (file_base == 0) 
    return FALSE;

  p_file->name = filename;
  p_file->size = size;
  p_file->file = file;
  p_file->file_mapping = file_mapping;
  p_file->file_base = file_base;

  return TRUE;
}

int
open_output_file (file_data *p_file, char *filename, unsigned long size)
{
  HANDLE file;
  HANDLE file_mapping;
  void  *file_base;

  file = CreateFile (filename, GENERIC_READ | GENERIC_WRITE, 0, NULL,
		     CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE) 
    return FALSE;

  file_mapping = CreateFileMapping (file, NULL, PAGE_READWRITE, 
				    0, size, NULL);
  if (!file_mapping) 
    return FALSE;
  
  file_base = MapViewOfFile (file_mapping, FILE_MAP_WRITE, 0, 0, size);
  if (file_base == 0) 
    return FALSE;
  
  p_file->name = filename;
  p_file->size = size;
  p_file->file = file;
  p_file->file_mapping = file_mapping;
  p_file->file_base = file_base;

  return TRUE;
}

/* Close the system structures associated with the given file.  */
void
close_file_data (file_data *p_file)
{
    UnmapViewOfFile (p_file->file_base);
    CloseHandle (p_file->file_mapping);
    CloseHandle (p_file->file);
}


/* Routines to manipulate NT executable file sections.  */

static void
get_bss_info_from_map_file (file_data *p_infile, PUCHAR *p_bss_start, 
			    DWORD *p_bss_size)
{
  int n, start, len;
  char map_filename[MAX_PATH];
  char buffer[256];
  FILE *map;

  /* Overwrite the .exe extension on the executable file name with
     the .map extension.  */
  strcpy (map_filename, p_infile->name);
  n = strlen (map_filename) - 3;
  strcpy (&map_filename[n], "map");

  map = fopen (map_filename, "r");
  if (!map)
    {
      printf ("Failed to open map file %s, error %d...bailing out.\n",
	      map_filename, GetLastError ());
      exit (-1);
    }

  while (fgets (buffer, sizeof (buffer), map))
    {
      if (!(strstr (buffer, ".bss") && strstr (buffer, "DATA")))
	continue;
      n = sscanf (buffer, " %*d:%x %x", &start, &len);
      if (n != 2)
	{
	  printf ("Failed to scan the .bss section line:\n%s", buffer);
	  exit (-1);
	}
      break;
    }
  *p_bss_start = (PUCHAR) start;
  *p_bss_size = (DWORD) len;
}

unsigned long
get_section_size (PIMAGE_SECTION_HEADER p_section)
{
  /* The true section size, before rounding.  Some linkers swap the
     meaning of these two values.  */
  return min (p_section->SizeOfRawData,
	      p_section->Misc.VirtualSize);
}

/* Return pointer to section header for named section. */
IMAGE_SECTION_HEADER *
find_section (char * name, IMAGE_NT_HEADERS * nt_header)
{
  PIMAGE_SECTION_HEADER section;
  int i;

  section = IMAGE_FIRST_SECTION (nt_header);

  for (i = 0; i < nt_header->FileHeader.NumberOfSections; i++)
    {
      if (strcmp (section->Name, name) == 0)
	return section;
      section++;
    }
  return NULL;
}

/* Return pointer to section header for section containing the given
   relative virtual address. */
IMAGE_SECTION_HEADER *
rva_to_section (DWORD rva, IMAGE_NT_HEADERS * nt_header)
{
  PIMAGE_SECTION_HEADER section;
  int i;

  section = IMAGE_FIRST_SECTION (nt_header);

  for (i = 0; i < nt_header->FileHeader.NumberOfSections; i++)
    {
      if (rva >= section->VirtualAddress
	  && rva < section->VirtualAddress + section->SizeOfRawData)
	return section;
      section++;
    }
  return NULL;
}


/* Flip through the executable and cache the info necessary for dumping.  */
static void
get_section_info (file_data *p_infile)
{
  PIMAGE_DOS_HEADER dos_header;
  PIMAGE_NT_HEADERS nt_header;
  PIMAGE_SECTION_HEADER section, data_section;
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
#ifdef SEPARATE_BSS_SECTION
      if (!strcmp (section->Name, ".bss")) 
	{
	  /* The .bss section.  */
	  ptr = (char *) nt_header->OptionalHeader.ImageBase +
	    section->VirtualAddress;
	  bss_start = ptr;
	  bss_size = get_section_size (section);
	}
#endif
#if 0
      if (!strcmp (section->Name, ".data")) 
	{
	  /* From lastfile.c  */
	  extern char my_edata[];

	  /* The .data section.  */
	  data_section = section;
	  ptr = (char *) nt_header->OptionalHeader.ImageBase +
	    section->VirtualAddress;
	  data_start_va = ptr;
	  data_start_file = section->PointerToRawData;

	  /* We want to only write Emacs data back to the executable,
	     not any of the library data (if library data is included,
	     then a dumped Emacs won't run on system versions other
	     than the one Emacs was dumped on).  */
	  data_size = my_edata - data_start_va;
	}
#else
      if (!strcmp (section->Name, "EMDATA")) 
	{
	  /* The Emacs initialized data section.  */
	  data_section = section;
	  ptr = (char *) nt_header->OptionalHeader.ImageBase +
	    section->VirtualAddress;
	  data_start_va = ptr;
	  data_start_file = section->PointerToRawData;

	  /* Write back the full section.  */
	  data_size = get_section_size (section);
	}
#endif
      section++;
    }

#ifdef SEPARATE_BSS_SECTION
  if (bss_start == UNINIT_PTR && bss_size == UNINIT_LONG)
    {
      /* Starting with MSVC 4.0, the .bss section has been eliminated
	 and appended virtually to the end of the .data section.  Our
	 only hint about where the .bss section starts in the address
	 comes from the SizeOfRawData field in the .data section
	 header.  Unfortunately, this field is only approximate, as it
	 is a rounded number and is typically rounded just beyond the
	 start of the .bss section.  To find the start and size of the
	 .bss section exactly, we have to peek into the map file.  */
      get_bss_info_from_map_file (p_infile, &ptr, &bss_size);
      bss_start = ptr + nt_header->OptionalHeader.ImageBase
	+ data_section->VirtualAddress;
    }
#else
  bss_start = my_begbss;
  bss_size = my_endbss - bss_start;
#endif
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
