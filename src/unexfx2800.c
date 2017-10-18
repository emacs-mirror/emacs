/* Unexec for the Alliant FX/2800.  */

#include <stdio.h>

unexec (new_name, a_name, data_start, bss_start, entry_address)
     char *new_name, *a_name;
     unsigned data_start, bss_start, entry_address;
{
  int stat;
    
  stat = elf_write_modified_data (a_name, new_name);
  if (stat < 0)
    perror ("emacs: elf_write_modified_data");
  else if (stat > 0)
    fprintf (stderr, "Unspecified error from elf_write_modified_data.\n");
}
