$! Command file to rename many files at once
$! performing a global string substitution on each name.
$!Example:
$! @allrename [...] "-" "_"
$!changes each `-' in a name into a `_'.
$!
$!The device and directory names are not altered.
$!
$	p2_length = f$length (p2)
$	p3_length = f$length (p3)
$
$file_loop:
$	   full_name = f$search("''p1'*.*;*")
$	   if ("''full_name'" .eqs. "") then goto done
$	   original_device = f$parse("''full_name'",,,"DEVICE")
$	   original_dir = f$parse("''full_name'",,,"DIRECTORY")
$	   original_file_name = f$parse("''full_name'",,,"NAME")
$	   original_file_type = f$parse("''full_name'",,,"TYPE")
$	   original_file_version = f$parse("''full_name'",,,"VERSION")
$	   original_file = original_file_name -
		+ original_file_type + original_file_version
$	   new_file = original_file
$	   something_done = "false"
$name_loop:
$	      max_length = f$length(new_file)
$	      index = f$locate("''p2'", new_file)
$	      if (index .ge. max_length) then goto end_of_name
$	      something_done = "true"
$	      new_file = f$extract (0,index,new_file) + p3 -
		+ f$extract(index+p2_length,max_length-index,new_file)
$	      goto name_loop
$end_of_name:
$	   original_file = original_device + original_dir + original_file
$	   new_file = original_device + original_dir + new_file
$	   if (something_done) then -
	      rename 'original_file' 'new_file'
$	   goto file_loop
$done:
$	exit
