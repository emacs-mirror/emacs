/^Building.+$/ {
  kind = $2
}

/^Start Imports$/ {
  imports = 1
}

// {
  if (imports && ++imports > 2)
    {
      if (!match ($0, /^End Imports$/))
	makefile_imports = makefile_imports " " $0
    }
  else if (!match ($0, /^End$/) && !match ($0, /^Building.+$/))
    {
      if (kind)
	{
	  if (target_found)
	    cxx_deps = $0
	  else if (ldflags_found)
	    {
	      target = $0
	      target_found = 1
	    }
	  else if (cflags_found)
	    {
	      ldflags = $0
	      ldflags_found = 1
	    }
	  else if (includes_found)
	    {
	      cflags = $0
	      cflags_found = 1
	    }
	  else if (src_found)
	    {
	      includes = $0
	      includes_found = 1
	    }
	  else if (name_found)
	    {
	      src = $0
	      src_found = 1;
	    }
	  else
	    {
	      name = $0
	      name_found = 1
	    }
	}
    }
}

/^End$/ {
  if (name == MODULE && (kind == "shared" || kind == "static"))
    {
      printf "module_name=%s;", name
      printf "module_kind=%s;", kind
      printf "module_src=\"%s\";", src
      printf "module_includes=\"%s\";", includes
      printf "module_cflags=\"%s\";", cflags
      printf "module_ldflags=\"%s\";", ldflags
      printf "module_target=\"%s\";", target
      printf "module_cxx_deps=\"%s\";", cxx_deps
    }

  src = ""
  name = ""
  kind = ""
  includes = ""
  cflags = ""
  ldflags = ""
  name_found = ""
  src_found = ""
  includes_found = ""
  cflags_found = ""
  ldflags_found = ""
  target_found = ""
}

/^End Imports$/ {
  imports = ""
  # Strip off leading whitespace.
  gsub (/^[ \t]+/, "", makefile_imports)
  printf "module_imports=\"%s\";", makefile_imports
  makefile_imports = ""
}
