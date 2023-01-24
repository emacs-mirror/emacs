/^Building.+$/ {
  kind = $2
}

// {
  if (!match ($0, /^End$/) && !match ($0, /^Building.+$/))
    {
      if (kind)
	{
	  if (ldflags_found)
	    target = $0
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
      printf "module_name=%s\n", name
      printf "module_kind=%s\n", kind
      printf "module_src=\"%s\"\n", src
      printf "module_includes=\"%s\"\n", includes
      printf "module_cflags=\"%s\"\n", cflags
      printf "module_ldflags=\"%s\"\n", ldflags
      printf "module_target=\"%s\"\n", target
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
}
