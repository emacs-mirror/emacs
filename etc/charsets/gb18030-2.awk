BEGIN {
  tohex["A"] = 10;
  tohex["B"] = 11;
  tohex["C"] = 12;
  tohex["D"] = 13;
  tohex["E"] = 14;
  tohex["F"] = 15;
  tohex["a"] = 10;
  tohex["b"] = 11;
  tohex["c"] = 12;
  tohex["d"] = 13;
  tohex["e"] = 14;
  tohex["f"] = 15;
}

function decode_hex(str) {
  n = 0;
  len = length(str);
  for (i = 1; i <= len; i++)
    {
      c = substr (str, i, 1);
      if (c >= "0" && c <= "9")
	n = n * 16 + (c - "0");
      else
	n = n * 16 + tohex[c];
    }
  return n;
}

function gb_to_index(gb) {
  b0 = int(gb / 256);
  b1 = gb % 256;
  idx = (((b0 - 129)) * 190 + b1 - 64); 
  if (b1 >= 128)
    idx--;
  return idx
}

function index_to_gb(idx) {
  b0 = int(idx / 190) + 129;
  b1 = (idx % 190) + 64;
  if (b1 >= 127)
    b1++;
  return (b0 * 256 + b1);
}
function decode_gb(str) {
  b0 = decode_hex(substr(str, 3, 2));
  b1 = decode_hex(substr(str, 7, 2));
  return (b0 * 256 + b1)
}

/^<U[0-9A-F][0-9A-F][0-9A-F][0-9A-F]>/ {
  if ($2 ~ /^\\x[0-9A-F][0-9A-F]\\x[0-9A-F][0-9A-F]$/)
    {
      unicode = decode_hex(substr($1, 3, 4));
      gb = decode_gb($2);
      idx = gb_to_index(gb);
      gb_table[idx] = unicode;
    }
}

END {
  last_idx = gb_to_index(decode_hex("FEFE"));
  from_idx = 0;
  from_unicode = gb_table[0];
  for (i = 1; i <= last_idx; i++)
    {
      gb = index_to_gb(i);
      unicode = gb_table[i];
      if (i - from_idx != unicode - from_unicode)
	{
	  if (i - 1 == from_idx)
	    printf ("0x%04X 0x%04X\n",
		    index_to_gb(from_idx), from_unicode);
	  else
	    printf ("0x%04X-0x%04X 0x%04X\n",
		    index_to_gb(from_idx), index_to_gb(i - 1), from_unicode);
	  from_idx = i;
	  from_unicode=unicode;
	}
    }
  if (i - from_idx != unicode - from_unicode)
    printf ("0x%04X-0x%04X 0x%04X\n",
	    index_to_gb(from_idx), index_to_gb(i - 1), from_unicode);
}
