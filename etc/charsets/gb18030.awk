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

function gb_to_index(b0,b1,b2,b3) {
  return ((((b0 - 129) * 10 + (b1 - 48)) * 126 + (b2 - 129)) * 10 + b3 - 48);
}

function index_to_gb(idx) {
  b3 = (idx % 10) + 48;
  idx /= 10;
  b2 = (idx % 126) + 129;
  idx /= 126;
  b1 = (idx % 10) + 48;
  b0 = (idx / 10) + 129;
  return sprintf("%02X%02X%02X%02X", b0, b1, b2, b3);
}

function decode_gb(str) {
  b0 = decode_hex(substr(str, 3, 2));
  b1 = decode_hex(substr(str, 7, 2));
  b2 = decode_hex(substr(str, 11, 2));
  b3 = decode_hex(substr(str, 15, 2));
  return gb_to_index(b0, b1, b2, b3);
}

function printline(from, to) {
  fromgb = index_to_gb(from);
  fromuni = gbtable[from];
  if (from == to)
    printf ("0x%s		U+%04X\n", fromgb, fromuni);
  else
    printf ("0x%s-0x%s	U+%04X-U+%04X\n", fromgb, index_to_gb(to),
	    fromuni, fromuni + (to - from));
}

/^<U[0-9A-F][0-9A-F][0-9A-F][0-9A-F]>/ {
  unicode = decode_hex(substr($1, 3, 4));
  if ($2 ~ /\\x8[1-4]\\x3[0-9]\\x[8-9A-F][0-9A-F]\\x3[0-9]/)
    unitable[unicode] = decode_gb($2);
  else
    unitable[unicode] = -1;
}

END {
  lastgb = 0;
  surrogate_min = decode_hex("D800");
  surrogate_max = decode_hex("DFFF");
  lastgb = unitable[128];
  gbtable[lastgb] = 128;
  for (i = 129; i < 65536; i++)
    {
      if (unitable[i] == 0 && (i < surrogate_min || i > surrogate_max))
	{
	  lastgb++;
	  gbtable[lastgb] = i;
	  unitable[i] = lastgb;
	}
      else if (unitable[i] > 0)
	{
	  lastgb = unitable[i];
	  gbtable[lastgb] = i;
	}
    }

  fromgb = lastgb = unitable[128];
  for (i = 129; i < 65536; i++)
    {
      if (unitable[i] > 0)
	{
	  if (lastgb + 1 == unitable[i])
	    {
	      lastgb++;
	    }
	  else
	    {
	      if (lastgb >= 0)
		printline(fromgb, lastgb);
	      fromgb = lastgb = unitable[i];
	    }
	}
      else			# i.e. (unitable[i] < 0)
	{
	  if (lastgb >= 0)
	    printline(fromgb, lastgb);
	  lastgb = -1;
	}
    }
  printline(fromgb, unitable[65535]);
}
