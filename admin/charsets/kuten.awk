/^[0-9]/ {
  ku=substr($1, 3, 2) + 32;
  ten=substr($1, 5, 2) + 32;
  printf "0x%02X%02X %s\n", ku, ten, $2;
}

# arch-tag: dade6b45-b4c5-42ab-9d49-d6bf23a710b6
