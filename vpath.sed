/^VPATH *=/c\
# This works only in GNU make.  Using the patterns avoids\
# object files being found by VPATH, and thus permits building\
# when $srcdir is configured itself.\
vpath %.c $(srcdir)\
vpath %.h $(srcdir)\
\

# arch-tag: 56a64b50-e4e8-443a-960f-f13af0f1a545
