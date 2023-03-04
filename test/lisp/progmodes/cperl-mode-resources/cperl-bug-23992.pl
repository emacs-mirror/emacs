# Test file for Bug#23992
#
# The "||" case is directly from the report,
# the "&&" case has been added for symmetry.

s/LEFT/L/g || s/RIGHT/R/g || s/aVALUE\D+//g;
s/LEFT/L/g||s/RIGHT/R/g||s/aVALUE\D+//g;

s/LEFT/L/g && s/RIGHT/R/g && s/aVALUE\D+//g;
s/LEFT/L/g&&s/RIGHT/R/g&&s/aVALUE\D+//g;
