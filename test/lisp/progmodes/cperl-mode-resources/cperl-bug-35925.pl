# This resource file can be run with cperl--run-testcases from
# cperl-tests.el and works with both perl-mode and cperl-mode.

# -------- Bug#35925: input -------
format FH =
@### @.### @###
42, 3.1415, 0
.
write FH;

# -------- Bug#35925: expected output -------
format FH =
@### @.### @###
42, 3.1415, 0
.
write FH;

# -------- Bug#35925: end -------

# -------- format not as top-level: input -------
foo: {
    format STDOUT =
^<<<<
$foo
.
write;
}
# -------- format not as top-level: expected output -------
foo: {
    format STDOUT =
^<<<<
$foo
.
    write;
}
# -------- format not as top-level: end -------
