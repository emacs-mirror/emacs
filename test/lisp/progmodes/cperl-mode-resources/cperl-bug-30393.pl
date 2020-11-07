# -------- bug#30393: input --------
#
          my $sql = "insert into jobs (id, priority) values (1, 2);";
               my $sth = $dbh->prepare($sql) or die "bother";

          my $sql = "insert into jobs
(id, priority)
values (1, 2);";
               my $sth = $dbh->prepare($sql) or die "bother";
# -------- bug#30393: expected output --------
#
my $sql = "insert into jobs (id, priority) values (1, 2);";
my $sth = $dbh->prepare($sql) or die "bother";

my $sql = "insert into jobs
(id, priority)
values (1, 2);";
my $sth = $dbh->prepare($sql) or die "bother";
# -------- bug#30393: end --------
