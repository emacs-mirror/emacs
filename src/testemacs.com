$ ! VMS command file to make definitions so that you can run
$ ! `temacs.exe' with the data file just dumped by build.com.
$
$ runtemacs :== $sys$disk:[]temacs -map temacs.dump
