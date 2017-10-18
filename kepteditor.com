$ verify = f$verify (0)
$ !
$ ! Kept_Editor.COM
$ ! Command file for use on VMS to spawn an Emacs process
$ ! that can be suspended with C-z and will not go away
$ ! when other programs are run.  This is the normal way
$ ! for users to invoke Emacs on VMS; the command "emacs"
$ ! is normally defined to execute this file.
$ ! That definition, and other definitions used by it, are done by `emacs.com'.
$ ! Users who want to use Emacs should have their `login.com' files
$ ! execute `emacs.com', which is to be found in this directory.
$ !
$ ! Joe Kelsey
$ ! FlexComm Corp.
$ !
$ ! September, 1985
$ !
$ ! Run or attach to an editor in a kept fork.
$ !
$ ! Modified by Marty Sasaki to define the job logical name
$ ! "EMACS_FILE_NAME" with the value of the filename on the command
$ ! line. Lisp code can then use the value of the logical to resume or
$ ! to start editing in that file.
$ !
$ !
$	edit		= ""
$	name		= p1 + " " + f$trnlnm ( "TT" ) - ":"
$	priv_list	= f$setprv ("NOWORLD, NOGROUP")
$	pid 		= 0
$ 10$:
$ 	proc		= f$getjpi ( f$pid ( pid ), "PRCNAM")
$ 	if proc .eqs. name then -
$		goto attach
$ 	if pid .ne. 0 then -
$		goto 10$
$ spawn:
$	args		= p2 + " " + p3 + " " + p4 + " " + p5 + " " + -
    			  p6 + " " + p7 + " " + p8
$ 	priv_list	= f$setprv ( priv_list )
$ 	write sys$error -
"[Spawning a new Kept ''P1']"
$	if p1 .nes. "TPU" then -
$		goto check_emacs
$ 	define/user	sys$input	sys$command
$ 	spawn	/process="''NAME'" -
    		/nolog -
    		edit/'p1' 'args'
$ 	goto quit1
$ check_emacs:
$	if p1 .nes. "EMACS" then -
$		goto un_kempt
$	define/user	sys$input	sys$command
$	spawn	/process="''NAME'" -
		/nolog -
		runemacs 'args'
$	goto quit1
$ un_kempt:
$ ! The editor is unruly - spawn a process and let the user deal with the
$ ! editor himself.
$	spawn	/process="''NAME'" -
		/nolog
$	goto quit1
$ attach:
$ 	priv_list	= f$setprv ( priv_list )
$	message_status = f$environment("message")
$	set noon
$	on control_y then goto quit
$	set message /nofacility/noidentification/noseverity/notext
$	if p2 .eqs. "" then goto no_logical
$	temp = f$trnlnm("SYS$DISK") + f$directory() + p2
$	temp = f$edit(temp,"lowercase")
$	define/nolog/job emacs_file_name "''temp'"
$ no_logical:
$ 	write sys$error -
"[Attaching to process ''NAME']"
$ 	define/user	sys$input	sys$command
$ 	attach "''NAME'"
$ quit:
$	set noon
$	if p2 .eqs. "" then goto quit1
$	deassign/job emacs_file_name
$ quit1:
$	set message 'message_status
$ 	write sys$error -
"[Attached to DCL in directory ''F$TRNLNM("SYS$DISK")'''F$DIRECTORY()']"
$ 	if verify then -
$		set verify
$	exit
