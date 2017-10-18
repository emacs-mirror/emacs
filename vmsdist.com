$ ! Command file to build Emacs on VMS and create a backup saveset.
$ ! rms, 16 May 1988
$
$ ! We assume that the files have been read in to a directory
$ ! $disk1:[rms.dist.emacs]; you should replace `$disk1:[rms.dist'
$ ! with the actual name of the superior directory of [.emacs].
$ ! This replacement needs to be done in a few places.
$
$ ! You should be in that directory when you run this command file.
$
$ ! We also assume that the tar-tape reader has changed all `-' to `_'
$ ! in filenames.  If this is not so, delete the first `@allrename' command.
$
$ ! You must also edit the mag tape device name
$ ! And the Emacs version that appears in the saveset name.
$
$ ! Change to vms 4.4 filenames.
$ @allrename [...] "_" "-"
$
$ ! Set up logical devices so we can compile and run Emacs.
$
$ edit/edt/nocommand emacs.com
s/sys$sysdevice:[emacs.]/$disk1:[rms.dist_18.emacs.]/w
exit
$ @emacs
$ ! Delete the modified emacs.com so we leave the source files clean.
$ ! emacs.com must be edited for real when Emacs is installed
$ ! on another machine.  This version wouldn't be right anyway.
$ delete emacs.com;0
$
$ ! Edit the configuration files.
$
$ set def [.src]
$ copy vmspaths.h paths.h
$ copy config.h-dist config.h
$ edit/edt/nocommand config.h
s/s-bsd4-2.h/s-vms5-5.h/w
exit
$
$ ! Compile, link and dump Emacs.
$
$ @compile
$ @link
$ @[-.etc]complink
$ @[-.etc]makedoc
$ @build
$
$ ! Move the executable and image to the appropriate place.
$
$ rename temacs.exe [-]emacs.exe
$ rename temacs.dump [-]emacs.dump
$ ! No longer necessary since VMS systems come with this file.
$ ! copy sys$library:vaxcrtl.olb vaxcrtl.olb
$
$ ! Recompile a few files for vms version 4.2.
$ ! Call these object files for 4.2 `.jbo'.
$
$ edit/edt/nocommand config.h
s/vms4-4/vms4-2/w
exit
$ rename doc.obj doc.obx
$ rename fileio.obj fileio.obx
$ rename sysdep.obj sysdep.obx
$ rename vmsfns.obj vmsfns.obx
$ @recomp doc
$ @recomp fileio
$ @recomp sysdep
$ @recomp vmsfns
$ rename doc.obj doc.jbo
$ rename fileio.obj fileio.jbo
$ rename sysdep.obj sysdep.jbo
$ rename vmsfns.obj vmsfns.jbo
$ rename *.obx *.obj
$ delete config.h;0
$
$ ! Change all files back to the names that VMS 4.2 can read in.
$
$ set def [-]
$ @allrename [...] "-" "_"
$
$ ! Dump onto tape
$
$ mount /foreign mua0:
$ define /translation=concealed dumping  $disk1:[rms.dist_18.]
$ set def dumping:[emacs]
$ backup /interchange /verify /list [...] mua0:emacs18.51
$ dismount mua0:
