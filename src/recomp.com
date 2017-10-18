$ ! VMS command file to recompile a .C file which needs recompilation.
$ ! This is a .C files that has no .OBJ file or that is newer
$ ! than the corresponding .OBJ file.  This file is self contained
$ ! and does not require you to do anything before running it.
$
$    file = f$search(f$parse(p1, ".C"), 1)
$    cmd = p2
$    if cmd .nes. "" then goto havcmd
$    if "''ccom'" .eqs. "" then @precomp
$    cmd = "ccom"
$havcmd:
$    name = f$parse(file,,,"NAME")
$    obj = name + ".OBJ"
$    if f$search(obj) .eqs. "" then goto docmd
$    if f$cvtime(f$file(file, "RDT")) .les. f$cvtime(f$file(obj, "RDT")) then -
	exit
$ docmd:
$    write sys$output "Compiling ''name'..."
$    'cmd' 'file'
$    purge /nolog 'obj'
$    write sys$output "---------------"
