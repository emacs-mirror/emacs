#!/bin/bash

vv=

show_vv()
{
    printf  '%s\n' "@(#) International Ispell Version 3.1.20 (but really Aspell 0.60.0)"
}

imitate_repl()
{
    while true ; do
	read a
#	printf 'debug="%s"\n' "$a"
	if [[ "$a" == '' ]] ; then
	    printf ''
	elif [[ "$a" == 'tarampampamtararam' ]] ; then
	    printf '# tarampampamtararam 0\n\n' # wrong word
	else
	    printf '*\n\n'
	fi
    done
}

show_vv

while :; do
    case $1 in
        -vv|-v)
            #show_vv # for ispell.el error detection
            exit
            ;;
        -a)       # imitate REPL
	    imitate_repl
            ;;
	-?*)
            printf 'WARN: Unknown option (ignored): %s\n' "$1" >&2
	    ;;
        *)
            break
    esac
    shift
done
