#!/bin/bash

#exec aspell "$@"

#rm -rf ~/lwf_mock-aspell.log

#printf 'date="%s"\n' "$(date --iso=seconds)" > /tmp/lwf_mock-aspell.log

#printf 'args="%s"\n' "$*" >> /tmp/lwf_mock-aspell.log || { printf "lwf:ERROR\n" ; exit 3 ; }

# coproc aspell { aspell "$@" ; }

if [[ "$HOME" == '' ]] ; then
    echo "HOME is unset. Aspell usually fails in such a case\n" 1>2
    exit 3
fi

vv=

show_vv()
{
    printf  '%s\n' "@(#) International Ispell Version 3.1.20 (but really Aspell 0.60.0)"
}

imitate_pipe()
{
    local a
    declare -A sessiondict
    show_vv
    while read a ; do
	#printf 'pipe="%s"\n' "$a" >> /tmp/lwf_mock-aspell.log
	if [[ "$a" == '' ]] ; then
	    printf ''
	elif [[ "$a" == '+' || "$a" == '~nroff' || "$a" == '~tex' || "$a" == '!' || "$a" == '-' || "$a" == '%' ]] ; then
	    printf ''
	elif [[ "${a:0:1}" == '@' ]] ; then
	    sessiondict["${a:1}"]="true"
	    printf ''
	else
	    for b in $a ; do
		if [[ "$b" == '^' ]] ; then
		    printf ''
		elif [[ ${sessiondict[$b]} == 'true' || ${sessiondict[${b#^}]} == 'true' ]] ; then
		    printf '*\n'
		elif [[ "$b" == '^tarampampamtararam' || "$b" == 'tarampampamtararam' ]] ; then
		    printf '# tarampampamtararam 0\n' # wrong word
		elif [[ "$b" == '^badworddd' || "$b" == 'badworddd' ]] ; then
		    printf '# badworddd 0\n' # wrong word
		elif [[ "$b" == '^hellooooooo' || "$b" == 'hellooooooo' ]] ; then
		    printf '# hellooooooo 0\n' # wrong word
		elif [[ "$b" == '^' ]] ; then
		    printf '\n'
		else
		    printf "*\n"
		fi
	    done
	    printf '\n'
	fi
    done
}

imitate_interactive()
{
    exit 6
    while true ; do
	read a
#	printf 'interactive="%s"\n' "$a" >> /tmp/lwf_mock-aspell.log
	if [[ "$a" == '' ]] ; then
	    printf '\n'
	elif [[ "$a" == 'tarampampamtararam' ]] ; then
	    printf '# tarampampamtararam 0\n\n' # wrong word
	else
	    printf '*\n\n'
	fi
    done
}


while :; do
    case $1 in
        -vv|-v)
            show_vv # for ispell.el version detection
            exit
            ;;
        -a)       # imitate REPL
	    imitate_pipe
	    exit
            ;;
	-?*)
            printf 'WARN: Unknown option (ignored): %s\n' "$1" >&2
	    ;;
        *)
            break
    esac
    shift
done

printf 'Usage: aspell [options] <command>\n'

#printf 'this place should be unreachable\n' >> /tmp/lwf_mock-aspell.log
