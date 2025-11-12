#!/bin/bash

# Presumably it is okay to use GNU Bash, because it is one of the most
# popular shells, and even if it is not available, the calling side
# inside Emacs knows how to handle errors gracefully.

#exec aspell "$@"

#rm -rf ~/lwf_mock-aspell.log

#printf 'date="%s"\n' "$(date --iso=seconds)" > /tmp/lwf_mock-aspell.log

#printf 'args="%s"\n' "$*" >> /tmp/lwf_mock-aspell.log || { printf "lwf:ERROR\n" ; exit 3 ; }

if [[ "$HOME" == '' ]] ; then
    printf "HOME is unset. Aspell usually fails in such a case\n" 1>&2
    exit 3
fi

dictionary=UNSET
repl=UNSET

show_vv()
{
    printf  '%s\n' "@(#) International Ispell Version 3.1.20 (but really Aspell 0.60.0)"
}

in_dict()
{
    local x=$1
    for y in ${sessiondict[@]}; do
        if [ $y = $x ]; then return 0; fi
    done
    return 1
}

imitate_pipe()
{
    local a
    show_vv
    while read a ; do
	#printf 'pipe="%s"\n' "$a" >> /tmp/lwf_mock-aspell.log
	if [[ "$a" == '' ]] ; then
	    printf ''
	elif [[ "$a" == '+' || "$a" == '~nroff' || "$a" == '~tex' || "$a" == '!' || "$a" == '-' || "$a" == '%' ]] ; then
	    printf ''
	elif [[ "${a:0:1}" == '@' ]] ; then
	    sessiondict+=("${a:1}")
	    printf ''
	else
	    for b in $a ; do
		if [[ "$b" == '^' ]] ; then
		    printf ''
		elif in_dict "$b" || in_dict "${b#^}" ; then
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
    : "This function is not used at the moment, but it might become
     useful eventually, if Emacs starts supporting calling the backend
     using the human interface, not just the pipe interface."
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
	    repl=pipe
            ;;
	-d)
	    if [ "$2" ]; then
                dictionary=$2
		if [[ "$dictionary" == "2110001888290146229" ]] ; then exit 3 ; fi
                shift
            else
                printf 'ERROR: "-d" requires an argument.' 1>&2
		exit 1
            fi
	    ;;
	-d*)
	    dictionary=${1#-d}
	    if [[ "$dictionary" == "2110001888290146229" ]] ; then exit 3 ; fi
	    ;;
	-?*)
	    :
	    ;;
        *)
            break
    esac
    shift
done

if [[ "$repl" == "pipe" ]] ; then
    imitate_pipe
else
    printf 'Usage: aspell [options] <command>\n'
    exit 4
fi

#printf 'this place should be unreachable\n' >> /tmp/lwf_mock-aspell.log
