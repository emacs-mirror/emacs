# This defines a bash command named `edit' which contacts/resumes an
# existing emacs or starts a new one if none exists.
#
# One way or another, any arguments are passed to emacs to specify files
# (provided you have loaded `resume.el').
#
# This function assumes the emacs program is named `emacs' and is somewhere
# in your load path.  If either of these is not true, the most portable
# (and convenient) thing to do is to make an alias called emacs which
# refers to the real program, e.g. 
#
#        alias emacs=/usr/local/bin/gemacs
#
# Written by Noah Friedman.

function edit ()
{
 local windowsys="${WINDOW_PARENT+sun}"

 windowsys="${windowsys:-${DISPLAY+x}}"

 if [ -n "${windowsys:+set}" ]; then
    # Do not just test if these files are sockets.  On some systems
    # ordinary files or fifos are used instead.  Just see if they exist.
    if [ -e "${HOME}/.emacs_server" -o -e "/tmp/esrv${UID}-"* ]; then
       emacsclient "$@"
       return $?
    else
       echo "edit: starting emacs in background..." 1>&2
    fi

    case "${windowsys}" in
      x ) (emacs "$@" &) ;;
      sun ) (emacstool "$@" &) ;;
    esac
 else   
    if jobs %emacs 2> /dev/null ; then
       echo "$(pwd)" "$@" >| ${HOME}/.emacs_args && fg %emacs
    else
       emacs "$@"
    fi
 fi
}


