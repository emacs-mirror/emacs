#!/bin/sh
# -*- eval: (bug-reference-mode 1) -*-

setlock -n /tmp/getmail.lock && echo getmail isn\'t running

# adsgsdg

if foo; then
    if bar; then
	toto
    fi
fi                              # bug#15613

case $X in
    foo)
        do_something
        ;;
    arg=*)			# bug#12953
        do_something_else_based_on_arg
        ;;
    *)
        default
        ;;
esac

echo -n $(( 5 << 2 ))
# This should not be treated as a heredoc (bug#12770).
2

foo='bar<<'                     # bug#11263
echo ${foo%<<aa}                # bug#11263
echo $((1<<8))                  # bug#11263
echo $[1<<8]                    # bug#11263

declare -a VERSION
for i in $(ls "$PREFIX/sbin") ; do
    echo -e $N')' $i
    VERSION[${#VERSION[*]}]=$i  # bug#11946.
    N=$(($N + 1))
done

foo () {

    bar () {
        blilbi
    }

    case toto
    in a) hello                 # KNOWN INDENT BUG
    ;; b) hi                    # KNOWN INDENT BUG
    ;; c) hi                    # KNOWN INDENT BUG
    esac

    case $toto in
        a) echo 1;; b) echo 2;;
        (c)
            echo 3;;
        d)
            echo 3;;
    esac
    
    case $as_nl`(ac_space=' '; set) 2>&1` in #(
        *${as_nl}ac_space=\ *)
            # `set' does not quote correctly, so add quotes: double-quote
            # substitution turns \\\\ into \\, and sed turns \\ into \.
            sed -n \
	        "s/'/'\\\\''/g;
                 s/^\\([_$as_cr_alnum]*_cv_[_$as_cr_alnum]*\\)=/\\1=''/p"
            ;; #(
        *)
            # `set' quotes correctly as required by POSIX, so do not add
            # quotes.
            sed -n "/^[_$as_cr_alnum]*_cv_[_$as_cr_alnum]*=/p"
            ;;
    esac |
        grep '.' |              # KNOWN INDENT BUG
        sed 1d
    
    case toto in
        -exec-prefix=* | --exec_prefix=* | --exec-prefix=* | --exec-prefi=* \
            | --exec-pref=* | --exec-pre=* | --exec-pr=* | --exec-p=* \
            | --exec=* | --exe=* | --ex=*)
            exec_prefix=$ac_optarg ;;
        5)
            hello ;;
        3) hello $(adfad)
           echo esac ;;         # KNOWN INDENT BUG
        5) hello ;;
        4) hello ;&
        4) hello ;;&
        5) hello ;;
        5) hello ;;
    esac
    
    echo "'" wfgfe

    #!/bin/bash
    cat << EOF \
        | cat sadfsafd \
              sadfsafd           "KNOWN INDENT BUG" \
        | tee -a bug.txt
asdfsaf
This is a test case for a bug in bash shell mode text highlighting
EOF

    cat <<EOF1 <<EOF2           # KNOWN INDENT BUG
help1
EOF1
help2
EOF2
}
bar () {
    if [ $# == 0 ]; then
        while
            f                   # KNOWN INDENT BUG
        do
            bla;
        done
        echo "Highlighting is screwed up now"
        if [ 1 = 1 ]; then
            # adsgsdg
            echo "screwed up"
        fi
        
        $@ $? $#
        
        for f in *
        do
            sdfg
        done
        
        if swrgfef
        then blas
        else sdf
        fi
        
    fi
}
