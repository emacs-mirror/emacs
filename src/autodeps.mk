/* autodeps.mk --- src/Makefile fragment for GNU Emacs

This is inserted in src/Makefile if AUTO_DEPEND=yes.  */

ALLOBJS=$(START_FILES) ${obj} ${otherobj} prefix-args.o
-include $(ALLOBJS:%.o=${DEPDIR}/%.d)
