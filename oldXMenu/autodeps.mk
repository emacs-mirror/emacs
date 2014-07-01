### autodeps.mk --- lwlib/Makefile fragment for GNU Emacs

## This is inserted in lwlib/Makefile if AUTO_DEPEND=yes.

-include $(ALLOBJS:%.o=${DEPDIR}/%.d)
