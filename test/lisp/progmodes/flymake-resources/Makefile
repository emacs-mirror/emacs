# Makefile for flymake tests

CC_OPTS = -Wall -Wextra

## Recent gcc (e.g. 4.8.2 on RHEL7) can automatically colorize their output,
## which can confuse flymake.  Set GCC_COLORS to disable that.
## This only seems to be an issue in batch mode, where you would not
## normally use flymake, so it seems like just avoiding the issue
## in this test is fine.  Set flymake-log-level to 3 to investigate.
check-syntax:
	GCC_COLORS= $(CC) $(CC_OPTS) ${CHK_SOURCES} || true

# eof
