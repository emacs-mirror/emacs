/*
    Structure for storing VMS specific information for an EMACS process

    We use the event flags 1-23 for processes, keyboard input and timer
*/

/*
    Same as MAXDESC in process.c
*/
#define	MAX_EVENT_FLAGS		23

typedef  struct {
    char	inputBuffer[1024];
    short	inputChan;
    short	outputChan;
    short	busy;
    int		pid;
    int		eventFlag;
    int		exitStatus;
    short       iosb[4];
} VMS_PROC_STUFF;
