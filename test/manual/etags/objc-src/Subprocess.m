/*
	Subprocess.m	(v10)
	by Charles L. Oei
	pty support by Joe Freeman
	Subprocess Example, Release 2.0
	NeXT Computer, Inc. 

	You may freely copy, distribute and reuse the code in this example.
	NeXT disclaims any warranty of any kind, expressed or implied, as to
	its fitness for any particular use.
*/

#import "Subprocess.h"
// #import <sgtty.h>	// needed to compile under Release 1.0
#import <appkit/nextstd.h>
#import <appkit/Application.h>
#import <appkit/Panel.h>
#import <sys/wait.h>

#define	PTY_TEMPLATE "/dev/pty??"
#define	PTY_LENGTH 11

static void showError();


/*==========================================================
 *
 * Private Instance Methods
 *
 *==========================================================*/

@interface Subprocess(Private)
- childDidExit;
- fdHandler:(int)theFd;
@end

@implementation Subprocess(Private)

- childDidExit
    // cleanup after a child process exits
{
    if (childPid)
    {
	union wait exitstatus;
	int waitresult;

	DPSRemoveFD(fromChild);
	close(fromChild);
	fclose(fpToChild);
	// Cleanup zombie processes. (blocking wait is too dangerous here...)
	waitresult = wait4(childPid, &exitstatus, WNOHANG, NULL);
	if (waitresult != childPid) {
		/* XXX should handle this gracefully, e.g, timed entry. */
	}
	childPid=0;	// specify that child is dead
	if (delegate)
	{    
	    if ([delegate respondsTo:@selector(subprocessDone:)])
		[delegate perform:@selector(subprocessDone:) with:self];
	    else if ([delegate respondsTo:@selector(subprocessDone)])
		[delegate perform:@selector(subprocessDone)];
	}
    }
    return self;
}

- fdHandler:(int)theFd
    // DPS handler for output from subprocess
{
    if ((bufferCount = read(theFd, outputBuffer, BUFFERSIZE-1)) <= 0)
    {
	[self childDidExit];
	return self;
    }
    outputBuffer[bufferCount] = '\0';
    if (delegate)
    {
        if ([delegate respondsTo:@selector(subprocess:output:)])
	    [delegate perform:@selector(subprocess:output:)
		  with:self with:(void *)&outputBuffer];
        else if ([delegate respondsTo:@selector(subprocessOutput:)])
	    [delegate perform:@selector(subprocessOutput:)
		  with:(void *)&outputBuffer];
    }
    return self;
}

@end


/*==========================================================
 *
 * Private Utility Routines
 *
 *==========================================================*/
 
static void
showError (const char *errorString, id theDelegate)
    // ensure errors never get dropped on the floor
{
    if (theDelegate && [theDelegate respondsTo:@selector(subprocessError:)])
	[theDelegate
	    perform:@selector(subprocessError:)
	    with:(void *)errorString];
    else if (NXApp)	// no delegate, but we're running w/in an App
	NXRunAlertPanel(0, errorString, 0, 0, 0);
    else
	perror(errorString);
}

static void
fdHandler (int theFd, id self)
    // DPS handler for output from subprocess
{
    [self fdHandler:theFd];
}

static void
getptys (int *master, int *slave)
    // attempt to setup the ptys
{
    char device[PTY_LENGTH];
    char *block, *num;
    char *blockLoc; // specifies the location of block for the device string
    char *numLoc; // specifies the pty name with the digit ptyxD
    char *msLoc; // specifies the master (ptyxx) or slave (ttyxx)
    
    struct sgttyb setp =
	{B9600, B9600, (char)0x7f, (char)0x15, (CRMOD|ANYP)};
    struct tchars setc =
	{CINTR, CQUIT, CSTART, CSTOP, CEOF, CBRK};
    struct ltchars sltc =
	{CSUSP, CDSUSP, CRPRNT, CFLUSH, CWERASE, CLNEXT};
    int	lset =
	(LCRTBS|LCRTERA|LCRTKIL|LCTLECH|LPENDIN|LDECCTQ);
    int	setd = NTTYDISC;
    
    strcpy(device, PTY_TEMPLATE); // string constants are not writable
    blockLoc = &device[ strlen("/dev/pty") ];
    numLoc = &device[ strlen("/dev/pty?") ];
    msLoc = &device[ strlen("/dev/") ];
    for (block = "pqrs"; *block; block++)
    {
	*blockLoc = *block;
	for (num = "0123456789abcdef"; *num; num++)
	{
	    *numLoc = *num;
	    *master = open(device, O_RDWR);
	    if (*master >= 0)
	    {
		*msLoc = 't';
		*slave = open(device, O_RDWR);
		if (*slave >= 0)
		{
		    (void) ioctl(*slave, TIOCSETP, (char *)&setp);
		    (void) ioctl(*slave, TIOCSETC, (char *)&setc);
		    (void) ioctl(*slave, TIOCSETD, (char *)&setd);
		    (void) ioctl(*slave, TIOCSLTC, (char *)&sltc);
		    (void) ioctl(*slave, TIOCLSET, (char *)&lset);
		    return;
		} else {
		    // close the master and reset the device
		    // name so that the master opens it properly
		    *msLoc = 'p';
		    close(*master);
		}
	    }
	} /* hunting through a bank of ptys */
    } /* hunting through blocks of ptys in all the right places */
    *master = -1;
    *slave = -1;
}


@implementation Subprocess

/*==========================================================
 *
 * Public Instance Methods
 *
 *==========================================================*/

- init:(const char *)subprocessString
    // a cover for the below withDelegate:nil, andPtySupport:NO, andStdErr:YES
{
    return
	[self
	    init:subprocessString
	    withDelegate:nil
	    andPtySupport:NO
	    andStdErr:YES];
}

- init:(const char *)subprocessString
    withDelegate:theDelegate
    andPtySupport:(BOOL)wantsPty
    andStdErr:(BOOL)wantsStdErr
    // initializes an instance of Subprocess and corresponding UNIX process
{
    int pipeTo[2];		// for non-Pty support
    int pipeFrom[2];
    int	tty, numFds, fd;	// for temporary use
    int processGroup;
    int pidChild;		// needed because childPid does not exist
				// until Subprocess is instantiated

    if (wantsPty)
    {
    	tty = open("/dev/tty", O_RDWR);
	getptys(&masterPty,&slavePty);
	if (masterPty <= 0 || slavePty <= 0)
	{
	    showError("Error grabbing ptys for subprocess.", theDelegate);
	    return self;
	}
	// remove the controlling tty if launched from a shell,
	// but not Workspace;
	// so that we have job control over the parent application in shell
	// and so that subprocesses can be restarted in Workspace
	if  ((tty<0) && ((tty = open("/dev/tty", 2))>=0))
	{
	    ioctl(tty, TIOCNOTTY, 0);
	    close(tty);
	}
    }
    else
    {
	if (pipe(pipeTo) < 0 || pipe(pipeFrom) < 0)
	{
	    showError("Error starting UNIX pipes to subprocess.", theDelegate);
	    return self;
	}
    }
    
    switch (pidChild = vfork())
    {
    case -1:	// error
	showError("Error starting UNIX vfork of subprocess.", theDelegate);
	return self;

    case 0:	// child
	if (wantsPty)
	{
	    dup2(slavePty, 0);
	    dup2(slavePty, 1);
	    if (wantsStdErr)
		dup2(slavePty, 2);
	}
	else
	{
	    dup2(pipeTo[0], 0);
	    dup2(pipeFrom[1], 1);
	    if (wantsStdErr)
		dup2(pipeFrom[1], 2);
	}
	
	numFds = getdtablesize();
	for (fd=3; fd<numFds; fd++)
	    close(fd);

	processGroup = getpid();
	ioctl(0, TIOCSPGRP, (char *)&processGroup);
	setpgrp (0, processGroup);
	
	// we exec a /bin/sh so that cmds are easier to specify for the user
	execl("/bin/sh", "sh", "-c", subprocessString, 0);
	perror("vfork (child)"); // should never gets here tho
	exit(1);

    default:	// parent
	[self setDelegate:theDelegate];
	childPid = pidChild;

	if (wantsPty)
	{
	    close(slavePty);
	    
	    fpToChild = fdopen(masterPty, "w");
	    fromChild = masterPty;
	}
	else
	{
	    close(pipeTo[0]);
	    close(pipeFrom[1]);
    
	    fpToChild = fdopen(pipeTo[1], "w");
	    fromChild = pipeFrom[0];
	}

	setbuf(fpToChild, NULL);
	DPSAddFD(
	    fromChild,
	    (DPSFDProc)fdHandler,
	    (id)self,
	    NX_MODALRESPTHRESHOLD+1);
	return self;
    }
}

- send:(const char *)string withNewline:(BOOL)wantNewline
{
    fputs(string, fpToChild);
    if (wantNewline)
        fputc('\n', fpToChild);
    return self;
}

- send:(const char *)string
{
    [self send:string withNewline:YES];
    return self;
}

- terminateInput
    // effectively sends an EOF to the child process stdin
{
    fclose(fpToChild);
    return self;
}

- terminate:sender
{
    if (childPid)
    {
	//kill(childPid+1, SIGTERM);
	killpg(childPid, SIGTERM);
	[self childDidExit];
    }
    return self;
}

- setDelegate:anObject
{
    delegate = anObject;
    return self;
}

- delegate
{
    return delegate;
}

@end
