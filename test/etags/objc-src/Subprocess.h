/*
	Subprocess.h	(v10)
	by Charles L. Oei
	pty support by Joe Freeman
	with encouragement from Kristofer Younger
	Subprocess Example, Release 2.0
	NeXT Computer, Inc.

	You may freely copy, distribute and reuse the code in this example.
	NeXT disclaims any warranty of any kind, expressed or implied, as to
	its fitness for any particular use.

	Hacked up for use in PackageInspector by Tom Hageman.
*/

#import <objc/Object.h>
#import <stdio.h>

/*
    This subprocess object sends/receives data to/from any UNIX
    subprocess asynchronously (via vfork/pipe).
    Its delegate, if any, will receive the following messages:

	- subprocessDone;
	    // sent when the subprocess exits
    
	- subprocessOutput:(char *)buffer;
	    // sent whenever there is data on the standard output pipe;
	    // buffer is only valid until next call
	
	- subprocessError:(const char *)errorString;
	    // sent when an error occurs;
	    // if it ever happens, it's usually only at startup time

	// [TRH] and this is how these should have been done in the first place...
	- subprocessDone:(SubProcess *)sender;
	- subprocess:(SubProcess *)sender output:(char *)buffer;	
*/

// Hack to uniquize classname (to avoid dynload errors.)
#define Subprocess SubprocessForPackageInspector

#define BUFFERSIZE 2048

@interface Subprocess:Object
{
    FILE *fpToChild;
    int fromChild;
    int childPid;
    id delegate;
    int masterPty;	// file descriptor for master/slave pty
    int slavePty;
    int bufferCount;
    char outputBuffer[BUFFERSIZE];
}

- init:(const char *)subprocessString;
    // a cover for the below withDelegate:nil, andPtySupport:NO, andStdErr:YES

- init:(const char *)subprocessString
    withDelegate:theDelegate
    andPtySupport:(BOOL)wantsPty
    andStdErr:(BOOL)wantsStdErr;
    // optional requests for pseudo terminal support and
    // redirecting the standard error stream thru standard output

- send:(const char *)string withNewline:(BOOL)wantNewline;
    // send the string optionally followed by a new line
- send:(const char *)string;
    // sends the string followed by a new line
    // shorthand for above withNewline:YES
- terminateInput;
    // sends an end-of-file (EOF) to the subprocess
    // (and closes input pipe to child)
- terminate:sender;
    // forces the subprocess to terminate (w/ SIGTERM)

- setDelegate:anObject;
- delegate;

@end
