//	SimpleCalc -- Randy Nelson -- NeXT Developer Training
//	A general class that serves as a liaison between a calculator interface
//	and a calculator engine.
//
//	You may freely copy, distribute and reuse the code in this example.
//	NeXT disclaims any warranty of any kind, expressed or implied, as to
//	its fitness for any particular use.
//
//	Created 8-22-90
//
// 	C++ "linkage" directive - tells the C++ compiler that the following 
//	interface files contain Objective-C code.

extern "Objective-C"		
{				
#import <appkit/Application.h>
#import <appkit/Panel.h>
#import <appkit/TextField.h>
#import <appkit/Button.h>
}

extern "C"
{
#import <appkit/publicWraps.h>
#import <objc/error.h>
#import <objc/NXStringTable.h>
#import <strings.h>
}

//	The C++ "linkage" directive serves two purposes (when importing
//	interface files that contain straight ANSI-C/Objective-C code). It:
//
// 	(a) allows you to link with libraries that have not been compiled with
//	the C++ compiler. Since libraries on the NeXT computer are compiled 
//	with the Objective-C compiler (cc, not cc++), you must use the C++ 
//	linkage directive when importing interface files that represent NeXT 
//	libraries (or any library that is not compiled with cc++). 
//
//	(b) tells the compiler to ignore C++ keywords that will result in
//	syntax errors when importing ANSI-C/Objective-C interface files. 
//	The linkage directive essentially tells the C++ compiler to treat 
//	keywords (such as "new", "delete", etc.) as normal identifiers.

#import "SimpleCalc.h"
#import "CalcEngine.h"
#import "InfoManager.h"

@implementation SimpleCalc

// Initialize an instance of the SimpleCalc class.  One instance variable of
// that class is the C++ calculator engine.
- init
{
    cplus_object = new CalcEngine; // new is a keyword in C++.
    previousAction = 0; 
    return self;
}

// Append a new digit entered by the user to the text field display.
- appendToDisplay:(const char *)theDigit
{
    char *copyOfDisplay = NXCopyStringBuffer([display stringValue]);

    [display setStringValue: strcat(copyOfDisplay, theDigit)];

    return self;
}

// We need to keep a history of one action to make decisions about the display.
- registerAction:(SEL)action
{
    previousAction = action;
    return self;
}

// The user has pushed the decimal key on the calculator.
- decimalKey:sender
{
    if (previousAction == @selector(operationKeys:))
	[display setStringValue:"."];
    else {
	if (strchr([display stringValue], '.'))
	    NXBeep();
	else 
	    [self appendToDisplay:"."];
    }
    return [self registerAction:_cmd];
}

// One of the number keys was selected by the user.
- numberKeys:sender
{	
    char aDigit[2];
    int digit = [sender selectedTag];

    sprintf(aDigit, "%d", digit);

    if (previousAction == @selector(operationKeys:) ||
	previousAction == @selector(equalsKey:))
    {
	[display setStringValue:aDigit];
    } else {
	if ([display doubleValue] == 0 && !strchr([display stringValue], '.'))
	    [display setStringValue:aDigit];
	else
	    [self appendToDisplay:aDigit];
    }
    return [self registerAction:_cmd];
}

// The user pressed the equals key on the calculator interface.
- equalsKey:sender
{
    if (previousAction == 0) 
	NXBeep();
    else {
	NX_DURING
	    [display setDoubleValue:
		cplus_object->equalsKey([display doubleValue])];
	NX_HANDLER
	    NXRunAlertPanel(
	    	[myNXStringTable valueForStringKey:"operationFailed"], 
		[myNXStringTable valueForStringKey:NXLocalHandler.data1],
		[myNXStringTable valueForStringKey:"OK"], NULL, NULL);
	NX_ENDHANDLER
    }
    return [self registerAction:_cmd];
}

// The user pressed one of the operation keys.
- operationKeys:sender
{
    if (previousAction == 0) 
	NXBeep();
    else if (previousAction == @selector(operationKeys:)) 
	cplus_object->setOperation([sender selectedTag]);
    else {
	NX_DURING
	    [display setDoubleValue:
		cplus_object->operationKeys([sender selectedTag],
					    [display doubleValue])];
	NX_HANDLER
	    NXRunAlertPanel(
	    	[myNXStringTable valueForStringKey:"operationFailed"], 
		[myNXStringTable valueForStringKey:NXLocalHandler.data1],
		[myNXStringTable valueForStringKey:"OK"], NULL, NULL);
	NX_ENDHANDLER
    }
    return [self registerAction:_cmd];
}

// User pressed the Clear key.
- clearKey:sender
{
    [display setStringValue:"0"];
    return self;
}

// User pressed the Clear All key.
- clearAllKey:sender
{
    cplus_object->clear();
    [self registerAction:0];
    return [self clearKey:sender];
}

// Called just after the application initializes and starts up.
- appDidInit:sender
{	
    // Set the Enter key on the keypad to be equivalent to the = key.
    [[display window] addToEventMask:NX_SYMBOLSET];
    [enterKey setKeyEquivalent:3];
    [[display window] makeKeyAndOrderFront:self];
    return self;
}

// Called just before the window closes.
- windowWillClose:sender
{
    return [NXApp terminate:self];
}

// Brings up the Info panel.   Not done on startup because it's in a separate
// interface file.  Saves startup time for the user if we do this when they ask
// for it, and not before.
- infoPanel:sender
{
    if(infoManager == nil){
	infoManager = [[InfoManager alloc] init];
    }
    [infoManager orderInfoPanelFront:sender];
    return self;	
}

// Brings up the Help panel.   Not done on startup because it's in a separate
// interface file.  Saves startup time for the user if we do this when they ask
// for it, and not before.
- helpPanel:sender
{
    if(infoManager == nil){
	infoManager = [[InfoManager alloc] init];
    }
    [infoManager orderHelpPanelFront:sender];
    return self;	
}

@end
