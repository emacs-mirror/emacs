//
//	SimpleCalc -- Randy Nelson -- NeXT Developer Training
//	A general class that serves as a liaison between a calculator interface
//	and a calculator engine.
//
//  You may freely copy, distribute and reuse the code in this example.
//  NeXT disclaims any warranty of any kind, expressed or implied, as to
//  its fitness for any particular use.
//
//	Created 8-8-90
//
#import <objc/Object.h>

@interface SimpleCalc:Object
{
    // outlets...the front-end.
    id 			display;
    id			enterKey;
    id			minusKey;
    id			infoManager;
    id			myNXStringTable;

    // C++ object's can be a-part-of Objective-C objects.
    class CalcEngine 	*cplus_object;	

    // record each previous user action
    SEL			previousAction;
}

// actions.

- numberKeys:sender;
- decimalKey:sender;
- operationKeys:sender;
- equalsKey:sender;
- clearKey:sender;
- clearAllKey:sender;
- infoPanel:sender;
- helpPanel:sender;

// delegate methods.

- windowWillClose:sender;
- appDidInit:sender;

- registerAction:(SEL)action;
- appendToDisplay:(const char *)theString;

@end
