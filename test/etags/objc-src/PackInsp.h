/*+++*
 *  title:	PackageInspector.h
 *  abstract:	interface definitions for WM PackageInspector 
 *  author:	T.R.Hageman, Groningen, The Netherlands
 *  created:	November 1994
 *  modified:	(see RCS Log at end)
 *  copyleft:
 *
 *		Copyright (C) 1994,1995  Tom R. Hageman.
 *
 *	This is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this software; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  description:
 *
 *---*/

#import <appkit/appkit.h>
#import <apps/Workspace.h>

#include <sys/stat.h>

#import "Subprocess.h"

#define NUMSTATS	4
#define TYPESTOSTAT	"bom", "info", "sizes", "tiff"

@interface PackageInspector:WMInspector
{
	// Outlets
    id	packageArchesField;
    id	packageDescriptionText;
    id	packageIconButton;
    id	packageLocationField;
    id	packageSizesField;
    id	packageStatusField;
    id	packageTitleField;
    id	packageVersionField;

    id  inspectorVersionField;
    id	infoPanel;
    id	infoVersionField;

	// other variables.
	NXBundle *bundle;		// class bundle.
	NXBundle *package;		// package bundle.
	struct stat stats[NUMSTATS];	// for lazy inspection.
	enum { listContents, listDescription } revertButtonState;

	Subprocess *archProcess;	// To determine architectures.
}

// Actions.
-showInfo:sender;

-open:sender;

// The workhorses
-(BOOL)shouldLoad;
-load;
-toggleDescription;

// Load helper methods
-loadKeyValuesFrom:(const char *)type inTable:(HashTable *)table;
-loadContentsOf:(const char *)type inTable:(HashTable *)table;
-loadImage;

// Support methods
-(const char *)getPath:(char *)path forType:(const char *)type;
-setRevertButtonTitle;
-(const char *)formatSize:(const char *)size inBuf:(char *)buf;

// Determine architectures, in separate subprocess.
-(void)getArchs;
// Subprocess [TRH-enhanced] delegate methods:
// Subprocess delegate methods:
-subprocess:(Subprocess *)sender output:(char *)buffer;
-subprocessDone:(Subprocess *)sender;

@end // PackageInspector

/*======================================================================
 * PackageInspector.h,v
 * Revision 1.7  1995/08/17 22:18:24  tom
 * (-open:): new method.
 *
 * Revision 1.6  1995/07/30 16:59:51  tom
 * import Subprocess.h; (archProcess): new ivar;
 * (-getArchs,-subprocess:output:,-subprocessDone:): new methods;
 * added for asynchronous arch-determination.
 *
 * Revision 1.5  1995/07/29 02:59:55  tom
 * (NUMSTATS,TYPESTOSTAT): new defines, (stats[NUMSTATS]): new ivar, replaces
 * bomstat, infostat, t ogeneralize lazy-load code.
 *
 * Revision 1.4  1995/04/02  02:39:05  tom
 * (package): NXBundle instead of (const char *). so that localized info files
 *  are found. (this loses out if *.pkg is a symbolic link, though.)
 *
 * Revision 1.3  1994/12/07  00:00:36  tom
 * add GNU copleft comment.
 *
 * Revision 1.2  1994/11/25  20:18:56  tom
 * (package ivar): use (char*) instead of (NXBundle*) to workaround symlink problems
 *
 * Revision 1.1  1994/11/24  22:39:56  tom
 * Initial revision
 *
 *======================================================================*/
