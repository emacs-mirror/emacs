/*+++*
 *  title:	PackageInspector.m
 *  abstract:	NEXTSTEP Workspace Manager Inspector for Installer ".pkg" files.
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

#ifdef RCS_ID
static const char RCSid[] =
"PackageInspector.m,v 1.8 1995/09/01 21:46:27";
#endif

#define VERSION	"0.951"

#ifndef DEBUG
#   define DEBUG 0
#endif
#define LISTCONTENTS	0	// List Contents not yet implemented

#import "PackageInspector.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

// Localized strings
#define OPENBUTTON		NXLocalizedStringFromTableInBundle(NULL, bundle, "Open", NULL, button label)
#define LISTCONTENTSBUTTON	NXLocalizedStringFromTableInBundle(NULL, bundle, "List Contents", NULL, button label)
#define LISTDESCRIPTIONBUTTON	NXLocalizedStringFromTableInBundle(NULL, bundle, "Description", NULL, button label)

// States
#define STATE_UNINSTALLED	NXLocalizedStringFromTableInBundle(NULL, bundle, "Uninstalled", NULL, original package state)
#define STATE_INSTALLED		NXLocalizedStringFromTableInBundle(NULL, bundle, "installed", "Installed", package has been uncompressed unto disk)
#define STATE_COMPRESSD		NXLocalizedStringFromTableInBundle(NULL, bundle, "compressed", "Compressed", installed package has been recompressed)

// so InfoView.strings can be ripped off from Installer.app
#define SIZEFORMAT		NXLocalizedStringFromTableInBundle("InfoView", bundle, "%s installed, %s compressed", NULL, Short indication to user about the size of a package once installed and the size when compressed)
#define KBYTES			NXLocalizedStringFromTableInBundle("InfoView", bundle, "KB", NULL, Kilobytes -- package size)
#define MBYTES			NXLocalizedStringFromTableInBundle("InfoView", bundle, "MB", NULL, MegaBytes -- package size)

#define LOCALIZE(s)		NXLoadLocalizedStringFromTableInBundle(NULL, bundle, s, NULL)
#define LOCALIZE_ARCH(s)	NXLoadLocalizedStringFromTableInBundle("Architectures", bundle, s, NULL)


@implementation PackageInspector

+new
{
	static PackageInspector *instance;
	
	if (instance == nil) {
		char path[MAXPATHLEN+1];
		const char *nibname = [self name];

		instance = [super new];

		instance->bundle = [NXBundle bundleForClass:self];

		if ([instance->bundle getPath:path forResource:nibname ofType:"nib"] &&
		    [NXApp loadNibFile:path owner:instance]) {
			[instance->inspectorVersionField setStringValue:VERSION];
			[instance->packageDescriptionText setVertResizable:YES]; // ??Necessary??
		}
		else {
		    	fprintf(stderr, "Couldn't load %s.nib\n", nibname);
			[instance free];
			instance = nil;
		}
	}
	return instance;
}

-showInfo:sender
{
	if (infoPanel == nil) {
		char path[MAXPATHLEN+1];

		if ([bundle getPath:path forResource:"Info" ofType:"nib"] &&
		    [NXApp loadNibFile:path owner:self]) {
			[infoVersionField setStringValue:[inspectorVersionField stringValue]];
		}
	}
	[infoPanel makeKeyAndOrderFront:sender];
	return self;
}

-revert:sender
{
	[super revert:sender];

	if ([self selectionCount] != 1) {
		return nil;
	}
	if (sender == [self revertButton]) {
		[self toggleDescription];
	}
	else {
		char path[MAXPATHLEN+1];

		[package free];
		[self selectionPathsInto:path separator:'\0'];
		if (!(package = [[NXBundle allocFromZone:[self zone]] initForDirectory:path])) {
			return nil;
		}
		if ([self shouldLoad]) {
			[self load];
			revertButtonState = listContents;
		}
	}
	[[[self okButton] setTitle:OPENBUTTON] setEnabled:YES];
	[self setRevertButtonTitle];

	return self;
}

-ok:sender
{
	[self perform:@selector(open:) with:sender afterDelay:0 cancelPrevious:NO];
	[super ok:sender];
	return self;
}

-load
{
	char buf[256], size[2][20];
	HashTable *table = [[HashTable alloc] initKeyDesc:"*" valueDesc:"*"];

	[self getArchs];
	// Collect information about the package in a hashtable.
	[self loadKeyValuesFrom:"info" inTable:table];
	[self loadKeyValuesFrom:"sizes" inTable:table];
	[self loadContentsOf:"location" inTable:table];
	[self loadContentsOf:"status" inTable:table];

	// Convenience macro.
#define LOOKUP(key, notfound)	([table isKey:key] ? [table valueForKey:key] : \
				 (notfound))
#if 0
	// Set the various controls.
	sprintf(buf, "<<not yet implemented>>");
	// Well then, how *DOES* Installer determine this??? 
	[packageArchesField setStringValue:buf];
#endif
	[packageDescriptionText setText:LOOKUP("Description", "")];
	[packageLocationField setStringValue:
	 LOOKUP("location", LOOKUP("DefaultLocation", "???"))];

	[self formatSize:[table valueForKey:"InstalledSize"] inBuf:size[0]];
	[self formatSize:[table valueForKey:"CompressedSize"] inBuf:size[1]];
	sprintf(buf, SIZEFORMAT, size[0], size[1]);
	[packageSizesField setStringValue:buf];

	[packageStatusField setStringValue:LOCALIZE(LOOKUP("status", "Uninstalled"))];
	[packageTitleField setStringValue:LOOKUP("Title", "???")];
	[packageVersionField setStringValue:LOOKUP("Version", "???")];
#undef LOOKUP
	// Is this how one frees the contents of a hashtable?
	[table freeKeys:free values:free];
	[table free];

	[self loadImage];

	return self;
}

-loadKeyValuesFrom:(const char *)type inTable:(HashTable *)table
{
	char path[MAXPATHLEN+1];
	NXStream *stream;

	if (stream = NXMapFile([self getPath:path forType:type], NX_READONLY)) {
		int c;

#if DEBUG & 1
		fprintf(stderr, "loadKeyValuesFrom:%s\n", path);
#endif
		while ((c = NXGetc(stream)) >= 0) {
			// Buffer sizes should be enough, according to doc.
			char key[1024+1], value[1024+1];
			char *p;

			if (NXIsSpace(c)) continue;
			if (c == '#') {
				while ((c = NXGetc(stream)) >= 0 && c != '\n') ;
				continue;
			}
			// Found key; collect it.
			p = key;
			do {
				if (p < &key[sizeof key-1]) *p++ = c;
			} while ((c = NXGetc(stream)) >= 0 && !NXIsSpace(c));
			*p = '\0';

			// Skip over spaces and tabs.
			while (c == ' ' || c == '\t') c = NXGetc(stream);

			// Value is rest of line, up to newline.
			p = value;
			do {
				if (p < &value[sizeof value-1]) *p++ = c;
			} while ((c = NXGetc(stream)) >= 0 && c != '\n');
			*p = '\0';

			// Insert key/value pair in hashtable.
#if DEBUG & 1
			fprintf(stderr, "key:%s value:%s\n", key, value);
#endif
			[table insertKey:NXCopyStringBuffer(key)
			 value:NXCopyStringBuffer(value)];
		}

		NXCloseMemory(stream, NX_FREEBUFFER);
	}
	return self;

}

-loadContentsOf:(const char *)type inTable:(HashTable *)table
{
	char path[MAXPATHLEN+1];
	NXStream *stream;

	if (stream = NXMapFile([self getPath:path forType:type], NX_READONLY)) {
		char line[1024+1];
		int n = NXRead(stream, line, sizeof line);

		if (n > 0 && line[n-1] == '\n') line[n-1] = '\0';	// remove trailing newline.

		NXCloseMemory(stream, NX_FREEBUFFER);

		[table insertKey:NXCopyStringBuffer(type)
		 value:NXCopyStringBuffer(line)];
	}
	return self;
}

-loadImage
{
	char path[MAXPATHLEN+1];
	NXImage *image;

	// Remove old image from the button.
	if (image = [packageIconButton image]) {
		[packageIconButton setImage:nil];
		[image free];
	}
	// Get the image (if any) from the package
	image = [[NXImage allocFromZone:[self zone]] initFromFile:[self getPath:path forType:"tiff"]];
	[packageIconButton setImage:image];

	return self;
}


#define STAT_EQ(s1, s2)	((s1)->st_ino == (s2)->st_ino && \
			 (s1)->st_dev == (s2)->st_dev && \
			 (s1)->st_mtime == (s2)->st_mtime && \
			 (s1)->st_size == (s2)->st_size)

-(BOOL)shouldLoad
{
	char path[MAXPATHLEN+1];
	struct stat newstats[NUMSTATS];
	static const char * const typesToStat[NUMSTATS] = { TYPESTOSTAT };
	BOOL result = NO;
	int i;

	for (i = 0;  i < NUMSTATS;  i++) {
		memset(&newstats[i], 0, sizeof(struct stat));
		if (!(stat([self getPath:path forType:typesToStat[i]], &newstats[i]) == 0 &&
		      STAT_EQ(&newstats[i], &stats[i]))) {
			result = YES;
			///break; // NOT!!! must stat all for accurate cache.
		}
		stats[i] = newstats[i];
	}

	return result;
}

-toggleDescription
{
	switch (revertButtonState) {
	case listContents:
		// TODO: swap views?
		revertButtonState = listDescription;
		break;
	case listDescription:
		revertButtonState = listContents;
		break;
	}
	return [self setRevertButtonTitle];
}


// Support methods
-(const char *)getPath:(char *)buf forType:(const char *)type
{
	char name[MAXPATHLEN+1];

	// Get package name, sans extension.
	*strrchr(strcpy(name, strrchr([package directory], '/')+1), '.') = '\0';

	// Now get the full pathname.
	[package getPath:buf forResource:name ofType:type];
#if DEBUG & 2
	fprintf(stderr, "PackageInspector: type=\"%s\" name=\"%s\" path=\"%s\"\n",
		type, name, buf);
#endif
	return buf;
}

-setRevertButtonTitle
{
#if LISTCONTENTS
	[[[self revertButton]
	  setTitle:LOCALIZE(revertButtonState == listContents ?
			    "List Contents" : "Description")]
	 setEnabled:YES];
#endif
	return self;
}

-(const char *)formatSize:(const char *)size inBuf:(char *)buf
{
	// [TRH] this is very simplistic (but seems consistent with Installer.app)
	if (!size) {
		strcpy(buf, "???");
	}
	else {
		int len = strlen(size);
		if (len < 4) {
			sprintf(buf, "%s%s", size, KBYTES);
		}
		else if (len < 6) {
			sprintf(buf, "%.*s.%.*s%s",
				(len-3), size, 3-(len-3), size+(len-3), MBYTES); 
		}
		else {
			sprintf(buf, "%.*s%s", (len-3), size, MBYTES);
		}
	}
	return buf;
}

// Determine architectures, in separate subprocess.

#define WORKING	" ..."	// `I'm still busy' indicator.

-(void)getArchs
{
	char command[2*MAXPATHLEN+10+1];

	if (archProcess) [archProcess terminate:self];

	[packageArchesField setStringValue:WORKING];

	[bundle getPath:command forResource:"archbom" ofType:NULL];
	strcat(command, " ");
	[self getPath:&command[strlen(command)] forType:"bom"];
	archProcess = [[Subprocess allocFromZone:[self zone]] init:command
		       withDelegate:self andPtySupport:NO andStdErr:NO];
}

-(void)addArchs:(const char *)string
{
	char result[1024];	// Should be big enough...
	const char *s;
	char *d;

	strcpy(result, [packageArchesField stringValue]);
	if ((d = strstr(result, WORKING)) != NULL) {
		*d = '\0';
	}
	else {
		d = result + strlen(result);
	}
	if ((s = string)) {
		do {
			char name[100];
			char *t = name;

			while (*s && !NXIsAlNum(*s)) {
				if (*s == '\n') {
					*d++ = ' ', s++;
				}
				else {
					*d++ = *s++;
				}
			}
			while (NXIsAlNum(*s)) *t++ = *s++;
			*t = '\0';
			if (t > name) {
#if DEBUG & 4
				fprintf(stderr, "addArchs:\"%s\" localized: \"%s\"\n", name, LOCALIZE_ARCH(name));
#endif
				strcpy(d, LOCALIZE_ARCH(name));
				d += strlen(d);
			}
		} while (*s);

		strcpy(d, WORKING);
	}
	[packageArchesField setStringValue:result];
	[window displayIfNeeded]; // necessary??
}

-subprocess:(Subprocess *)sender output:(char *)buffer
{
	if (sender == archProcess) {
		[self addArchs:buffer];
	}
	return self;
}

-subprocessDone:(Subprocess *)sender
{
	if (sender == archProcess) {
		archProcess = nil;
		[self addArchs:NULL];
	}
	[sender free];
	return self;
}

static void openInWorkspace(const char *filename)
{
	// Indirect approach to circumvent Workspace deadlock/timeout.
	char command[14+3*MAXPATHLEN+1];
	const char *s;
	char *d = command;

	for (s = "exec open '"; *s; ) *d++ = *s++;
	// Escape single quote characters.
	for (s = filename; *s; ) {
		if ((*d++ = *s++) == '\'') {
			*d++ = '\\', *d++ = '\'', *d++ = '\'';
		}
	}
	for (s = "'&"; *d++ = *s++; ) ;
	system(command);
}

-open:sender
{
	openInWorkspace([package directory]);
	return self;
}

@end

/*======================================================================
 * PackageInspector.m,v
 * Revision 1.8  1995/09/01 21:46:27  tom
 * Circumvent open deadlock/timeout (when Installer.app is not yet launched);
 * (openInWorkspace): new private function; (-open:): new method.
 *
 * Revision 1.7  1995/07/30 22:20:26  tom
 * (LOCALIZE_ARCH): new macro; (-addArchs:): new method;
 * (-subprocess:output:,-subprocessDone:) use it.
 *
 * Revision 1.6  1995/07/30 16:59:51  tom
 * import Subprocess.h; (archProcess): new ivar;
 * (-getArchs,-subprocess:output:,-subprocessDone:): new methods;
 * added for asynchronous arch-determination.
 *
 * Revision 1.5  1995/07/29 19:13:35  tom
 * (+new): avoid reassignment of self;
 *  make packageDescriptionText vertically resizable;
 * (-shouldLoad): rewritten to generalized array-driven approach.
 *
 * Revision 1.4  1995/04/02 02:39:01  tom
 * (package): NXBundle instead of (const char *). so that localized info files
 *  are found. (this loses out if *.pkg is a symbolic link, though.)
 *
 * Revision 1.3  1994/12/07 00:00:36  tom
 * (RCSid): add spaces.
 *
 * Revision 1.2  1994/11/25 21:27:18  tom
 * (package ivar): use (char*) instead of (NXBundle*) to workaround symlink problems
 *
 * Revision 1.1  1994/11/25 16:13:12  tom
 * Initial revision
 *
 *======================================================================*/
