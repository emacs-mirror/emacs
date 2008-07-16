/* mac-fix-env: A small utility to pick up the shell environment on MacOS X
                and insert it into the file ~/.MacOSX/environment.plist
                creating if necessary.
   Copyright (C) 1989, 1993, 2005, 2008 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

/*
 usage:
        Run from command line (in Terminal) once or whenever path changes:

 /Applications/Emacs.app/Contents/MacOS/bin/mac-fix-env

 (change initial part to where you installed Emacs).
*/

#import <Foundation/Foundation.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    NSMutableDictionary *envPlist;
    NSString *file = [[NSHomeDirectory()
                         stringByAppendingPathComponent:@".MacOSX"]
                         stringByAppendingPathComponent:@"environment.plist"];
    NSString *path = [NSString stringWithCString: getenv("PATH")];

    envPlist = [[NSDictionary dictionaryWithContentsOfFile: file] mutableCopy];
    if (envPlist == nil)
      {
        // create
        NSString *dir = [file stringByDeletingLastPathComponent];
        envPlist = [NSMutableDictionary dictionaryWithCapacity: 5];

        if ([[NSFileManager defaultManager] fileExistsAtPath: dir] == NO)
          {
            if ([[NSFileManager defaultManager] createDirectoryAtPath:dir
                                                           attributes:nil]==NO)
              {
                NSLog(@":\nCould not create directory at '%@'; aborting.",dir);
                return 1;
              }
          }
      }

    [envPlist setObject: path forKey: @"PATH"];

    if ([envPlist writeToFile: file atomically: YES] == NO)
      {
        NSLog(@":\nCould not write file at '%@'; aborting.", file);
        return 1;
      }

    NSLog(@":\nWrote file to '%@'.\nPlease inspect it to make sure PATH is correct.", file);
    return 0;
}

// arch-tag: 609d5528-5ac1-42c5-859b-24c14341ee3b
