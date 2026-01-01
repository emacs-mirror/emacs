/* Haiku window system support
   Copyright (C) 2021-2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#include <cstdio>
#include <cstring>
#include <cstdlib>

#include <SupportDefs.h>
#include <Path.h>
#include <AppFileInfo.h>
#include <TranslationUtils.h>
#include <Application.h>
#include <Catalog.h>
#include <Roster.h>
#include <Bitmap.h>
#include <Rect.h>
#include <View.h>

using namespace std;

static void
be_perror (status_t code, char *arg)
{
  if (code != B_OK)
    {
      switch (code)
	{
	case B_BAD_VALUE:
	  fprintf (stderr, "%s: Bad value\n", arg);
	  break;
	case B_ENTRY_NOT_FOUND:
	  fprintf (stderr, "%s: Not found\n", arg);
	  break;
	case B_PERMISSION_DENIED:
	  fprintf (stderr, "%s: Permission denied\n", arg);
	  break;
	case B_NO_MEMORY:
	  fprintf (stderr, "%s: No memory\n", arg);
	  break;
	case B_LINK_LIMIT:
	  fprintf (stderr, "%s: Link limit reached\n", arg);
	  break;
	case B_BUSY:
	  fprintf (stderr, "%s: Busy\n", arg);
	  break;
	case B_NO_MORE_FDS:
	  fprintf (stderr, "%s: No more file descriptors\n", arg);
	  break;
	case B_FILE_ERROR:
	  fprintf (stderr, "%s: File error\n", arg);
	  break;
	default:
	  fprintf (stderr, "%s: Unknown error\n", arg);
	}
    }
  else
    abort ();

  fprintf (stderr, "Setting resources failed on the `src/Emacs' binary.\n"
	   "This may result in the installed `Emacs' binary not launching\n"
	   " from the tracker, but is inconsequential during packaging.\n");
}

int
main (int argc, char **argv)
{
  BApplication app ("application/x-vnd.GNU-emacs-resource-helper");
  BFile file;
  BBitmap *icon;
  BBitmap scale32 (BRect (0, 0, 31, 31), B_RGBA32, true);
  BBitmap scale16 (BRect (0, 0, 15, 15), B_RGBA32, true);
  BAppFileInfo info;
  status_t code;
  struct version_info vinfo;
  char *v = strdup (PACKAGE_VERSION);

  if (scale32.InitCheck () != B_OK
      || scale16.InitCheck () != B_OK)
    {
      fprintf (stderr, "Bitmap initialization ran out of memory\n");
      return EXIT_FAILURE;
    }

  BView scale32view (scale32.Bounds (), NULL,
		     B_FOLLOW_NONE, B_WILL_DRAW);
  BView scale16view (scale16.Bounds (), NULL,
		     B_FOLLOW_NONE, B_WILL_DRAW);

  if (argc != 3)
    {
      printf ("be-resources ICON FILE: make FILE appropriate for Emacs.\n");
      return EXIT_FAILURE;
    }

  code = file.SetTo (argv[2], B_READ_WRITE);
  if (code != B_OK)
    {
      be_perror (code, argv[2]);
      return 0;
    }
  code = info.SetTo (&file);
  if (code != B_OK)
    {
      be_perror (code, argv[2]);
      return 0;
    }
  code = info.SetAppFlags (B_EXCLUSIVE_LAUNCH | B_ARGV_ONLY);
  if (code != B_OK)
    {
      be_perror (code, argv[2]);
      return 0;
    }

  icon = BTranslationUtils::GetBitmapFile (argv[1], NULL);

  if (!icon)
    {
      be_perror (B_ERROR, argv[1]);
      return EXIT_FAILURE;
    }

  scale32.AddChild (&scale32view);
  scale16.AddChild (&scale16view);

  if (!scale32view.LockLooper ()
      || !scale16view.LockLooper ())
    {
      fprintf (stderr, "Failed to lock bitmap looper\n");
      return EXIT_FAILURE;
    }

  scale32view.DrawBitmapAsync (icon, scale32.Bounds ());
  scale16view.DrawBitmapAsync (icon, scale16.Bounds ());

  scale32view.Sync ();
  scale16view.Sync ();

  info.SetIcon (&scale16, B_MINI_ICON);
  info.SetIcon (&scale32, B_LARGE_ICON);
  info.SetSignature ("application/x-vnd.GNU-emacs");

  v = strtok (v, ".");
  vinfo.major = atoi (v);

  v = strtok (NULL, ".");
  vinfo.middle = atoi (v);

  v = strtok (NULL, ".");
  vinfo.minor = v ? atoi (v) : 0;

  vinfo.variety = 0;
  vinfo.internal = 0;

  strncpy ((char *) &vinfo.short_info, PACKAGE_VERSION,
	   sizeof vinfo.short_info - 1);
  strncpy ((char *) &vinfo.long_info, PACKAGE_STRING,
	   sizeof vinfo.long_info - 1);

  info.SetVersionInfo (&vinfo, B_APP_VERSION_KIND);

  exit (EXIT_SUCCESS);
}
