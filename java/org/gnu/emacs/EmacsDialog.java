/* Communication module for Android terminals.  -*- c-file-style: "GNU" -*-

Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

package org.gnu.emacs;

import java.util.List;
import java.util.ArrayList;

import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;

import android.app.AlertDialog;

import android.content.Context;
import android.content.DialogInterface;

import android.content.res.Resources.NotFoundException;
import android.content.res.Resources.Theme;
import android.content.res.TypedArray;

import android.os.Build;

import android.provider.Settings;

import android.util.Log;

import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.FrameLayout;

import android.view.ContextThemeWrapper;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;

/* Toolkit dialog implementation.  This object is built from JNI and
   describes a single alert dialog.  Then, `inflate' turns it into
   AlertDialog.  */

public final class EmacsDialog implements DialogInterface.OnDismissListener
{
  private static final String TAG = "EmacsDialog";

  /* List of buttons in this dialog.  */
  private List<EmacsButton> buttons;

  /* Dialog title.  */
  private String title;

  /* Dialog text.  */
  private String text;

  /* Whether or not a selection has already been made.  */
  private boolean wasButtonClicked;

  /* Dialog to dismiss after click.  */
  private AlertDialog dismissDialog;

  /* The menu serial associated with this dialog box.  */
  private int menuEventSerial;

  private final class EmacsButton implements View.OnClickListener,
				  DialogInterface.OnClickListener
  {
    /* Name of this button.  */
    public String name;

    /* ID of this button.  */
    public int id;

    /* Whether or not the button is enabled.  */
    public boolean enabled;

    @Override
    public void
    onClick (View view)
    {
      wasButtonClicked = true;
      EmacsNative.sendContextMenu (0, id, menuEventSerial);
      dismissDialog.dismiss ();
    }

    @Override
    public void
    onClick (DialogInterface dialog, int which)
    {
      wasButtonClicked = true;
      EmacsNative.sendContextMenu (0, id, menuEventSerial);
    }
  };

  /* Create a popup dialog with the title TITLE and the text TEXT.
     TITLE may be NULL.  MENUEVENTSERIAL is a number which will
     identify this popup dialog inside events it sends.  */

  public static EmacsDialog
  createDialog (String title, String text, int menuEventSerial)
  {
    EmacsDialog dialog;

    dialog = new EmacsDialog ();
    dialog.buttons = new ArrayList<EmacsButton> ();
    dialog.title = title;
    dialog.text = text;
    dialog.menuEventSerial = menuEventSerial;

    return dialog;
  }

  /* Add a button named NAME, with the identifier ID.  If DISABLE,
     disable the button.  */

  public void
  addButton (String name, int id, boolean disable)
  {
    EmacsButton button;

    button = new EmacsButton ();
    button.name = name;
    button.id = id;
    button.enabled = !disable;
    buttons.add (button);
  }

  /* Turn this dialog into an AlertDialog for the specified
     CONTEXT.

     Upon a button being selected, the dialog will send an
     ANDROID_CONTEXT_MENU event with the id of that button.

     Upon the dialog being dismissed, an ANDROID_CONTEXT_MENU event
     will be sent with an id of 0.  */

  public AlertDialog
  toAlertDialog (Context context)
  {
    AlertDialog dialog;
    int size, styleId, flag;
    int[] attrs;
    EmacsButton button;
    EmacsDialogButtonLayout layout;
    Button buttonView;
    ViewGroup.LayoutParams layoutParams;
    Theme theme;
    TypedArray attributes;
    Window window;

    /* Wrap the context within a style wrapper.  Any dialog properties
       tied to EmacsStyle (such as those applied by the system ``dark
       theme'') will thus affect the dialog irrespective of whether
       CONTEXT is an activity or the service.  */

    context = new ContextThemeWrapper (context, R.style.EmacsStyle);

    size = buttons.size ();
    styleId = -1;

    if (size <= 3)
      {
	dialog = new AlertDialog.Builder (context).create ();
	dialog.setMessage (text);
	dialog.setCancelable (true);
	dialog.setOnDismissListener (this);

	if (title != null)
	  dialog.setTitle (title);

	/* There are less than 4 buttons.  Add the buttons the way
	   Android intends them to be added.  */

	if (size >= 1)
	  {
	    button = buttons.get (0);
	    dialog.setButton (DialogInterface.BUTTON_POSITIVE,
			      button.name, button);
	  }

	if (size >= 2)
	  {
	    button = buttons.get (1);
	    dialog.setButton (DialogInterface.BUTTON_NEGATIVE,
			      button.name, button);
	  }

	if (size >= 3)
	  {
	    button = buttons.get (2);
	    dialog.setButton (DialogInterface.BUTTON_NEUTRAL,
			      button.name, button);
	  }
      }
    else
      {
	/* There are more than 3 buttons.  Add them all to a special
	   container widget that handles wrapping.  First, create the
	   layout.  */

	layout = new EmacsDialogButtonLayout (context);
	layoutParams
	  = new FrameLayout.LayoutParams (ViewGroup.LayoutParams.MATCH_PARENT,
					  ViewGroup.LayoutParams.WRAP_CONTENT);
	layout.setLayoutParams (layoutParams);

	/* Add that layout to the dialog's custom view.

	   android.R.id.custom is documented to work.  But looking it
	   up returns NULL, so setView must be used instead.  */

	dialog = new AlertDialog.Builder (context).setView (layout).create ();
	dialog.setMessage (text);
	dialog.setCancelable (true);
	dialog.setOnDismissListener (this);

	if (title != null)
	  dialog.setTitle (title);

	/* Now that the dialog has been created, set the style of each
	   custom button to match the usual dialog buttons found on
	   Android 5 and later.  */

	if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
	  {
	    /* Obtain the Theme associated with the dialog.  */
	    theme = dialog.getContext ().getTheme ();

	    /* Resolve the dialog button style.  */
	    attrs
	      = new int [] { android.R.attr.buttonBarNeutralButtonStyle, };

	    try
	      {
		attributes = theme.obtainStyledAttributes (attrs);

		/* Look for the style ID.  Default to -1 if it could
		   not be found.  */
		styleId = attributes.getResourceId (0, -1);

		/* Now clean up the TypedAttributes object.  */
		attributes.recycle ();
	      }
	    catch (NotFoundException e)
	      {
		/* Nothing to do here.  */
	      }
	  }

	/* Create each button and add it to the layout.  Set the style
	   if necessary.  */

	for (EmacsButton emacsButton : buttons)
	  {
	    if (styleId == -1)
	      /* No specific style... */
	      buttonView = new Button (context);
	    else
	      /* Use the given styleId.  */
	      buttonView = new Button (context, null, 0, styleId);

	    /* Set the text and on click handler.  */
	    buttonView.setText (emacsButton.name);
	    buttonView.setOnClickListener (emacsButton);
	    buttonView.setEnabled (emacsButton.enabled);
	    layout.addView (buttonView);
	  }
      }

    return dialog;
  }

  /* Internal helper for display run on the main thread.  */

  @SuppressWarnings("deprecation")
  private boolean
  display1 ()
  {
    Context context;
    int size, type;
    Button buttonView;
    EmacsButton button;
    AlertDialog dialog;
    Window window;

    if (EmacsActivity.focusedActivities.isEmpty ())
      {
	/* If focusedActivities is empty then this dialog may have
	   been displayed immediately after another popup dialog was
	   dismissed.  Or Emacs might legitimately be in the
	   background, possibly displaying this popup in response to
	   an Emacsclient request.  Try the service context if it will
	   work, then any focused EmacsOpenActivity, and finally the
	   last EmacsActivity to be focused.  */

	if (Build.VERSION.SDK_INT < Build.VERSION_CODES.M
	    || Settings.canDrawOverlays (EmacsService.SERVICE))
	  context = EmacsService.SERVICE;
	else if (EmacsOpenActivity.currentActivity != null)
	  context = EmacsOpenActivity.currentActivity;
	else
	  context = EmacsActivity.lastFocusedActivity;

	if (context == null)
	  return false;
      }
    else
      /* Display using the activity context when Emacs is in the
	 foreground, as this allows the dialog to be dismissed more
	 consistently.  */
      context = EmacsActivity.focusedActivities.get (0);

    dialog = dismissDialog = toAlertDialog (context);

    try
      {
	if (context == EmacsService.SERVICE)
	  {
	    /* Apply the system alert window type to make sure this
	       dialog can be displayed.  */

	    window = dialog.getWindow ();
	    type = (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O
		    ? WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY
		    : WindowManager.LayoutParams.TYPE_PHONE);
	    window.setType (type);
	  }

	dismissDialog.show ();
      }
    catch (Exception exception)
      {
	/* This can happen when the system decides Emacs is not in the
	   foreground any longer.  */
	return false;
      }

    /* If there are less than four buttons, then they must be
       individually enabled or disabled after the dialog is
       displayed.  */
    size = buttons.size ();

    if (size <= 3)
      {
	if (size >= 1)
	  {
	    button = buttons.get (0);
	    buttonView
	      = dialog.getButton (DialogInterface.BUTTON_POSITIVE);
	    buttonView.setEnabled (button.enabled);
	  }

	if (size >= 2)
	  {
	    button = buttons.get (1);
	    buttonView
	      = dialog.getButton (DialogInterface.BUTTON_NEGATIVE);
	    buttonView.setEnabled (button.enabled);
	  }

	if (size >= 3)
	  {
	    button = buttons.get (2);
	    buttonView
	      = dialog.getButton (DialogInterface.BUTTON_NEUTRAL);
	    buttonView.setEnabled (button.enabled);
	  }
      }

    return true;
  }

  /* Display this dialog for a suitable activity.
     Value is false if the dialog could not be displayed,
     and true otherwise.  */

  public boolean
  display ()
  {
    FutureTask<Boolean> task;

    task = new FutureTask<Boolean> (new Callable<Boolean> () {
	@Override
	public Boolean
	call ()
	{
	  return display1 ();
	}
      });

    return EmacsService.<Boolean>syncRunnable (task);
  }



  @Override
  public void
  onDismiss (DialogInterface dialog)
  {
    if (wasButtonClicked)
      return;

    EmacsNative.sendContextMenu (0, 0, menuEventSerial);
  }
};
