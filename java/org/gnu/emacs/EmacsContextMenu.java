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

import android.content.Context;
import android.content.Intent;

import android.os.Build;

import android.view.ContextMenu;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.SubMenu;

import android.util.Log;

/* Context menu implementation.  This object is built from JNI and
   describes a menu hierarchy.  Then, `inflate' can turn it into an
   Android menu, which can be turned into a popup (or other kind of)
   menu.  */

public final class EmacsContextMenu
{
  private static final String TAG = "EmacsContextMenu";

  /* Whether or not an item was selected.  */
  public static boolean itemAlreadySelected;

  /* Whether or not a submenu was selected.
     Value is -1 if no; value is -2 if yes, and a context menu
     close event will definitely be sent.  Any other value is
     the timestamp when the submenu was selected.  */
  public static long wasSubmenuSelected;

  /* The serial ID of the last context menu to be displayed.  */
  public static int lastMenuEventSerial;

  /* The last group ID used for a menu item.  */
  public int lastGroupId;

  private static final class Item implements MenuItem.OnMenuItemClickListener
  {
    public int itemID;
    public String itemName, tooltip;
    public EmacsContextMenu subMenu;
    public boolean isEnabled, isCheckable, isChecked;
    public EmacsView inflatedView;
    public boolean isRadio;

    @Override
    public boolean
    onMenuItemClick (MenuItem item)
    {
      if (subMenu != null)
	{
	  /* Android 6.0 and earlier don't support nested submenus
	     properly, so display the submenu popup by hand.  */

	  if (Build.VERSION.SDK_INT < Build.VERSION_CODES.N)
	    {
	      /* Still set wasSubmenuSelected -- if not set, the
		 dismissal of this context menu will result in a
		 context menu event being sent.  */
	      wasSubmenuSelected = -2;

	      /* Running a popup menu from inside a click handler
		 doesn't work, so make sure it is displayed
		 outside.  */

	      inflatedView.post (new Runnable () {
		  @Override
		  public void
		  run ()
		  {
		    inflatedView.popupMenu (subMenu, 0, 0, true);
		  }
		});

	      return true;
	    }

	  /* After opening a submenu within a submenu, Android will
	     send onContextMenuClosed for a ContextMenuBuilder.  This
	     will normally confuse Emacs into thinking that the
	     context menu has been dismissed.  Wrong!

	     Setting this flag prompts EmacsActivity to only handle
	     SubMenuBuilders being closed, which always means the menu
	     has actually been dismissed.

	     However, these extraneous events aren't sent on devices
	     where submenus display without dismissing their parents.
	     Thus, only ignore the close event if it happens within
	     300 milliseconds of the submenu being selected.  */
	  wasSubmenuSelected = System.currentTimeMillis ();
	  return false;
	}

      /* Send a context menu event.  */
      EmacsNative.sendContextMenu (0, itemID, lastMenuEventSerial);

      /* Say that an item has already been selected.  */
      itemAlreadySelected = true;
      return true;
    }
  };

  /* List of menu items contained in this menu.  */
  public List<Item> menuItems;

  /* The parent context menu, or NULL if none.  */
  private EmacsContextMenu parent;

  /* The title of this context menu, or NULL if none.  */
  private String title;



  /* Create a context menu with no items inside and the title TITLE,
     which may be NULL.  */

  public static EmacsContextMenu
  createContextMenu (String title)
  {
    EmacsContextMenu menu;

    menu = new EmacsContextMenu ();
    menu.title = title;
    menu.menuItems = new ArrayList<Item> ();

    return menu;
  }

  /* Add a normal menu item to the context menu with the id ITEMID and
     the name ITEMNAME.  Enable it if ISENABLED, else keep it
     disabled.

     If this is not a submenu and ISCHECKABLE is set, make the item
     checkable.  Likewise, if ISCHECKED is set, make the item
     checked.

     If TOOLTIP is non-NULL, set the menu item tooltip to TOOLTIP.

     If ISRADIO, then display the check mark as a radio button.  */

  public void
  addItem (int itemID, String itemName, boolean isEnabled,
	   boolean isCheckable, boolean isChecked,
	   String tooltip, boolean isRadio)
  {
    Item item;

    item = new Item ();
    item.itemID = itemID;
    item.itemName = itemName;
    item.isEnabled = isEnabled;
    item.isCheckable = isCheckable;
    item.isChecked = isChecked;
    item.tooltip = tooltip;
    item.isRadio = isRadio;

    menuItems.add (item);
  }

  /* Create a disabled menu item with the name ITEMNAME.  */

  public void
  addPane (String itemName)
  {
    Item item;

    item = new Item ();
    item.itemName = itemName;

    menuItems.add (item);
  }

  /* Add a submenu to the context menu with the specified title and
     item name.  */

  public EmacsContextMenu
  addSubmenu (String itemName, String tooltip)
  {
    EmacsContextMenu submenu;
    Item item;

    item = new Item ();
    item.itemID = 0;
    item.itemName = itemName;
    item.tooltip = tooltip;
    item.subMenu = createContextMenu (itemName);
    item.subMenu.parent = this;

    menuItems.add (item);
    return item.subMenu;
  }

  /* Add the contents of this menu to MENU.  Assume MENU will be
     displayed in INFLATEDVIEW.  */

  private void
  inflateMenuItems (Menu menu, EmacsView inflatedView)
  {
    Intent intent;
    MenuItem menuItem;
    SubMenu submenu;

    for (Item item : menuItems)
      {
	if (item.subMenu != null)
	  {
	    /* This is a submenu.  On versions of Android which
	       support doing so, create the submenu and add the
	       contents of the menu to it.

	       Note that Android 4.0 and later technically supports
	       having multiple layers of nested submenus, but if they
	       are used, onContextMenuClosed becomes unreliable.  */

	    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N)
	      {
		submenu = menu.addSubMenu (item.itemName);
		item.subMenu.inflateMenuItems (submenu, inflatedView);

		/* This is still needed to set wasSubmenuSelected.  */
		menuItem = submenu.getItem ();
	      }
	    else
	      menuItem = menu.add (item.itemName);

	    item.inflatedView = inflatedView;
	    menuItem.setOnMenuItemClickListener (item);
	  }
	else
	  {
	    if (item.isRadio)
	      menuItem = menu.add (++lastGroupId, Menu.NONE, Menu.NONE,
				   item.itemName);
	    else
	      menuItem = menu.add (item.itemName);
	    menuItem.setOnMenuItemClickListener (item);

	    /* If the item ID is zero, then disable the item.  */
	    if (item.itemID == 0 || !item.isEnabled)
	      menuItem.setEnabled (false);

	    /* Now make the menu item display a checkmark as
	       appropriate.  */

	    if (item.isCheckable)
	      menuItem.setCheckable (true);

	    if (item.isChecked)
	      menuItem.setChecked (true);

	    /* Define an exclusively checkable group if the item is a
	       radio button.  */

	    if (item.isRadio)
	      menu.setGroupCheckable (lastGroupId, true, true);

	    /* If the tooltip text is set and the system is new enough
	       to support menu item tooltips, set it on the item.  */

	    if (item.tooltip != null
		&& Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
	      menuItem.setTooltipText (item.tooltip);
	  }
      }
  }

  /* Enter the items in this context menu to MENU.
     Assume that MENU will be displayed in VIEW; this may lead to
     popupMenu being called on VIEW if a submenu is selected.

     If MENU is a ContextMenu, set its header title to the one
     contained in this object.  */

  public void
  expandTo (Menu menu, EmacsView view)
  {
    inflateMenuItems (menu, view);

    /* See if menu is a ContextMenu and a title is set.  */
    if (title == null || !(menu instanceof ContextMenu))
      return;

    /* Set its title to this.title.  */
    ((ContextMenu) menu).setHeaderTitle (title);
  }

  /* Return the parent or NULL.  */

  public EmacsContextMenu
  parent ()
  {
    return this.parent;
  }

  /* Like display, but does the actual work and runs in the main
     thread.  */

  private boolean
  display1 (EmacsWindow window, int xPosition, int yPosition)
  {
    /* Set this flag to false.  It is used to decide whether or not to
       send 0 in response to the context menu being closed.  */
    itemAlreadySelected = false;

    /* No submenu has been selected yet.  */
    wasSubmenuSelected = -1;

    return window.view.popupMenu (this, xPosition, yPosition,
				  false);
  }

  /* Display this context menu on WINDOW, at xPosition and yPosition.
     SERIAL is a number that will be returned in any menu event
     generated to identify this context menu.  */

  public boolean
  display (final EmacsWindow window, final int xPosition,
	   final int yPosition, final int serial)
  {
    FutureTask<Boolean> task;

    /* Android will permanently cease to display any popup menus at
       all if the list of menu items is empty.  Prevent this by
       promptly returning if there are no menu items.  */

    if (menuItems.isEmpty ())
      return false;

    task = new FutureTask<Boolean> (new Callable<Boolean> () {
	@Override
	public Boolean
	call ()
	{
	  boolean rc;

	  lastMenuEventSerial = serial;
	  rc = display1 (window, xPosition, yPosition);

	  /* Android 3.0 to Android 7.0 perform duplicate calls to
	     onContextMenuClosed the second time a context menu is
	     dismissed.  Since the second call after such a dismissal is
	     otherwise liable to prematurely cancel any context menu
	     displayed immediately afterwards, ignore calls received
	     within 150 milliseconds of this menu's being displayed.  */

	  if (rc && Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB
	      && Build.VERSION.SDK_INT < Build.VERSION_CODES.N)
	    wasSubmenuSelected = System.currentTimeMillis () - 150;

	  return rc;
	}
      });

    return EmacsService.<Boolean>syncRunnable (task);
  }

  /* Dismiss this context menu.  WINDOW is the window where the
     context menu is being displayed.  */

  public void
  dismiss (final EmacsWindow window)
  {
    Runnable runnable;

    EmacsService.SERVICE.runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  window.view.cancelPopupMenu ();
	  itemAlreadySelected = false;
	}
      });
  }
};
