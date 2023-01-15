/* Communication module for Android terminals.  -*- c-file-style: "GNU" -*-

Copyright (C) 2023 Free Software Foundation, Inc.

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

import android.content.Context;
import android.content.Intent;

import android.os.Bundle;

import android.view.Menu;
import android.view.MenuItem;
import android.view.View;

import android.util.Log;

import android.widget.PopupMenu;

/* Context menu implementation.  This object is built from JNI and
   describes a menu hiearchy.  Then, `inflate' can turn it into an
   Android menu, which can be turned into a popup (or other kind of)
   menu.  */

public class EmacsContextMenu
{
  private static final String TAG = "EmacsContextMenu";

  /* Whether or not an item was selected.  */
  public static boolean itemAlreadySelected;

  private class Item implements MenuItem.OnMenuItemClickListener
  {
    public int itemID;
    public String itemName;
    public EmacsContextMenu subMenu;
    public boolean isEnabled;

    @Override
    public boolean
    onMenuItemClick (MenuItem item)
    {
      Log.d (TAG, "onMenuItemClick: " + itemName + " (" + itemID + ")");

      /* Send a context menu event.  */
      EmacsNative.sendContextMenu ((short) 0, itemID);

      /* Say that an item has already been selected.  */
      itemAlreadySelected = true;
      return true;
    }
  };

  public List<Item> menuItems;
  public String title;
  private EmacsContextMenu parent;

  /* Create a context menu with no items inside and the title TITLE,
     which may be NULL.  */

  public static EmacsContextMenu
  createContextMenu (String title)
  {
    EmacsContextMenu menu;

    menu = new EmacsContextMenu ();
    menu.menuItems = new ArrayList<Item> ();
    menu.title = title;

    return menu;
  }

  /* Add a normal menu item to the context menu with the id ITEMID and
     the name ITEMNAME.  Enable it if ISENABLED, else keep it
     disabled.  */

  public void
  addItem (int itemID, String itemName, boolean isEnabled)
  {
    Item item;

    item = new Item ();
    item.itemID = itemID;
    item.itemName = itemName;
    item.isEnabled = isEnabled;

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
  addSubmenu (String itemName, String title)
  {
    EmacsContextMenu submenu;
    Item item;

    item = new Item ();
    item.itemID = 0;
    item.itemName = itemName;
    item.subMenu = createContextMenu (title);
    item.subMenu.parent = this;

    menuItems.add (item);
    return item.subMenu;
  }

  /* Add the contents of this menu to MENU.  */

  private void
  inflateMenuItems (Menu menu)
  {
    Intent intent;
    MenuItem menuItem;
    Menu submenu;

    for (Item item : menuItems)
      {
	if (item.subMenu != null)
	  {
	    /* This is a submenu.  Create the submenu and add the
	       contents of the menu to it.  */
	    submenu = menu.addSubMenu (item.itemName);
	    inflateMenuItems (submenu);
	  }
	else
	  {
	    menuItem = menu.add (item.itemName);
	    menuItem.setOnMenuItemClickListener (item);

	    /* If the item ID is zero, then disable the item.  */
	    if (item.itemID == 0 || !item.isEnabled)
	      menuItem.setEnabled (false);
	  }
      }
  }

  /* Enter the items in this context menu to MENU.  Create each menu
     item with an Intent containing a Bundle, where the key
     "emacs:menu_item_hi" maps to the high 16 bits of the
     corresponding item ID, and the key "emacs:menu_item_low" maps to
     the low 16 bits of the item ID.  */

  public void
  expandTo (Menu menu)
  {
    inflateMenuItems (menu);
  }

  /* Return the parent or NULL.  */

  public EmacsContextMenu
  parent ()
  {
    return parent;
  }

  /* Like display, but does the actual work and runs in the main
     thread.  */

  private boolean
  display1 (EmacsWindow window, int xPosition, int yPosition)
  {
    /* Set this flag to false.  It is used to decide whether or not to
       send 0 in response to the context menu being closed.  */
    itemAlreadySelected = false;

    return window.view.popupMenu (this, xPosition, yPosition);
  }

  /* Display this context menu on WINDOW, at xPosition and
     yPosition.  */

  public boolean
  display (final EmacsWindow window, final int xPosition,
	   final int yPosition)
  {
    Runnable runnable;
    final Holder<Boolean> rc;

    rc = new Holder<Boolean> ();

    runnable = new Runnable () {
	@Override
	public void
	run ()
	{
	  synchronized (this)
	    {
	      rc.thing = display1 (window, xPosition, yPosition);
	      notify ();
	    }
	}
      };

    synchronized (runnable)
      {
	EmacsService.SERVICE.runOnUiThread (runnable);

	try
	  {
	    runnable.wait ();
	  }
	catch (InterruptedException e)
	  {
	    EmacsNative.emacsAbort ();
	  }
      }

    return rc.thing;
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
