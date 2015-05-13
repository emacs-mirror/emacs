/*
 * @(#)SystemColor.java	1.5 97/01/27
 * 
 * Copyright (c) 1995, 1996 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the confidential and proprietary information of Sun
 * Microsystems, Inc. ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Sun.
 * 
 * SUN MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. SUN SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 * 
 * CopyrightVersion 1.1_beta
 * 
 */
package java.awt;

/**
 * A class to encapsulate symbolic colors representing the color
 * of GUI objects on a system.  For systems which support the dynamic
 * update of the system colors (when the user changes the colors)
 * the actual RGB values of these symbolic colors will also change
 * dynamically.  In order to compare the "current" RGB value of a SystemColor
 * object with a non-symbolic Color object, getRGB() should be used
 * rather than equals(). 
 *
 * @version 	1.5, 27 Jan 1997
 * @author 	Carl Quinn
 * @author      Amy Fowler
 */
public final class SystemColor extends Color implements java.io.Serializable {

    /**
     * The array index for the desktop background color.
     */
    public final static int DESKTOP = 0;

    /**
     * The array index for the active caption background color.
     */
    public final static int ACTIVE_CAPTION = 1;

    /**
     * The array index for the action caption text color.
     */
    public final static int ACTIVE_CAPTION_TEXT = 2;

    /**
     * The array index for the active caption border color.
     */
    public final static int ACTIVE_CAPTION_BORDER = 3;

    /**
     * The array index for the inactive caption background color.
     */
    public final static int INACTIVE_CAPTION = 4;

    /**
     * The array index for the inactive caption text color.
     */
    public final static int INACTIVE_CAPTION_TEXT = 5;

    /**
     * The array index for the inactive caption border color.
     */
    public final static int INACTIVE_CAPTION_BORDER = 6;

    /**
     * The array index for the window background color.
     */
    public final static int WINDOW = 7;

    /**
     * The array index for the window border color.
     */
    public final static int WINDOW_BORDER = 8;

    /**
     * The array index for the window text color.
     */
    public final static int WINDOW_TEXT = 9;

    /**
     * The array index for the menu background color.
     */
    public final static int MENU = 10;

    /**
     * The array index for the menu text color.
     */
    public final static int MENU_TEXT = 11;

    /**
     * The array index for the text background color.
     */
    public final static int TEXT = 12;

    /**
     * The array index for the text text color.
     */
    public final static int TEXT_TEXT = 13;

    /**
     * The array index for the text highlight color.
     */
    public final static int TEXT_HIGHLIGHT = 14;

    /**
     * The array index for the text highlight text color.
     */
    public final static int TEXT_HIGHLIGHT_TEXT = 15;

    /**
     * The array index for the text inactive text color.
     */
    public final static int TEXT_INACTIVE_TEXT = 16;

    /**
     * The array index for the control background color.
     */
    public final static int CONTROL = 17;

    /**
     * The array index for the control text color.
     */
    public final static int CONTROL_TEXT = 18;

    /**
     * The array index for the control highlight color.
     */
    public final static int CONTROL_HIGHLIGHT = 19;

    /**
     * The array index for the control light highlight color.
     */
    public final static int CONTROL_LT_HIGHLIGHT = 20;

    /**
     * The array index for the control shadow color.
     */
    public final static int CONTROL_SHADOW = 21;

    /**
     * The array index for the control dark shadow color.
     */
    public final static int CONTROL_DK_SHADOW = 22;

    /**
     * The array index for the scrollbar background color.
     */
    public final static int SCROLLBAR = 23;

    /**
     * The array index for the info background color.
     */
    public final static int INFO = 24;

    /**
     * The array index for the info text color.
     */
    public final static int INFO_TEXT = 25;

    /**
     * The number of system colors in the array.
     */
    public final static int NUM_COLORS = 26;
    
   /**
     * The color of the desktop background.
     */
    public final static SystemColor desktop = new SystemColor((byte)DESKTOP);

    /**
     * The background color for captions in window borders.
     */
    public final static SystemColor activeCaption = new SystemColor((byte)ACTIVE_CAPTION);

    /**
     * The text color for captions in window borders.
     */
    public final static SystemColor activeCaptionText = new SystemColor((byte)ACTIVE_CAPTION_TEXT);

    /**
     * The border color for captions in window borders.
     */
    public final static SystemColor activeCaptionBorder = new SystemColor((byte)ACTIVE_CAPTION_BORDER);

    /**
     * The background color for inactive captions in window borders.
     */
    public final static SystemColor inactiveCaption = new SystemColor((byte)INACTIVE_CAPTION);

    /**
     * The text color for inactive captions in window borders.
     */
    public final static SystemColor inactiveCaptionText = new SystemColor((byte)INACTIVE_CAPTION_TEXT);

    /**
     * The border color for inactive captios in window borders.
     */
    public final static SystemColor inactiveCaptionBorder = new SystemColor((byte)INACTIVE_CAPTION_BORDER);

    /**
     * The background color for windows.
     */
    public final static SystemColor window = new SystemColor((byte)WINDOW);

    /**
     * The border color for windows.
     */
    public final static SystemColor windowBorder = new SystemColor((byte)WINDOW_BORDER);

    /**
     * The text color for windows.
     */
    public final static SystemColor windowText = new SystemColor((byte)WINDOW_TEXT);

    /**
     * The background color for menus.
     */
    public final static SystemColor menu = new SystemColor((byte)MENU);

    /**
     * The text color for menus.
     */
    public final static SystemColor menuText = new SystemColor((byte)MENU_TEXT);

    /**
     * The background color for text components.
     */
    public final static SystemColor text = new SystemColor((byte)TEXT);

    /**
     * The text color for text components.
     */
    public final static SystemColor textText = new SystemColor((byte)TEXT_TEXT);

    /**
     * The background color for highlighted text.
     */
    public final static SystemColor textHighlight = new SystemColor((byte)TEXT_HIGHLIGHT);

    /**
     * The text color for highlighted text.
     */
    public final static SystemColor textHighlightText = new SystemColor((byte)TEXT_HIGHLIGHT_TEXT);

    /**
     * The text color for inactive text.
     */
    public final static SystemColor textInactiveText = new SystemColor((byte)TEXT_INACTIVE_TEXT);

    /**
     * The background color for control objects.
     */
    public final static SystemColor control = new SystemColor((byte)CONTROL);

    /**
     * The text color for control objects.
     */
    public final static SystemColor controlText = new SystemColor((byte)CONTROL_TEXT);

    /**
     * The light highlight color for control objects.
     */
    public final static SystemColor controlHighlight = new SystemColor((byte)CONTROL_HIGHLIGHT);

    /**
     * The regular highlight color for control objects.
     */
    public final static SystemColor controlLtHighlight = new SystemColor((byte)CONTROL_LT_HIGHLIGHT);

    /**
     * The regular shadow color for control objects.
     */
    public final static SystemColor controlShadow = new SystemColor((byte)CONTROL_SHADOW);

    /**
     * The dark shadow color for control objects.
     */
    public final static SystemColor controlDkShadow = new SystemColor((byte)CONTROL_DK_SHADOW);

    /**
     * The background color for scrollbars.
     */
    public final static SystemColor scrollbar = new SystemColor((byte)SCROLLBAR);

    /**
     * The background color for info(help) text.
     */
    public final static SystemColor info = new SystemColor((byte)INFO);

    /**
     * The text color for info(help) text.
     */
    public final static SystemColor infoText = new SystemColor((byte)INFO_TEXT);

    /*
     * System colors with default initial values, overwritten by toolkit if 
     * system values differ and are available.
     */
    private static int[] systemColors = {
        0xFF005C5C,  // desktop = new Color(0,92,92);
        0xFF000080,  // activeCaption = new Color(0,0,128);
        0xFFFFFFFF,  // activeCaptionText = Color.white;
        0xFFC0C0C0,  // activeCaptionBorder = Color.lightGray;
        0xFF808080,  // inactiveCaption = Color.gray;
        0xFFC0C0C0,  // inactiveCaptionText = Color.lightGray;
        0xFFC0C0C0,  // inactiveCaptionBorder = Color.lightGray;
        0xFFFFFFFF,  // window = Color.white;
        0xFF000000,  // windowBorder = Color.black;
        0xFF000000,  // windowText = Color.black;
        0xFFC0C0C0,  // menu = Color.lightGray;
        0xFF000000,  // menuText = Color.black;
        0xFFC0C0C0,  // text = Color.lightGray;
        0xFF000000,  // textText = Color.black;
        0xFF000080,  // textHighlight = new Color(0,0,128);
        0xFFFFFFFF,  // textHighlightText = Color.white;
        0xFF808080,  // textInactiveText = Color.gray;
        0xFFC0C0C0,  // control = Color.lightGray;
        0xFF000000,  // controlText = Color.black;
        0xFFFFFFFF,  // controlHighlight = Color.white;
        0xFFE0E0E0,  // controlLtHighlight = new Color(224,224,224);
        0xFF808080,  // controlShadow = Color.gray;
        0xFF000000,  // controlDkShadow = Color.black;
        0xFFE0E0E0,  // scrollbar = new Color(224,224,224);
        0xFFE0E000,  // info = new Color(224,224,0);
        0xFF000000,  // infoText = Color.black;
    };

    /*
     * JDK 1.1 serialVersionUID 
     */
    private static final long serialVersionUID = 4503142729533789064L;

    static {
      updateSystemColors();
    }

    /**
     * called from <init> & toolkit to update the above systemColors cache
     */
    private static void updateSystemColors() {
      Toolkit.getDefaultToolkit().loadSystemColors(systemColors);
    }

    /**
     * Create a symbolic color that represents an indexed entry into system
     * color cache. Used by above static system colors.
     */
    private SystemColor(byte index) {
        super(0, 0, 0);
	value = index;
    }

    /**
     * Gets the "current" RGB value representing the symbolic color.
     * (Bits 24-31 are 0xff, 16-23 are red, 8-15 are green, 0-7 are blue).
     * @see java.awt.image.ColorModel#getRGBdefault
     * @see #getRed
     * @see #getGreen
     * @see #getBlue
     */
    public int getRGB() {
	return systemColors[value];
    }

    /**
     * Returns the String representation of this Color's values.
     */
    public String toString() {
        return getClass().getName() + "[i=" + (value) + "]";
    }

}
