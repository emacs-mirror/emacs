/* ======================================================================= */
/*                                 SCREEN.H                                */
/* ======================================================================= */

// This stuff is entirely non-portable MSDOS-ish code. Note the hardware
// address below, for the standard location of the EGA video buffer.

#if !defined(__COLORS)
#define __COLORS

enum COLORS {
    BLACK,          /* dark colors */
    BLUE,
    GREEN,
    CYAN,
    RED,
    MAGENTA,
    BROWN,
    LIGHTGRAY,
    DARKGRAY,       /* light colors */
    LIGHTBLUE,
    LIGHTGREEN,
    LIGHTCYAN,
    LIGHTRED,
    LIGHTMAGENTA,
    YELLOW,
    WHITE
};
#endif

#define SCREEN_FP(x,y) \
    ((char far *) (0xB8000000L | ((unsigned) (160 * (y) + 2 * (x)))))
#define SCREEN_START   SCREEN_FP(0, 0)

void goto_xy(unsigned char x, unsigned char y);
void hide_cursor(void);
void cursor_position(void);
void clear_screen(void);
void write_xyc(int x, int y, char c);
