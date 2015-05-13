/* ======================================================================= */
/*                                SCREEN.CPP                               */
/* ======================================================================= */

#include "stdio.h"
#include "stdlib.h"
#include "dos.h"

#include "screen.h"

/* ----------------------------------------------------------------------- */
/*              Cursor Position and Screen Buffering Functions             */
/* ----------------------------------------------------------------------- */

unsigned char cursor_x, cursor_y;
static union REGS regs;

void goto_xy(unsigned char x, unsigned char y)
    {
    regs.h.ah = 2;
    regs.h.bh = 0;
    regs.h.dh = y;
    regs.h.dl = x;
    int86(0x10, &regs, &regs);
    }

void hide_cursor(void)
    {
    goto_xy(0, NUM_ROWS);
    }

void cursor_position(void)
    {
    regs.h.ah = 3;
    regs.h.bh = 0;
    int86(0x10, &regs, &regs);
    cursor_x = regs.h.dl;
    cursor_y = regs.h.dh;
    }

void clear_screen(void)
    {
    unsigned int i, j;
    char far *p;

    p = SCREEN_START;
    for (i = 0; i < NUM_ROWS; i++)
        for (j = 0; j < 80; j++)
            {
            *p++ = ' ';
            *p++ = LIGHTGRAY;
            }
    }

void write_xyc(int x, int y, char c)
    {
    char far *p;

    p = SCREEN_FP(x, y);
    *p++ = c;
    *p = LIGHTGRAY;
    }
