/* ======================================================================= */
/*                                CONWAY.CPP                               */
/* ======================================================================= */

#include "assert.h"
#include "iostream.h"
#include "conio.h"
#include "clheir.h"
#include "screen.h"
#include "conway.h"

#define max(x,y)  ((x > y) ? x : y)
#define min(x,y)  ((x > y) ? y : x)

const int num_rows = min(50, NUM_ROWS);
const int num_columns = 40;

class site *field_of_play[num_rows][num_columns];

int site::total_surrounding(void)
    {
    int i, j, imin, imax, jmin, jmax, total;

    total = 0;
    imin = max(0, x - 1);
    imax = min(num_rows - 1, x + 1);
    jmin = max(0, y - 1);
    jmax = min(num_columns - 1, y + 1);

    for (i = imin; i <= imax; i++)
        for (j = jmin; j <= jmax; j++)
            if (field_of_play[i][j]->read()) total++;
    if (alive) total--;
    return total;
    }

void display(void)
    {
    int i, j;

    for (i = 0; i < num_rows; i++)
        for (j = 0; j < num_columns; j++)
            {
            if (field_of_play[i][j]->read()) write_xyc(2*j, i, 'X');
            else write_xyc(2*j, i, '.');
            }
    hide_cursor();
    }

void glider(int x, int y)
    {
    field_of_play[x - 1][y + 0]->set();
    field_of_play[x - 1][y + 1]->set();
    field_of_play[x + 0][y - 1]->set();
    field_of_play[x + 0][y + 0]->set();
    field_of_play[x + 1][y + 1]->set();
    }

void traffic_light(int x, int y)
    {
    field_of_play[x - 1][y]->set();
    field_of_play[x + 0][y]->set();
    field_of_play[x + 1][y]->set();
    }


void main(void)
    {
    int i, j, c;

    init_registry();

    for (i = 0; i < num_rows; i++)
        for (j = 0; j < num_columns; j++)
            field_of_play[i][j] = new site(i, j);

start_over:
    traffic_light(num_rows/2 - 8, num_columns/2 - 8);
    glider(num_rows/2 + 8, num_columns/2 + 8);

    clear_screen();
    while (1)
        {
        display();
        if ((c = getch()) == 'q') { clear_screen(); return; }
        if (c == 'i')
            {
            for (i = 0; i < num_rows; i++)
                for (j = 0; j < num_columns; j++)
                    field_of_play[i][j]->clear();
            goto start_over;
            }
        step_everybody();
        }
    }
