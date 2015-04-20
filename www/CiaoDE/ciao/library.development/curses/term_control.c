/* 
 * term_control.c -- Terminal control from prolog.
 * AFSID           : $__Header$
 * Author          : Manuel Carro Li~nares
 * Created On      : Mon Jul 11 17:34:08 1994
 * Last Modified By: Manuel Carro Li~nares
 * Last Modified On: Tue Oct 18 13:23:05 1994
 * Update Count    : 26
 * Status          : Unknown, Use with caution!
 */

/* compile with cc -c term_control.c -lcurses */

#include <curses.h>

static WINDOW * prolog_window;

void init_term()
{
  prolog_window = initscr();
}

void close_term()
{
  endwin();
}

void cls()
{
  erase();
  refresh();
}

void twrite_prolog(string)
     char * string;
{
  printw("%s", string);
  refresh();
}

void tmove(h, v)
     long h, v;
{
  move((int)h, (int)v);
}

void tget(h, v)
     long *h, *v;
{
  getyx(prolog_window, (int)*h, (int)*v);
}


/* main()
 * {
 *   long x, y;
 * 
 *   init_term();
 * 
 *   cls();
 * 
 *   for(x = 1; x < 24; x++){
 *     tmove(x, x);
 *     twrite("x");
 *     twrite("afrd");
 *   }
 * 
 *   close_term();
 * }
 */
