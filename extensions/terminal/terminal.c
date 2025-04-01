// linux check Dockerfile or Dockerfile.musl
// osx   gcc terminal.c -I/opt/homebrew/include -L/opt/homebrew/lib -lvterm -o terminal.so -shared -fPIC

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <sys/select.h>
#include <vterm.h>
#include <signal.h>

#ifdef __APPLE__
  #include <util.h>
  #include <sys/ioctl.h>
#else
  #include <pty.h>
#endif

///
typedef struct {
  pid_t pid;
  int fd;
} run_shell_result;

static run_shell_result run_shell(int rows, int cols, const char *program, char* const argv[])
{
  int fd;
  struct winsize win = { rows, cols, 0, 0 };
  pid_t pid = forkpty(&fd, NULL, NULL, &win);
  assert(pid >= 0);
  if (pid == 0) {
    setenv("TERM", "xterm-256color", 1);
    assert(execvp(program, argv) >= 0);
  }

  run_shell_result result = { pid, fd };
  return result;
}

///
struct terminal {
  int id;
  VTerm *vterm;
  VTermScreen *screen;
  int fd;
  int pid;
  VTermScreenCell lastCell;
  VTermPos cursorPos;
  int cursorVisible;

  int (*cb_damage)(VTermRect *rect, int id);
  int (*cb_moverect)(VTermRect *dest, VTermRect *src, int id);
  int (*cb_movecursor)(VTermPos *pos, VTermPos *oldpos, int visible, int id);
  int (*cb_settermprop)(VTermProp prop, VTermValue *val, int id);
  int (*cb_bell)(int id);
  int (*cb_resize)(int rows, int cols, int id);
  int (*cb_sb_pushline)(int cols, const VTermScreenCell *cells, int id);
  int (*cb_sb_popline)(int cols, VTermScreenCell *cells, int id);
  int (*cb_sb_clear)(int id); // TODO
};

static int cb_damage(VTermRect rect, void *user)
{
  struct terminal *terminal = ((struct terminal *)user);
  terminal->cb_damage(&rect, terminal->id);
  return 0;
}

static int cb_moverect(VTermRect dest, VTermRect src, void *user)
{
  struct terminal *terminal = ((struct terminal *)user);
  terminal->cb_moverect(&dest, &src, terminal->id);
  return 0;
}

static int cb_movecursor(VTermPos pos, VTermPos oldpos, int visible,
			 void *user)
{
  struct terminal *terminal = ((struct terminal *)user);

  // TODO
  terminal->cursorPos = pos;
  terminal->cursorVisible = visible;

  terminal->cb_movecursor(&pos, &oldpos, visible, terminal->id);
  return 0;
}

static int cb_settermprop(VTermProp prop, VTermValue *val, void *user)
{
  struct terminal *terminal = ((struct terminal *)user);
  terminal->cb_settermprop(prop, val, terminal->id);
  return 0;
}

static int cb_bell(void *user)
{
  struct terminal *terminal = ((struct terminal *)user);
  terminal->cb_bell(terminal->id);
  return 0;
}

static int cb_resize(int rows, int cols, void *user)
{
  struct terminal *terminal = ((struct terminal *)user);
  terminal->cb_resize(rows, cols, terminal->id);
  return 0;
}

static int cb_sb_pushline(int cols, const VTermScreenCell *cells,
			  void *user)
{
  struct terminal *terminal = ((struct terminal *)user);
  terminal->cb_sb_pushline(cols, cells, terminal->id);
  return 0;
}

static int cb_sb_popline(int cols, VTermScreenCell *cells, void *user)
{
  struct terminal *terminal = ((struct terminal *)user);
  terminal->cb_sb_popline(cols, cells, terminal->id);
  return 0;
}

static void output_callback(const char *s, size_t len, void *user)
{
  struct terminal *terminal = (struct terminal *) user;
  write(terminal->fd, s, len);
}

VTermScreenCallbacks screen_callbacks = {
  cb_damage,
  cb_moverect,
  cb_movecursor,
  cb_settermprop,
  cb_bell,
  cb_resize,
  cb_sb_pushline,
  cb_sb_popline,
};

struct terminal *terminal_new(int id,
                              int rows,
                              int cols,
                              const char *program,
                              char* const argv[],
			      void *cb_damage,
			      void *cb_moverect,
			      void *cb_movecursor,
			      void *cb_settermprop,
			      void *cb_bell,
			      void *cb_resize,
			      void *cb_sb_pushline,
                              void *cb_sb_popline)
{
  run_shell_result result = run_shell(rows, cols, program, argv);

  VTerm *vterm = vterm_new(rows, cols);
  vterm_set_utf8(vterm, 1);

  VTermScreen *screen = vterm_obtain_screen(vterm);

  struct terminal *terminal = malloc(sizeof(struct terminal));
  if (terminal == NULL) {
    return NULL;
  }
  terminal->id = id;
  terminal->screen = screen;
  terminal->vterm = vterm;
  terminal->fd = result.fd;
  terminal->pid = result.pid;

  terminal->cb_damage = cb_damage;
  terminal->cb_moverect = cb_moverect;
  terminal->cb_movecursor = cb_movecursor;
  terminal->cb_settermprop = cb_settermprop;
  terminal->cb_bell = cb_bell;
  terminal->cb_resize = cb_resize;
  terminal->cb_sb_pushline = cb_sb_pushline;
  terminal->cb_sb_popline = cb_sb_popline;

  vterm_output_set_callback(vterm, output_callback, (void *) terminal);
  vterm_screen_set_callbacks(screen, &screen_callbacks, terminal);
  vterm_screen_reset(screen, 1);

  return terminal;
}

void terminal_delete(struct terminal *terminal)
{
  vterm_free(terminal->vterm);
  free(terminal);
}

void terminal_input_char(struct terminal *terminal, uint32_t c,
			 VTermModifier mod)
{
  vterm_keyboard_unichar(terminal->vterm, c, mod);
}

void terminal_input_key(struct terminal *terminal, VTermKey key,
			VTermModifier mod)
{
  vterm_keyboard_key(terminal->vterm, key, mod);
}

void terminal_process_input_wait(struct terminal *terminal)
{
  int fd = terminal->fd;
  fd_set readfds;
  FD_ZERO(&readfds);
  FD_SET(fd, &readfds);
  select(fd + 1, &readfds, NULL, NULL, NULL);
}

bool terminal_process_input_nonblock(struct terminal *terminal)
{
  int fd = terminal->fd;
  fd_set readfds;
  FD_ZERO(&readfds);
  FD_SET(fd, &readfds);
  struct timeval timeout = { 0, 0 };
  if (select(fd + 1, &readfds, NULL, NULL, &timeout) > 0) {
    char buf[4096];
    ssize_t size = read(fd, buf, sizeof(buf));
    if (size > 0) {
      vterm_input_write(terminal->vterm, buf, size);
      return true;
    }
  }
  return false;
}

void terminal_process_input(struct terminal *terminal)
{
  int fd = terminal->fd;
  char buf[4096];
  ssize_t size = read(fd, buf, sizeof(buf));
  if (size > 0) {
    vterm_input_write(terminal->vterm, buf, size);
  }
}

VTermScreenCell *terminal_query_cell(struct terminal *terminal, int x,
				     int y)
{
  VTermPos pos = { row: y, col:x };
  VTermScreenCell cell;
  vterm_screen_get_cell(terminal->screen, pos, &cell);
  if (VTERM_COLOR_IS_INDEXED(&cell.fg)) {
    vterm_screen_convert_color_to_rgb(terminal->screen, &cell.fg);
  }
  if (VTERM_COLOR_IS_INDEXED(&cell.bg)) {
    vterm_screen_convert_color_to_rgb(terminal->screen, &cell.bg);
  }
  terminal->lastCell = cell;
  return &terminal->lastCell;
}

int *terminal_last_cell_chars(struct terminal *terminal)
{
  return terminal->lastCell.chars;
}

int terminal_last_cell_width(struct terminal *terminal)
{
  return terminal->lastCell.width;
}

unsigned int terminal_last_cell_attrs_bold(struct terminal *terminal)
{
  return terminal->lastCell.attrs.bold;
}

unsigned int terminal_last_cell_attrs_underline(struct terminal *terminal)
{
  return terminal->lastCell.attrs.underline;
}

unsigned int terminal_last_cell_attrs_italic(struct terminal *terminal)
{
  return terminal->lastCell.attrs.italic;
}

unsigned int terminal_last_cell_attrs_blink(struct terminal *terminal)
{
  return terminal->lastCell.attrs.blink;
}

unsigned int terminal_last_cell_attrs_reverse(struct terminal *terminal)
{
  return terminal->lastCell.attrs.reverse;
}

unsigned int terminal_last_cell_attrs_conceal(struct terminal *terminal)
{
  return terminal->lastCell.attrs.conceal;
}

unsigned int terminal_last_cell_attrs_strike(struct terminal *terminal)
{
  return terminal->lastCell.attrs.strike;
}

unsigned int terminal_last_cell_attrs_font(struct terminal *terminal)
{
  return terminal->lastCell.attrs.font;
}

unsigned int terminal_last_cell_attrs_dwl(struct terminal *terminal)
{
  return terminal->lastCell.attrs.dwl;
}

unsigned int terminal_last_cell_attrs_dhl(struct terminal *terminal)
{
  return terminal->lastCell.attrs.dhl;
}

unsigned int terminal_last_cell_attrs_small(struct terminal *terminal)
{
  return terminal->lastCell.attrs.small;
}

unsigned int terminal_last_cell_attrs_baseline(struct terminal *terminal)
{
  return terminal->lastCell.attrs.baseline;
}

uint8_t terminal_last_cell_fg_red(struct terminal *terminal)
{
  return terminal->lastCell.fg.rgb.red;
}

uint8_t terminal_last_cell_fg_green(struct terminal *terminal)
{
  return terminal->lastCell.fg.rgb.green;
}

uint8_t terminal_last_cell_fg_blue(struct terminal *terminal)
{
  return terminal->lastCell.fg.rgb.blue;
}

uint8_t terminal_last_cell_bg_red(struct terminal *terminal)
{
  return terminal->lastCell.bg.rgb.red;
}

uint8_t terminal_last_cell_bg_green(struct terminal *terminal)
{
  return terminal->lastCell.bg.rgb.green;
}

uint8_t terminal_last_cell_bg_blue(struct terminal *terminal)
{
  return terminal->lastCell.bg.rgb.blue;
}

int terminal_cursor_row(struct terminal *terminal)
{
  return terminal->cursorPos.row;
}

int terminal_cursor_col(struct terminal *terminal)
{
  return terminal->cursorPos.col;
}

void terminal_resize(struct terminal *terminal, int rows, int cols)
{
  vterm_set_size(terminal->vterm, rows, cols);
  vterm_screen_flush_damage(terminal->screen);

  struct winsize win = { 0 };
  win.ws_row = rows;
  win.ws_col = cols;
  ioctl(terminal->fd, TIOCSWINSZ, &win);
}

#if 0
int main(void)
{
  int cols = 80, rows = 24;

  struct terminal *terminal = terminal_new(rows, cols);
  terminal_process_input(terminal);
  terminal_input_char(terminal, 'l', 0);
  terminal_process_input(terminal);
  terminal_input_char(terminal, 's', 0);
  terminal_process_input(terminal);
  terminal_input_key(terminal, VTERM_KEY_ENTER, 0);
  terminal_process_input(terminal);
  terminal_process_input(terminal);
  terminal_process_input(terminal);

  for (int row = 0; row < rows; row++) {
    for (int col = 0; col < cols; col++) {
      terminal_query_cell(terminal, col, row);
      printf("%c", terminal->lastCell.chars[0]);
    }
    putchar('\n');
  }
}
#endif

#if 0
int main(void)
{
  printf("VTERM_KEY_NONE: %d\n", VTERM_KEY_NONE);

  printf("VTERM_KEY_ENTER: %d\n", VTERM_KEY_ENTER);
  printf("VTERM_KEY_TAB: %d\n", VTERM_KEY_TAB);
  printf("VTERM_KEY_BACKSPACE: %d\n", VTERM_KEY_BACKSPACE);
  printf("VTERM_KEY_ESCAPE: %d\n", VTERM_KEY_ESCAPE);

  printf("VTERM_KEY_UP: %d\n", VTERM_KEY_UP);
  printf("VTERM_KEY_DOWN: %d\n", VTERM_KEY_DOWN);
  printf("VTERM_KEY_LEFT: %d\n", VTERM_KEY_LEFT);
  printf("VTERM_KEY_RIGHT: %d\n", VTERM_KEY_RIGHT);

  printf("VTERM_KEY_INS: %d\n", VTERM_KEY_INS);
  printf("VTERM_KEY_DEL: %d\n", VTERM_KEY_DEL);
  printf("VTERM_KEY_HOME: %d\n", VTERM_KEY_HOME);
  printf("VTERM_KEY_END: %d\n", VTERM_KEY_END);
  printf("VTERM_KEY_PAGEUP: %d\n", VTERM_KEY_PAGEUP);
  printf("VTERM_KEY_PAGEDOWN: %d\n", VTERM_KEY_PAGEDOWN);

  printf("VTERM_KEY_FUNCTION_0: %d\n", VTERM_KEY_FUNCTION_0);
  printf("VTERM_KEY_FUNCTION_MAX: %d\n", VTERM_KEY_FUNCTION_MAX);

  printf("VTERM_KEY_KP_0: %d\n", VTERM_KEY_KP_0);
  printf("VTERM_KEY_KP_1: %d\n", VTERM_KEY_KP_1);
  printf("VTERM_KEY_KP_2: %d\n", VTERM_KEY_KP_2);
  printf("VTERM_KEY_KP_3: %d\n", VTERM_KEY_KP_3);
  printf("VTERM_KEY_KP_4: %d\n", VTERM_KEY_KP_4);
  printf("VTERM_KEY_KP_5: %d\n", VTERM_KEY_KP_5);
  printf("VTERM_KEY_KP_6: %d\n", VTERM_KEY_KP_6);
  printf("VTERM_KEY_KP_7: %d\n", VTERM_KEY_KP_7);
  printf("VTERM_KEY_KP_8: %d\n", VTERM_KEY_KP_8);
  printf("VTERM_KEY_KP_9: %d\n", VTERM_KEY_KP_9);
  printf("VTERM_KEY_KP_MULT: %d\n", VTERM_KEY_KP_MULT);
  printf("VTERM_KEY_KP_PLUS: %d\n", VTERM_KEY_KP_PLUS);
  printf("VTERM_KEY_KP_COMMA: %d\n", VTERM_KEY_KP_COMMA);
  printf("VTERM_KEY_KP_MINUS: %d\n", VTERM_KEY_KP_MINUS);
  printf("VTERM_KEY_KP_PERIOD: %d\n", VTERM_KEY_KP_PERIOD);
  printf("VTERM_KEY_KP_DIVIDE: %d\n", VTERM_KEY_KP_DIVIDE);
  printf("VTERM_KEY_KP_ENTER: %d\n", VTERM_KEY_KP_ENTER);
  printf("VTERM_KEY_KP_EQUAL: %d\n", VTERM_KEY_KP_EQUAL);

  printf("VTERM_KEY_MAX: %d\n", VTERM_KEY_MAX);
  printf("VTERM_N_KEYS: %d\n", VTERM_N_KEYS);
}
#endif
