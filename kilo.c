/*** Includes ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

/*** Defines ***/

#define KILO_VERSION "0.0.1"
#define KILO_TAB_STOP 4
#define REL_LINE_NUMS true

#define CTRL_KEY(k) ((k) & 0x1f)

#define HL_KEYWORD(n) (HL_KEYWORD1 + n)

#define MAX(a, b) (a >= b ? (a) : (b))

enum editorKey {
    BACKSPACE = 127,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN
};

enum editorHighlight {
    HL_NORMAL = 0,
    HL_COMMENT,
    HL_MLCOMMENT,
    HL_KEYWORD1,
    HL_KEYWORD2,
    HL_STRING,
    HL_NUMBER,
    HL_MATCH
};

typedef enum editorMode { NORMAL_MODE, INSERT_MODE, COMMAND_MODE } editorMode;

typedef enum kiloCmd {
    CMD_WRITE,
    CMD_QUIT,
    CMD_FORCEQUIT,
    CMD_WRITEQUIT,
    CMD_SEARCH,
    CMD_GOTOLINE,
    CMD_UNKNOWN
} kiloCmd;

#define NORMAL_MODE_STR "NORMAL"
#define INSERT_MODE_STR "INSERT"
#define COMMAND_MODE_STR "COMMAND"

#define HL_HIGHLIGHT_NUMBERS (1 << 0)
#define HL_HIGHLIGHT_STRINGS (1 << 1)

/*** Data ***/

struct editorSyntax {
    char *filetype;
    char **filematch;
    char ***keywords;
    char *singleline_comment_start;
    char *multiline_comment_start;
    char *multiline_comment_end;
    int flags;
};

typedef struct erow {
    int idx;
    int size;
    int rsize;
    char *chars;
    char *render;
    unsigned char *hl;
    bool hl_open_comment;
} erow;

typedef struct editorState {
    struct editorState *prev;
    struct editorState *next;
    editorMode mode;
    int cx, cy;
    int rx;
    int rowoff;
    int coloff;
    int numrows;
    int max_line_num_len;
    erow *row;
} editorState;

struct editorConfig {
    int screenrows;
    int screencols;
    struct editorState *curr_state;
    bool rel_line_num;
    bool dirty;
    char *filename;
    char statusmsg[80];
    time_t statusmsg_time;
    struct editorSyntax *syntax;
    struct termios orig_termios;
};

struct editorConfig E;

/*** Filetypes ***/

char *C_HL_extensions[] = {".c", ".h", ".cpp", NULL};

char *C_HL_keywords_primary[] = {"switch", "if",       "while",   "for",
                                 "break",  "continue", "return",  "else",
                                 "struct", "union",    "typedef", "static",
                                 "enum",   "class",    "case",    NULL};

char *C_HL_keywords_secondary[] = {"int",  "long",     "double", "float",
                                   "char", "unsigned", "signed", "bool",
                                   "void", NULL};

char **C_HL_keywords[] = {C_HL_keywords_primary, C_HL_keywords_secondary, NULL};

struct editorSyntax HLDB[] = {{"c", C_HL_extensions, C_HL_keywords, "//", "/*",
                               "*/",
                               HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS}};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/*** Prototypes ***/

void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));
char *modeString();
void gotoPrevState();
void gotoNextState();
bool newState();

/*** Terminal ***/

void die(const char *s) {
    write(STDOUT_FILENO, "\x1b[2J", 4); // clear entire screen
    write(STDOUT_FILENO, "\x1b[H", 3);  // reset cursor position

    perror(s);
    exit(1);
}

void disableRawMode() {
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1) {
        die("tcsetattr");
    }
}

void enableRawMode() {
    if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) {
        die("tcgetattr");
    }
    atexit(disableRawMode);

    struct termios raw = E.orig_termios;
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= (CS8);
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) {
        die("tcsetattr");
    }
}

int editorReadKey() {
    int nread;
    char c;
    while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
        if (nread == -1 && errno != EAGAIN) {
            die("read");
        }
    }

    if (c == '\x1b') {
        char seq[3];

        if (read(STDIN_FILENO, &seq[0], 1) != 1) {
            return '\x1b';
        }
        if (read(STDIN_FILENO, &seq[1], 1) != 1) {
            return '\x1b';
        }

        if (seq[0] == '[') {
            if (seq[1] >= '0' && seq[1] <= '9') {
                if (read(STDIN_FILENO, &seq[2], 1) != 1) {
                    return '\x1b';
                }
                if (seq[2] == '~') {
                    switch (seq[1]) {
                    case '1':
                        return HOME_KEY;
                    case '3':
                        return DEL_KEY;
                    case '4':
                        return END_KEY;
                    case '5':
                        return PAGE_UP;
                    case '6':
                        return PAGE_DOWN;
                    case '7':
                        return HOME_KEY;
                    case '8':
                        return END_KEY;
                    }
                }
            } else {
                switch (seq[1]) {
                case 'A':
                    return ARROW_UP;
                case 'B':
                    return ARROW_DOWN;
                case 'C':
                    return ARROW_RIGHT;
                case 'D':
                    return ARROW_LEFT;
                case 'H':
                    return HOME_KEY;
                case 'F':
                    return END_KEY;
                }
            }
        } else if (seq[0] == 'O') {
            switch (seq[1]) {
            case 'H':
                return HOME_KEY;
            case 'F':
                return END_KEY;
            }
        }

        return '\x1b';
    } else {
        return c;
    }
}

int getCursorPosition(int *rows, int *cols) {
    if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) {
        return -1;
    }

    char buf[32];
    unsigned int i = 0;
    while (i < sizeof(buf) - 1) {
        if (read(STDIN_FILENO, &buf[i], 1) != 1) {
            break;
        }
        if (buf[i] == 'R') {
            break;
        }
        i++;
    }
    buf[i] = '\0';

    if (buf[0] != '\x1b' || buf[1] != '[') {
        return -1;
    }
    if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) {
        return -1;
    }

    return 0;
}

int getWindowSize(int *rows, int *cols) {
    struct winsize ws;

    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) {
            return -1;
        }
        return getCursorPosition(rows, cols);
    } else {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
        return 0;
    }
}

/*** Syntax Highlighting ***/

bool is_separator(int c) {
    return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow *row) {
    if (row->rsize <= 0) {
        return;
    }
    unsigned char *newhl = (unsigned char *)realloc(row->hl, row->rsize);
    if (newhl != NULL) {
        row->hl = newhl;
    } else {
        editorSetStatusMessage("[ERROR] (%s) | Allocation failed!", __func__);
        return;
    }

    memset(row->hl, HL_NORMAL, row->rsize);

    if (E.syntax == NULL) {
        return;
    }

    char ***keywords = E.syntax->keywords;

    char *scs = E.syntax->singleline_comment_start;
    char *mcs = E.syntax->multiline_comment_start;
    char *mce = E.syntax->multiline_comment_end;

    int scs_len = scs ? strlen(scs) : 0;
    int mcs_len = mcs ? strlen(mcs) : 0;
    int mce_len = mce ? strlen(mce) : 0;

    bool prev_sep = true;
    char in_string = 0;
    bool in_comment =
        (row->idx > 0 && E.curr_state->row[row->idx - 1].hl_open_comment);

    int i = 0;
    while (i < row->rsize) {
        char c = row->render[i];
        unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

        if (scs_len && !in_string && !in_comment) {
            if (!strncmp(&row->render[i], scs, scs_len)) {
                memset(&row->hl[i], HL_COMMENT, row->rsize - i);
                break;
            }
        }

        if (mcs_len && mce_len && !in_string) {
            if (in_comment) {
                row->hl[i] = HL_MLCOMMENT;
                if (!strncmp(&row->render[i], mce, mce_len)) {
                    memset(&row->hl[i], HL_MLCOMMENT, mce_len);
                    i += mce_len;
                    in_comment = false;
                    prev_sep = true;
                    continue;
                } else {
                    i++;
                    continue;
                }
            } else if (!strncmp(&row->render[i], mcs, mcs_len)) {
                memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
                i += mcs_len;
                in_comment = true;
                continue;
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
            if (in_string) {
                row->hl[i] = HL_STRING;
                if (c == '\\' && i + 1 < row->size) {
                    row->hl[i + 1] = HL_STRING;
                    i += 2;
                    continue;
                }
                if (c == in_string) {
                    in_string = 0;
                }
                i++;
                prev_sep = true;
                continue;
            } else {
                if (c == '"' || c == '\'') {
                    in_string = c;
                    row->hl[i] = HL_STRING;
                    i++;
                    continue;
                }
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
            if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
                (c == '.' && prev_hl == HL_NUMBER)) {
                row->hl[i] = HL_NUMBER;
                i++;
                prev_sep = false;
                continue;
            }
        }

        if (prev_sep) {
            bool kw = false;
            int group_idx = 0, word_idx = 0;
            while (keywords[group_idx] != NULL) {
                word_idx = 0;
                while (keywords[group_idx][word_idx] != NULL) {
                    int key_len = strlen(keywords[group_idx][word_idx]);

                    if (!strncmp(&row->render[i], keywords[group_idx][word_idx],
                                 key_len) &&
                        is_separator(row->render[i + key_len])) {
                        memset(&row->hl[i], HL_KEYWORD(group_idx), key_len);
                        i += key_len;
                        kw = true;
                        break;
                    }
                    word_idx++;
                }
                group_idx++;
            }
            if (kw) {
                prev_sep = false;
                continue;
            }
        }

        prev_sep = is_separator(c);
        i++;
    }

    bool changed = (row->hl_open_comment != in_comment);
    row->hl_open_comment = in_comment;
    if (changed && row->idx + 1 < E.curr_state->numrows) {
        editorUpdateSyntax(&E.curr_state->row[row->idx + 1]);
    }
}

int editorSyntaxToColor(int hl) {
    switch (hl) {
    case HL_COMMENT:
    case HL_MLCOMMENT:
        return 36;
    case HL_KEYWORD1:
        return 33;
    case HL_KEYWORD2:
        return 32;
    case HL_STRING:
        return 35;
    case HL_NUMBER:
        return 31;
    case HL_MATCH:
        return 34;
    default:
        return 37;
    }
}

void editorSelectSyntaxHighlight() {
    E.syntax = NULL;
    if (E.filename == NULL) {
        return;
    }

    char *ext = strrchr(E.filename, '.');

    for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
        struct editorSyntax *s = &HLDB[j];
        unsigned int i = 0;
        while (s->filematch[i]) {
            int is_ext = (s->filematch[i][0] == '.');
            if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
                (!is_ext && strstr(E.filename, s->filematch[i]))) {
                E.syntax = s;

                for (int filerow = 0; filerow < E.curr_state->numrows;
                     filerow++) {
                    editorUpdateSyntax(&E.curr_state->row[filerow]);
                }

                return;
            }
            i++;
        }
    }
}

/*** Row Operations ***/

int editorRowCxToRx(erow *row, int cx) {
    int offset = E.curr_state->max_line_num_len + 1;

    int rx = 0;
    for (int j = 0; j < cx; j++) {
        if (row->chars[j] == '\t') {
            rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
        }
        rx++;
    }

    return rx + offset;
}

int editorRowRxToCx(erow *row, int rx) {
    int offset = E.curr_state->max_line_num_len + 1;
    int cur_rx = 0;
    int cx;
    for (cx = 0; cx < row->size; cx++) {
        if (row->chars[cx] == '\t') {
            cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
        }
        cur_rx++;

        if (cur_rx > rx) {
            return MAX(cx - offset, 0);
        }
    }

    return MAX(cx - offset, 0);
}

void editorUpdateRow(erow *row) {
    int tabs = 0;
    for (int j = 0; j < row->size; j++) {
        if (row->chars[j] == '\t') {
            tabs++;
        }
    }

    free(row->render);
    row->render = (char *)malloc(row->size + tabs * (KILO_TAB_STOP - 1) + 1);

    int idx = 0;
    for (int j = 0; j < row->size; j++) {
        if (row->chars[j] == '\t') {
            row->render[idx++] = ' ';
            while (idx % KILO_TAB_STOP != 0) {
                row->render[idx++] = ' ';
            }
        } else {
            row->render[idx++] = row->chars[j];
        }
    }

    row->render[idx] = '\0';
    row->rsize = idx;

    editorUpdateSyntax(row);
}

void editorInsertRow(int at, char *s, size_t len) {
    if (at < 0 || at > E.curr_state->numrows) {
        return;
    }
    E.curr_state->row =
        realloc(E.curr_state->row, sizeof(erow) * (E.curr_state->numrows + 1));
    memmove(&E.curr_state->row[at + 1], &E.curr_state->row[at],
            sizeof(erow) * (E.curr_state->numrows - at));
    for (int j = at + 1; j <= E.curr_state->numrows; j++) {
        E.curr_state->row[j].idx++;
    }

    E.curr_state->row[at].idx = at;

    E.curr_state->row[at].size = len;
    E.curr_state->row[at].chars = (char *)malloc(len + 1);
    memcpy(E.curr_state->row[at].chars, s, len);
    E.curr_state->row[at].chars[len] = '\0';

    E.curr_state->row[at].rsize = 0;
    E.curr_state->row[at].render = NULL;
    E.curr_state->row[at].hl = NULL;
    E.curr_state->row[at].hl_open_comment = false;
    editorUpdateRow(&E.curr_state->row[at]);

    E.curr_state->numrows++;
    E.dirty = true;
}

void editorFreeRow(erow *row) {
    free(row->render);
    free(row->chars);
    free(row->hl);
}

void editorDelRow(int at) {
    if (at < 0 || at >= E.curr_state->numrows) {
        return;
    }

    editorFreeRow(&E.curr_state->row[at]);
    memmove(&E.curr_state->row[at], &E.curr_state->row[at + 1],
            sizeof(erow) * (E.curr_state->numrows - at - 1));
    for (int j = at; j < E.curr_state->numrows - 1; j++) {
        E.curr_state->row[j].idx--;
    }

    E.curr_state->numrows--;
    E.dirty = true;
}

void editorRowInsertChar(erow *row, int at, int c) {
    if (at < 0 || at > row->size) {
        at = row->size;
    }

    row->chars = realloc(row->chars, row->size + 2);

    memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
    row->size++;
    row->chars[at] = c;
    editorUpdateRow(row);
    E.dirty = true;
}

void editorRowAppendString(erow *row, char *s, size_t len) {
    row->chars = realloc(row->chars, row->size + len + 1);
    memcpy(&row->chars[row->size], s, len);
    row->size += len;
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
    E.dirty = true;
}

void editorRowDelChar(erow *row, int at) {
    if (at < 0 || at >= row->size) {
        return;
    }
    memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
    row->size--;
    editorUpdateRow(row);
    E.dirty = true;
}

/*** Editor Operations ***/

void editorInsertChar(int c) {
    if (E.curr_state->cy == E.curr_state->numrows) {
        editorInsertRow(E.curr_state->numrows, "", 0);
    }
    editorRowInsertChar(&E.curr_state->row[E.curr_state->cy], E.curr_state->cx,
                        c);
    E.curr_state->cx++;
}

void editorInsertNewline() {
    if (E.curr_state->cx == 0) {
        editorInsertRow(E.curr_state->cy, "", 0);
    } else {
        erow *row = &E.curr_state->row[E.curr_state->cy];
        editorInsertRow(E.curr_state->cy + 1, &row->chars[E.curr_state->cx],
                        row->size - E.curr_state->cx);
        row = &E.curr_state->row[E.curr_state->cy];
        row->size = E.curr_state->cx;

        char *new_row = (char *)realloc(row->chars, row->size + 1);
        if (new_row != NULL) {
            row->chars = new_row;
        }
        row->chars[row->size] = '\0'; // BUGBUG memory leak without realloc?
        editorUpdateRow(row);
    }
    E.curr_state->cy++;
    E.curr_state->cx = 0;
}

void editorDelChar() {
    if (E.curr_state->cy == E.curr_state->numrows) {
        return;
    }
    if (E.curr_state->cx == 0 && E.curr_state->cy == 0) {
        return;
    }

    erow *row = &E.curr_state->row[E.curr_state->cy];
    if (E.curr_state->cx > 0) {
        editorRowDelChar(row, E.curr_state->cx - 1);
        E.curr_state->cx--;
    } else {
        E.curr_state->cx = E.curr_state->row[E.curr_state->cy - 1].size;
        editorRowAppendString(&E.curr_state->row[E.curr_state->cy - 1],
                              row->chars, row->size);
        editorDelRow(E.curr_state->cy);
        E.curr_state->cy--;
    }
}

/*** File I/O ***/

char *editorRowsToString(int *buflen) {
    int totlen = 0;
    for (int j = 0; j < E.curr_state->numrows; j++) {
        totlen += E.curr_state->row[j].size + 1;
    }
    *buflen = totlen;

    char *buf = (char *)malloc(totlen);
    char *p = buf;
    for (int j = 0; j < E.curr_state->numrows; j++) {
        memcpy(p, E.curr_state->row[j].chars, E.curr_state->row[j].size);
        p += E.curr_state->row[j].size;
        *p = '\n';
        p++;
    }

    return buf;
}

void editorOpen(char *filename) {
    free(E.filename);
    E.filename = strdup(filename);

    editorSelectSyntaxHighlight();

    FILE *fp = fopen(filename, "r");
    if (!fp) {
        die("fopen");
    }

    char *line = NULL;
    size_t linecap = 0;
    ssize_t linelen;
    while ((linelen = getline(&line, &linecap, fp)) != -1) {
        while (linelen > 0 &&
               (line[linelen - 1] == '\n' || line[linelen - 1] == '\r')) {
            linelen--;
        }
        editorInsertRow(E.curr_state->numrows, line, linelen);
    }

    free(line);
    fclose(fp);
    E.dirty = false;
}

void editorSave() {
    if (E.filename == NULL) {
        E.filename = editorPrompt("Save as: %s", NULL);
        if (E.filename == NULL) {
            editorSetStatusMessage("Save aborted");
            return;
        }
        editorSelectSyntaxHighlight();
    }

    int len;
    char *buf = editorRowsToString(&len);

    int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
    if (fd != -1) {
        if (ftruncate(fd, len) != -1) {
            if (write(fd, buf, len) == len) {
                close(fd);
                free(buf);
                E.dirty = false;
                editorSetStatusMessage("%d bytes written to %s", len,
                                       E.filename);
                return;
            }
        }
        close(fd);
    }

    free(buf);
    editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
}

/*** Find ***/

void editorFindCallback(char *query, int key) {
    static int last_match = -1;
    static bool findforward = true;

    static int saved_hl_line = -1;
    static char *saved_hl = NULL;

    if (saved_hl) {
        memcpy(E.curr_state->row[saved_hl_line].hl, saved_hl,
               E.curr_state->row[saved_hl_line].rsize);
        free(saved_hl);
        saved_hl = NULL;
    }

    if (key == '\r' || key == '\x1b') {
        last_match = -1;
        findforward = true;
        return;
    } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
        findforward = true;
    } else if (key == ARROW_LEFT || key == ARROW_UP) {
        findforward = false;
    } else {
        last_match = -1;
        findforward = true;
    }

    if (last_match == -1) {
        findforward = true;
    }
    int current = last_match;

    for (int i = 0; i < E.curr_state->numrows; i++) {
        current += findforward ? 1 : -1;
        if (current == -1) {
            current = E.curr_state->numrows - 1;
        } else if (current == E.curr_state->numrows) {
            current = 0;
        }

        erow *row = &E.curr_state->row[current];
        char *match = strstr(row->render, query);
        if (match) {
            last_match = current;
            E.curr_state->cy = current;
            E.curr_state->cx = editorRowRxToCx(row, match - row->render);
            E.curr_state->rowoff = E.curr_state->numrows;

            saved_hl_line = current;
            saved_hl = (char *)malloc(row->rsize);
            if (saved_hl == NULL) {
                editorSetStatusMessage(
                    "[E.curr_state->ROR] | (%s) Allocation failed!", __func__);
                saved_hl_line = -1;
            } else {
                memcpy(saved_hl, row->hl, row->rsize);
            }
            memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
            break;
        }
    }
}

void editorFind() {
    int saved_cx = E.curr_state->cx;
    int saved_cy = E.curr_state->cy;
    int saved_coloff = E.curr_state->coloff;
    int saved_rowoff = E.curr_state->rowoff;

    char *query =
        editorPrompt("Search %s (Use ESC/Arrows/Enter)", editorFindCallback);
    if (query == NULL) {
        editorSetStatusMessage("Search aborted");
        return;
    }

    if (query != NULL) {
        free(query);
    } else {
        E.curr_state->cx = saved_cx;
        E.curr_state->cy = saved_cy;
        E.curr_state->coloff = saved_coloff;
        E.curr_state->rowoff = saved_rowoff;
    }
}

/*** Go To Line ***/

void editorGotoLine() {
    int saved_cx = E.curr_state->cx;
    int saved_cy = E.curr_state->cy;
    int saved_coloff = E.curr_state->coloff;
    int saved_rowoff = E.curr_state->rowoff;

    char *line = editorPrompt("Goto Line #: %s", NULL);
    if (line == NULL) {
        editorSetStatusMessage("Goto operation aborted");
        return;
    }

    if (line != NULL) {
        int target_line = atoi(line);
        if (target_line >= 1 && target_line <= E.curr_state->numrows) {
            E.curr_state->rowoff = E.curr_state->numrows;
            E.curr_state->cy = target_line - 1;
        } else {
            editorSetStatusMessage("Invalid line number");
        }
        free(line);
    } else {
        E.curr_state->cx = saved_cx;
        E.curr_state->cy = saved_cy;
        E.curr_state->coloff = saved_coloff;
        E.curr_state->rowoff = saved_rowoff;
    }
}

/*** Append Buffer ***/

struct abuf {
    char *b;
    int len;
};

#define ABUF_INIT                                                              \
    { NULL, 0 }

void abAppend(struct abuf *ab, const char *s, int len) {
    char *new = realloc(ab->b, ab->len + len);

    if (new == NULL) {
        return;
    }
    memcpy(&new[ab->len], s, len);
    ab->b = new;
    ab->len += len;
}

void abFree(struct abuf *ab) { free(ab->b); }

/*** Output ***/

void editorScroll() {
    E.curr_state->rx = E.curr_state->max_line_num_len + 1;
    if (E.curr_state->cy < E.curr_state->numrows) {
        E.curr_state->rx = editorRowCxToRx(&E.curr_state->row[E.curr_state->cy],
                                           E.curr_state->cx);
    }

    if (E.curr_state->cy < E.curr_state->rowoff) {
        E.curr_state->rowoff = E.curr_state->cy;
    }
    if (E.curr_state->cy >= E.curr_state->rowoff + E.screenrows) {
        E.curr_state->rowoff = E.curr_state->cy - E.screenrows + 1;
    }
    if (E.curr_state->rx < E.curr_state->coloff) {
        E.curr_state->coloff = E.curr_state->rx;
    }
    if (E.curr_state->rx >= E.curr_state->coloff + E.screencols) {
        E.curr_state->coloff = E.curr_state->rx - E.screencols + 1;
    }
}

void editorDrawRows(struct abuf *ab, int row_idx) {
    for (int y = 0; y < E.screenrows; y++) {
        int filerow = y + E.curr_state->rowoff;
        if (filerow >= E.curr_state->numrows) {
            if (E.curr_state->numrows == 0 && y == E.screenrows / 3) {
                char welcome[80];
                int welcomelen =
                    snprintf(welcome, sizeof(welcome),
                             "Kilo Editor -- Version %s", KILO_VERSION);
                if (welcomelen > E.screencols) {
                    welcomelen = E.screencols;
                }
                int padding = (E.screencols - welcomelen) / 2;
                if (padding) {
                    abAppend(ab, "~", 1);
                    padding--;
                }
                while (padding--) {
                    abAppend(ab, " ", 1);
                }
                abAppend(ab, welcome, welcomelen);
            } else {
                abAppend(ab, "~", 1);
            }
        } else {
            // Draw line number
            char buf[8];

            abAppend(ab, "\x1b[7m", 4); // inverted colors

            int num;
            if (E.rel_line_num) {
                if (filerow == row_idx) {
                    num = filerow + 1;
                } else {
                    num = abs(filerow - row_idx);
                }
            } else {
                num = filerow + 1;
            }
            int num_len =
                snprintf(buf, 8, "%*d ", E.curr_state->max_line_num_len, num);
            if (num_len > 0 && num_len < 8) {
                abAppend(ab, buf, num_len);
            } else {
                abAppend(ab, "?", 1);
            }
            abAppend(ab, "\x1b[m", 3); // back to normal formatting

            int len = E.curr_state->row[filerow].rsize - E.curr_state->coloff;
            if (len < 0) {
                len = 0;
            }
            if (len > (E.screencols - E.curr_state->max_line_num_len - 1)) {
                len = E.screencols - E.curr_state->max_line_num_len - 1;
            }
            char *c = &E.curr_state->row[filerow].render[E.curr_state->coloff];
            unsigned char *hl =
                &E.curr_state->row[filerow].hl[E.curr_state->coloff];
            int current_color = -1;
            for (int j = 0; j < len; j++) {
                // non-printable characters get translated to printable
                // representations
                if (iscntrl(c[j])) {
                    char sym = (c[j] <= 26) ? '@' + c[j] : '?';
                    abAppend(ab, "\x1b[7m", 4); // invert colors
                    abAppend(ab, &sym, 1);
                    abAppend(ab, "\x1b[m", 3); // turn off all colors
                    // restore previous color if there is one
                    if (current_color != -1) {
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm",
                                            current_color);
                        abAppend(ab, buf, clen);
                    }
                } else if (hl[j] == HL_NORMAL) {
                    if (current_color != -1) {
                        abAppend(ab, "\x1b[39m", 5);
                        current_color = -1;
                    }
                    abAppend(ab, &c[j], 1);
                } else {
                    int color = editorSyntaxToColor(hl[j]);
                    if (color != current_color) {
                        current_color = color;
                        char buf[16];
                        int clen =
                            snprintf(buf, sizeof(buf), "\x1b[%dm", color);
                        abAppend(ab, buf, clen);
                    }
                    abAppend(ab, &c[j], 1);
                }
            }
            abAppend(ab, "\x1b[39m", 5);
        }

        abAppend(ab, "\x1b[K", 3);
        abAppend(ab, "\r\n", 2);
    }
}

void editorDrawStatusBar(struct abuf *ab) {
    abAppend(ab, "\x1b[7m", 4); // inverted colors

    char status[80], rstatus[80];
    int len = snprintf(status, sizeof(status), "[%s] %.20s - %d lines %s",
                       modeString(), E.filename ? E.filename : "[No Name]",
                       E.curr_state->numrows, E.dirty ? "(modified)" : "");
    int percent = ((float)E.curr_state->cy / E.curr_state->numrows) * 100;
    int rlen = snprintf(rstatus, sizeof(rstatus), "%s - %d%% - %d:%d",
                        E.syntax ? E.syntax->filetype : "no ft", percent,
                        E.curr_state->cy + 1, E.curr_state->cx + 1);
    if (len > E.screencols) {
        len = E.screencols;
    }
    abAppend(ab, status, len);

    while (len < E.screencols) {
        if (E.screencols - len == rlen) {
            abAppend(ab, rstatus, rlen);
            break;
        } else {
            abAppend(ab, " ", 1);
            len++;
        }
    }

    abAppend(ab, "\x1b[m", 3); // back to normal formatting
    abAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(struct abuf *ab) {
    abAppend(ab, "\x1b[K", 3); // clear the message bar
    int msglen = strlen(E.statusmsg);
    if (msglen > E.screencols) {
        msglen = E.screencols;
    }

    if (msglen && time(NULL) - E.statusmsg_time < 5) {
        abAppend(ab, E.statusmsg, msglen);
    }
}

void editorRefreshScreen() {
    E.curr_state->max_line_num_len =
        snprintf(NULL, 0, "%d", E.curr_state->numrows);

    editorScroll();

    struct abuf ab = ABUF_INIT;

    int row_idx = E.curr_state->cy;
    abAppend(&ab, "\x1b[?25l", 6); // hide cursor
    abAppend(&ab, "\x1b[H", 3);    // reset cursor position

    editorDrawRows(&ab, row_idx);
    editorDrawStatusBar(&ab);
    editorDrawMessageBar(&ab);

    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH",
             (E.curr_state->cy - E.curr_state->rowoff) + 1,
             (E.curr_state->rx - E.curr_state->coloff) + 1);
    abAppend(&ab, buf, strlen(buf));

    abAppend(&ab, "\x1b[?25h", 6); // show cursor

    write(STDOUT_FILENO, ab.b, ab.len);
    abFree(&ab);
}

void editorSetStatusMessage(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
    va_end(ap);
    E.statusmsg_time = time(NULL);
}

/*** Editor Mode ***/

char *modeString() {
    switch (E.curr_state->mode) {
    case NORMAL_MODE:
        return NORMAL_MODE_STR;
    case INSERT_MODE:
        return INSERT_MODE_STR;
    case COMMAND_MODE:
        return COMMAND_MODE_STR;
    default:
        return "UNKNOWN";
    }
}

kiloCmd inputToCmd(char *s) {
    // write
    if (!strncmp(s, "write", strlen(s))) {
        return CMD_WRITE;
    }
    // quit
    if (!strncmp(s, "quit", strlen(s))) {
        return CMD_QUIT;
    }
    // write quit
    if (!strncmp(s, "wq", strlen(s)) || !strncmp(s, "writequit", strlen(s))) {
        return CMD_WRITEQUIT;
    }
    // force quit
    if (!strncmp(s, "q!", strlen(s)) || !strncmp(s, "quit!", strlen(s))) {
        return CMD_FORCEQUIT;
    }
    // find
    if (!strncmp(s, "find", strlen(s))) {
        return CMD_SEARCH;
    }
    // goto line
    if (!strncmp(s, "goto", strlen(s))) {
        return CMD_GOTOLINE;
    }
    return CMD_UNKNOWN;
}

void cmdModeCallback(char *s, int c) {
    if (c == '\x1b') {
        return;
    } else if (c == '\r') {
        editorSetStatusMessage("Would have tried to run [%s]", s);
        // match each command in turn here...
        kiloCmd cmd = inputToCmd(s);
        switch (cmd) {
        case CMD_WRITE:
            editorSave();
            break;

        case CMD_QUIT:
            if (E.dirty) {
                editorSetStatusMessage("[WARNING] | File has unsaved changes.");
                break;
            }
            // fall through

        case CMD_FORCEQUIT:
            write(STDOUT_FILENO, "\x1b[2J", 4); // clear entire screen
            write(STDOUT_FILENO, "\x1b[H", 3);  // reset cursor position
            exit(0);
            break;

        case CMD_WRITEQUIT:
            editorSave();
            write(STDOUT_FILENO, "\x1b[2J", 4); // clear entire screen
            write(STDOUT_FILENO, "\x1b[H", 3);  // reset cursor position
            exit(0);
            break;

        case CMD_SEARCH:
            editorFind();
            break;

        case CMD_GOTOLINE:
            editorGotoLine();
            break;

        case CMD_UNKNOWN:
            editorSetStatusMessage("Unknown command");
            break;
        }
    }
}

/*** Input ***/

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
    size_t bufsize = 128;
    char *buf = (char *)malloc(bufsize);
    if (buf == NULL) {
        return NULL;
    }

    size_t buflen = 0;
    buf[0] = '\0';

    while (true) {
        editorSetStatusMessage(prompt, buf);
        editorRefreshScreen();

        int c = editorReadKey();
        if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
            if (buflen > 0) {
                buf[--buflen] = '\0';
            }
        } else if (c == '\x1b') {
            editorSetStatusMessage("");
            if (callback) {
                callback(buf, c);
            }
            free(buf);
            return NULL;
        } else if (c == '\r') {
            if (buflen > 0) {
                editorSetStatusMessage("");
                if (callback) {
                    callback(buf, c);
                }
                return buf;
            }
        } else if (!iscntrl(c) && c < 128) {
            if (buflen == bufsize - 1) {
                bufsize *= 2;
                char *new_buff = realloc(buf, bufsize);
                if (new_buff != NULL) {
                    buf = new_buff;
                } else {
                    editorSetStatusMessage("[ERROR] | (%s) Allocation failed!",
                                           __func__);
                    return buf;
                }
            }
            buf[buflen++] = c;
            buf[buflen] = '\0';
        }

        if (callback) {
            callback(buf, c);
        }
    }
}

void editorMoveCursor(int key) {
    erow *row = (E.curr_state->cy >= E.curr_state->numrows)
                    ? NULL
                    : &E.curr_state->row[E.curr_state->cy];

    switch (key) {
    case ARROW_LEFT:
        if (E.curr_state->cx > 0) {
            E.curr_state->cx--;
        } else if (E.curr_state->cy > 0) {
            E.curr_state->cy--;
            E.curr_state->cx = E.curr_state->row[E.curr_state->cy].size;
        }
        break;
    case ARROW_RIGHT:
        if (row && E.curr_state->cx < row->size) {
            E.curr_state->cx++;
        } else if (row && E.curr_state->cx == row->size) {
            E.curr_state->cy++;
            E.curr_state->cx = 0;
        }
        break;
    case ARROW_UP:
        if (E.curr_state->cy != 0) {
            E.curr_state->cy--;
        }
        break;
    case ARROW_DOWN:
        if (E.curr_state->cy < E.curr_state->numrows) {
            E.curr_state->cy++;
        }
        break;
    }

    row = (E.curr_state->cy >= E.curr_state->numrows)
              ? NULL
              : &E.curr_state->row[E.curr_state->cy];
    int rowlen = row ? row->size : 0;
    if (E.curr_state->cx > rowlen) {
        E.curr_state->cx = rowlen;
    }
}

void editorProcessKeyInsert(int c) {
    switch (c) {
    case '\r':
        newState();
        editorInsertNewline();

        break;

    case HOME_KEY:
        E.curr_state->cx = 0;
        break;
    case END_KEY:
        if (E.curr_state->cy < E.curr_state->numrows) {
            E.curr_state->cx = E.curr_state->row[E.curr_state->cy].size;
        }
        break;

    case BACKSPACE:
    case CTRL_KEY('h'):
    case DEL_KEY:
        newState();
        if (c == DEL_KEY) {
            editorMoveCursor(ARROW_RIGHT);
        }
        editorDelChar();
        break;

    case PAGE_UP:
    case PAGE_DOWN: {
        if (c == PAGE_UP) {
            E.curr_state->cy = E.curr_state->rowoff;
        } else if (c == PAGE_DOWN) {
            E.curr_state->cy = E.curr_state->rowoff + E.screenrows - 1;
            if (E.curr_state->cy > E.curr_state->numrows) {
                E.curr_state->cy = E.curr_state->numrows;
            }
        }
        int times = E.screenrows;
        while (times--) {
            editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
        }
        break;
    }
    case ARROW_LEFT:
    case ARROW_RIGHT:
    case ARROW_UP:
    case ARROW_DOWN:
        editorMoveCursor(c);
        break;
    case CTRL_KEY('l'):
    case '\x1b':
        E.curr_state->mode = NORMAL_MODE;
        break;

    default:
        newState();
        editorInsertChar(c);
        break;
    }
}

void editorProcessKeyNormal(int c) {
    switch (c) {
    case '\r':
        editorMoveCursor(ARROW_DOWN);
        break;

    case HOME_KEY:
        E.curr_state->cx = 0;
        break;
    case END_KEY:
        if (E.curr_state->cy < E.curr_state->numrows) {
            E.curr_state->cx = E.curr_state->row[E.curr_state->cy].size;
        }
        break;

    case BACKSPACE:
        editorMoveCursor(ARROW_LEFT);
        break;

    case DEL_KEY:
        break;

    case PAGE_UP:
    case PAGE_DOWN: {
        if (c == PAGE_UP) {
            E.curr_state->cy = E.curr_state->rowoff;
        } else if (c == PAGE_DOWN) {
            E.curr_state->cy = E.curr_state->rowoff + E.screenrows - 1;
            if (E.curr_state->cy > E.curr_state->numrows) {
                E.curr_state->cy = E.curr_state->numrows;
            }
        }
        int times = E.screenrows;
        while (times--) {
            editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
        }
        break;
    }
    case ARROW_LEFT:
    case ARROW_RIGHT:
    case ARROW_UP:
    case ARROW_DOWN:
        editorMoveCursor(c);
        break;
    case CTRL_KEY('l'):
    case '\x1b':
        break;

    case 'u':
        gotoPrevState();
        break;
    case 'U':
        gotoNextState();
        break;

    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        // TODO
        break;

    case 'h':
        editorMoveCursor(ARROW_LEFT);
        break;
    case 'j':
        editorMoveCursor(ARROW_DOWN);
        break;
    case 'k':
        editorMoveCursor(ARROW_UP);
        break;
    case 'l':
        editorMoveCursor(ARROW_RIGHT);
        break;

    case 'b': {
        // move cursor right until
        // separator is encountered
        int old_y = E.curr_state->cy;
        do {
            editorMoveCursor(ARROW_LEFT);
        } while (
            !is_separator(
                E.curr_state->row[E.curr_state->cy].chars[E.curr_state->cx]) &&
            old_y == E.curr_state->cy);
        break;
    }
    case 'w': {
        // move cursor right until
        // separator is encountered
        int old_y = E.curr_state->cy;
        do {
            editorMoveCursor(ARROW_RIGHT);
        } while (
            !is_separator(
                E.curr_state->row[E.curr_state->cy].chars[E.curr_state->cx]) &&
            old_y == E.curr_state->cy);
        break;
    }

    case 'o':
        newState();
        E.curr_state->cx = E.curr_state->row[E.curr_state->cy].size;
        editorInsertNewline();
        E.curr_state->mode = INSERT_MODE;
        break;

    case 'i':
        E.curr_state->mode = INSERT_MODE;
        break;

    case ':':
        E.curr_state->mode = COMMAND_MODE;
        editorPrompt(":%s", cmdModeCallback);
        E.curr_state->mode = NORMAL_MODE;
        break;

    default:
        break;
    }
}

void editorProcessKeypress() {

    int c = editorReadKey();

    switch (E.curr_state->mode) {
    case NORMAL_MODE:
        editorProcessKeyNormal(c);
        break;
    case INSERT_MODE:
        editorProcessKeyInsert(c);
        break;
    case COMMAND_MODE:
        // this is all handled inside editorProcessKeyNormal()
        break;
    }
}

/*** Editor State ***/

void dbgPrintState(editorState *state) {
    if (state == NULL) {
        return;
    }
    editorSetStatusMessage("prev: %p", (void *)state->prev);
    editorRefreshScreen();
    sleep(2);
    editorSetStatusMessage("next: %p", (void *)state->next);
    editorRefreshScreen();
    sleep(2);
    editorSetStatusMessage("mode: %d", state->mode);
    editorRefreshScreen();
    sleep(2);
    editorSetStatusMessage("cx: %d, cy: %d", state->cx, state->cy);
    editorRefreshScreen();
    sleep(2);
    editorSetStatusMessage("rx: %d", state->rx);
    editorRefreshScreen();
    sleep(2);
    editorSetStatusMessage("rowoff: %d", state->rowoff);
    editorRefreshScreen();
    sleep(2);
    editorSetStatusMessage("coloff: %d", state->coloff);
    editorRefreshScreen();
    sleep(2);
    editorSetStatusMessage("numrows: %d", state->numrows);
    editorRefreshScreen();
    sleep(2);
    editorSetStatusMessage("max_line_num_len: %d", state->max_line_num_len);
    editorRefreshScreen();
    sleep(2);
    editorSetStatusMessage("row: %p", (void *)state->row);
    editorRefreshScreen();
    sleep(2);
}

erow *erowCopy(erow *row) {
    erow *new_row = (erow *)malloc(sizeof(erow));
    if (new_row == NULL) {
        return NULL;
    }

    memcpy(new_row, row, sizeof(erow));
    new_row->chars = NULL;
    new_row->render = NULL;
    new_row->hl = NULL;

    new_row->chars = (char *)malloc(row->size + 1);
    if (new_row->chars == NULL) {
        free(new_row);
        return NULL;
    }

    new_row->render = (char *)malloc(row->rsize);
    if (new_row->render == NULL) {
        free(new_row->chars);
        free(new_row);
        return NULL;
    }

    new_row->hl = (unsigned char *)malloc(row->rsize);
    if (new_row->hl == NULL) {
        free(new_row->render);
        free(new_row->chars);
        free(new_row);
        return NULL;
    }

    if (row->size > 0) {
        memcpy(new_row->chars, row->chars, row->size);
        new_row->chars[row->size] = '\0';
    }
    if (row->rsize > 0) {
        memcpy(new_row->render, row->render, row->rsize);
        memcpy(new_row->hl, row->hl, row->rsize);
    }

    return new_row;
}

// create a new editorState object with the state variables copied over
// leaves prev and next fields NULL
editorState *stateCopy(editorState *state) {
    editorState *new_state = (editorState *)malloc(sizeof(editorState));
    if (new_state == NULL) {
        return NULL;
    }

    memcpy(new_state, state, sizeof(editorState));
    new_state->row = erowCopy(state->row);
    if (new_state->row == NULL) {
        free(new_state);
        return NULL;
    }
    new_state->prev = NULL;
    new_state->next = NULL;

    return new_state;
}

// Appends a new state to the global linked list
bool newState() {
    //  create deep copy of old state
    editorState *new_state = stateCopy(E.curr_state);
    if (new_state == NULL) {
        editorSetStatusMessage("[ERROR] | (%s) Allocation failed", __func__);
        return false;
    }

    E.curr_state->next = new_state;
    new_state->prev = E.curr_state;
    new_state->next = NULL;
    // editorSetStatusMessage("new_state->prev: %p, curr state: %p",
    // new_state->prev,
    //                        E.curr_state);
    // dbgPrintState(new_state);
    // editorRefreshScreen();
    // sleep(5);
    E.curr_state = new_state;
    // E.curr_state = E.curr_state;
    //  editorSetStatusMessage("This will never print");
    //  editorRefreshScreen();
    //  sleep(5);

    return true;
}

void gotoPrevState() {
    if (E.curr_state->prev != NULL) {
        E.curr_state = E.curr_state->prev;
    } else {
        editorSetStatusMessage("Already at oldest change");
    }
}

void gotoNextState() {
    if (E.curr_state->next != NULL) {
        E.curr_state = E.curr_state->next;
    } else {
        editorSetStatusMessage("Already at newest change");
    }
}

/*** Init ***/

void initEditor() {
    editorState *state = (editorState *)malloc(sizeof(editorState));
    if (state == NULL) {
        die("Allocation failed");
    }
    state->mode = NORMAL_MODE;
    state->cx = 0;
    state->cy = 0;
    state->rx = 0;
    state->rowoff = 0;
    state->coloff = 0;
    state->numrows = 0;
    state->row = NULL;
    state->max_line_num_len = 0;
    state->prev = NULL;
    state->next = NULL;

    E.curr_state = state;
    E.rel_line_num = REL_LINE_NUMS;
    E.filename = NULL;
    E.statusmsg[0] = '\0';
    E.statusmsg_time = 0;
    E.syntax = NULL;

    if (getWindowSize(&E.screenrows, &E.screencols) == -1) {
        die("getWindowSize");
    }
    E.screenrows -= 2;
}

int main(int argc, char *argv[]) {
    enableRawMode();
    initEditor();
    if (argc >= 2) {
        editorOpen(argv[1]);
    }

    editorSetStatusMessage(
        "HELP: :w[rite], :q[uit], :q[uit]!, :wq, :f[ind], :g[oto]");

    while (true) {
        editorRefreshScreen();
        editorProcessKeypress();
    }

    return 0;
}
