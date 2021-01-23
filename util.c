#include "nerocc.h"

// Common util methods
void error(char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

void verror_at(int line_no, char *loc, char *fmt, va_list ap)
{
    char *line = loc;
    while (current_input < line && line[-1] != '\n')
    {
        line--;
    }

    char *end = loc;
    while (*end != '\n')
    {
        end++;
    }

    int indent = fprintf(stderr, "%s:%d: ", current_filename, line_no);
    fprintf(stderr, "%.*s\n", (int)(end - line), line);

    int pos = loc - line + indent;

    fprintf(stderr, "%*s", pos, "");
    fprintf(stderr, "^ ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

void error_at(char *loc, char *fmt, ...)
{
    int line_no = 1;
    for (char *p = current_input; p < loc; p++)
    {
        if (*p == '\n')
        {
            line_no++;
        }
    }

    va_list ap;
    va_start(ap, fmt);
    verror_at(line_no, loc, fmt, ap);
}

void error_tok(Token *tok, char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    verror_at(tok->line_no, tok->loc, fmt, ap);
}