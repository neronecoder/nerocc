#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>

typedef enum
{
    TK_PUNCT, // Punctuators
    TK_NUM,   // Numeric literals
    TK_EOF,   // End-of-file markers
} TokenKind;

typedef struct Token Token;

struct Token
{
    TokenKind kind;
    Token *next;
    int val;
    char *loc;
    int len;
};

Token *new_token(TokenKind kind, char *start, char *end)
{
    Token *tok = calloc(1, sizeof(Token));
    tok->kind = kind;
    tok->len = end - start;
    tok->loc = start;
    return tok;
}

void error(char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

static bool equal(Token *tok, char *op)
{
    return memcmp(tok->loc, op, tok->len) == 0 && op[tok->len] == '\0';
}

// Consumes the current token if it matches 's'
static Token *skip(Token *tok, char *s)
{
    if (!equal(tok, s))
    {
        error("Expected '%s'", s);
    }
    return tok->next;
}

static int get_number(Token *tok)
{
    if (tok->kind != TK_NUM)
    {
        error("Expected a number");
    }
    return tok->val;
}

static Token *tokenize(char *p)
{
    Token head = {};
    Token *cur = &head;

    while (*p)
    {
        // skip white spaces
        if (isspace(*p))
        {
            p++;
            continue;
        }

        // Numbers
        if (isdigit(*p))
        {
            cur->next = new_token(TK_NUM, p, p);
            cur = cur->next;
            char *tmp = p;
            cur->val = strtoul(p, &p, 10);
            cur->len = p - tmp;
            continue;
        }

        // Punctuators
        if (*p == '+' || *p == '-')
        {
            cur->next = new_token(TK_PUNCT, p, p + 1);
            cur = cur->next;
            p++;
            continue;
        }
        error("invalid token");
    }

    cur->next = new_token(TK_EOF, p, p);
    return head.next;
}

int main(int argc, char **argv)
{
    if (argc != 2)
    {
        fprintf(stderr, "%s: invalid number of arguments\n", argv[0]);
        return 1;
    }

    Token *tok = tokenize(argv[1]);

    // Code generation
    printf(".intel_syntax noprefix\n");
    printf(".globl main\n");
    printf("main:\n");

    // First token has to be number here.
    printf("    mov rax, %d\n", get_number(tok));
    tok = tok->next;

    // ... followed by either '+ <number>' or '- <number>'
    while (tok->kind != TK_EOF)
    {
        if (equal(tok, "+"))
        {
            printf("    add rax, %d\n", get_number(tok->next));
            tok = tok->next->next;
            continue;
        }

        // If token is not +, then it has to be - here.
        tok = skip(tok, "-");
        printf("    sub rax, %d\n", get_number(tok));
        tok = tok->next;
    }
    printf("    ret\n");
    return 0;
}