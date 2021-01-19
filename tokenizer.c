#include "nerocc.h"

Token *new_token(TokenKind kind, char *start, char *end)
{
    Token *tok = calloc(1, sizeof(Token));
    tok->kind = kind;
    tok->len = end - start;
    tok->loc = start;
    return tok;
}

bool equal(Token *tok, char *op)
{
    return memcmp(tok->loc, op, tok->len) == 0 && op[tok->len] == '\0';
}

// Consumes the current token if it matches 's'
Token *skip(Token *tok, char *s)
{
    if (!equal(tok, s))
    {
        error("Expected '%s'", s);
    }
    return tok->next;
}

int get_number(Token *tok)
{
    if (tok->kind != TK_NUM)
    {
        error("Expected a number");
    }
    return tok->val;
}

bool starts_with(char *p, char *q)
{
    return strncmp(p, q, strlen(q)) == 0;
}

// Read a punctuator token from p and returns its length.
int read_punct(char *p)
{
    if (starts_with(p, "==") || starts_with(p, "!=") || starts_with(p, "<=") || starts_with(p, ">="))
    {
        return 2;
    }

    return ispunct(*p) ? 1 : 0;
}

Token *tokenize(char *p)
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

        // Variable
        if ('a' <= *p && *p <= 'z')
        {
            cur->next = new_token(TK_IDENT, p, p + 1);
            cur->len = 1;
            cur = cur->next;
            p++;
            continue;
        }

        // Punctuators
        int punct_len = read_punct(p);
        if (punct_len > 0)
        {
            cur->next = new_token(TK_PUNCT, p, p + punct_len);
            cur = cur->next;
            p += punct_len;
            continue;
        }

        error("invalid token");
    }

    cur->next = new_token(TK_EOF, p, p);
    return head.next;
}