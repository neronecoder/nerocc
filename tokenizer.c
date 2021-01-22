#include "nerocc.h"

bool consume(Token **cur, Token *tok, char *str)
{
    if (equal(tok, str))
    {
        *cur = tok->next;
        return true;
    }
    *cur = tok;
    return false;
}

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

int read_ident(char *p)
{
    if (!is_valid_ident1(*p))
    {
        return 0;
    }
    int cnt = 1;
    while (is_valid_ident2(*(p + cnt)))
    {
        cnt++;
    }
    return cnt;
}

// check if the first character of identifier is valid
bool is_valid_ident1(char c)
{
    return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
}

// check if the second, ... characters of identifier are valid
bool is_valid_ident2(char c)
{
    return is_valid_ident1(c) || ('0' <= c && c <= '9');
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

        if (*p == '"')
        {
            cur->next = read_string_literal(p);
            cur = cur->next;
            p += cur->len;
            continue;
        }

        // Variable
        int variable_len = read_ident(p);
        if (variable_len > 0)
        {
            cur->next = new_token(TK_IDENT, p, p + variable_len);
            cur = cur->next;
            p += variable_len;
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
    convert_keyword(head.next);
    return head.next;
}

bool is_keyword(Token *tok)
{
    return equal(tok, "return") || equal(tok, "if") || equal(tok, "else") || equal(tok, "for") || equal(tok, "while") || equal(tok, "char") || equal(tok, "int") || equal(tok, "sizeof");
}

void convert_keyword(Token *tok)
{
    for (Token *cur = tok; cur->kind != TK_EOF; cur = cur->next)
    {
        if (is_keyword(cur))
        {
            cur->kind = TK_KEYWORD;
        }
    }
}

Token *read_string_literal(char *start)
{
    char *end = string_literal_end(start + 1);
    char *buf = calloc(1, end - start);
    int len = 0;
    for (char *p = start + 1; p < end;)
    {
        if (*p == '\\')
        {
            buf[len++] = read_escaped_char(&p, p + 1);
        }
        else
        {
            buf[len++] = *p++;
        }
    }

    Token *tok = new_token(TK_STR, start, end + 1);
    tok->ty = array_of(ty_char, len + 1);
    tok->str = buf;
    return tok;
}

char *string_literal_end(char *p)
{
    char *start = p;
    for (; *p != '"'; p++)
    {
        if (*p == '\n' || *p == '\0')
        {
            error("Unclosed string literal");
        }
        if (*p == '\\')
        {
            p++;
        }
    }
    return p;
}

int read_escaped_char(char **new_pos, char *p)
{
    if ('0' <= *p && *p <= '7')
    {
        // Read an octal number.
        int c = *p++ - '0';
        if ('0' <= *p && *p <= '7')
        {
            c = (c << 3) + (*p++ - '0');
            if ('0' <= *p && *p <= '7')
            {
                c = (c << 3) + (*p++ - '0');
            }
        }
        *new_pos = p;
        return c;
    }

    if (*p == 'x')
    {
        // Read a hexadecimal number.
        p++;
        if (!isxdigit(*p))
        {
            error("Invalid hex escape sequence.");
        }

        int c = 0;
        for (; isxdigit(*p); p++)
        {
            c = (c << 4) + from_hex(*p);
        }
        *new_pos = p;
        return c;
    }

    *new_pos = p + 1;
    switch (*p)
    {
    case 'a':
        return '\a';
    case 'b':
        return '\b';
    case 't':
        return '\t';
    case 'n':
        return '\n';
    case 'v':
        return '\v';
    case 'f':
        return '\f';
    case 'r':
        return '\r';
    case 'e':
        return 27;
    default:
        return *p;
    }
}

int from_hex(char c)
{
    if ('0' <= c && c <= '9')
    {
        return c - '0';
    }
    if ('a' <= c && c <= 'f')
    {
        return c - 'a' + 10;
    }
    return c - 'A' + 10;
}