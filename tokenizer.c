#include "nerocc.h"

// Input filename
char *current_filename;

char *current_input;

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
        error_tok(tok, "Expected '%s'", s);
    }
    return tok->next;
}

long get_number(Token *tok)
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
    if (starts_with(p, ">>=") || starts_with(p, "<<="))
    {
        return 3;
    }

    if (starts_with(p, "==") || starts_with(p, "!=") || starts_with(p, "<=") || starts_with(p, ">=") || starts_with(p, "->") || starts_with(p, "+=") || starts_with(p, "-=") || starts_with(p, "*=") || starts_with(p, "/=") || starts_with(p, "++") || starts_with(p, "--") || starts_with(p, "%=") || starts_with(p, "&=") || starts_with(p, "|=") || starts_with(p, "^=") || starts_with(p, "&&") || starts_with(p, "||") || starts_with(p, "<<") || starts_with(p, ">>"))
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

Token *tokenize_file(char *filename)
{
    return tokenize(filename, read_file(filename));
}

Token *tokenize(char *filename, char *p)
{
    current_filename = filename;
    current_input = p;
    Token head = {};
    Token *cur = &head;

    while (*p)
    {
        // Skip line comments.
        if (starts_with(p, "//"))
        {
            p += 2;
            while (*p != '\n')
            {
                p++;
            }
            continue;
        }

        // Skip block comments.
        if (starts_with(p, "/*"))
        {
            char *q = strstr(p + 1, "*/");
            if (!q)
            {
                error("Unclosed block comment.");
            }
            p = q + 2;
            continue;
        }
        // skip white spaces
        if (isspace(*p))
        {
            p++;
            continue;
        }

        // Numbers
        if (isdigit(*p))
        {
            cur->next = read_int_literal(p);
            cur = cur->next;
            p += cur->len;
            continue;
        }

        // String literal
        if (*p == '"')
        {
            cur->next = read_string_literal(p);
            cur = cur->next;
            p += cur->len;
            continue;
        }

        // Character literal
        if (*p == '\'')
        {
            cur->next = read_char_literal(p);
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
    add_line_numbers(head.next);
    convert_keyword(head.next);
    return head.next;
}

char *read_file(char *filename)
{
    FILE *fp;

    if (strcmp(filename, "-") == 0)
    {
        // By convention, read from stdin if a given filename is "-".
        fp = stdin;
    }
    else
    {
        fp = fopen(filename, "r");
        if (!fp)
        {
            error("Cannot open %s: %s", filename, strerror(errno));
        }
    }

    char *buf;
    size_t buflen;
    FILE *out = open_memstream(&buf, &buflen);

    for (;;)
    {
        char buf2[4096];
        int n = fread(buf2, 1, sizeof(buf2), fp);
        if (n == 0)
        {
            break;
        }

        fwrite(buf2, 1, n, out);
    }

    if (fp != stdin)
    {
        fclose(fp);
    }

    fflush(out);
    if (buflen == 0 || buf[buflen - 1] != '\n')
    {
        fputc('\n', out);
    }
    fputc('\0', out);
    fclose(out);
    return buf;
}

bool is_keyword(Token *tok)
{
    return equal(tok, "return") || equal(tok, "if") || equal(tok, "else") || equal(tok, "for") || equal(tok, "while") || equal(tok, "char") || equal(tok, "int") || equal(tok, "sizeof") || equal(tok, "struct") || equal(tok, "union") || equal(tok, "short") || equal(tok, "long") || equal(tok, "void") || equal(tok, "typedef") || equal(tok, "_Bool") || equal(tok, "enum") || equal(tok, "static") || equal(tok, "goto") || equal(tok, "break") || equal(tok, "continue") || equal(tok, "switch") || equal(tok, "case") || equal(tok, "default");
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

void add_line_numbers(Token *tok)
{
    char *p = current_input;
    int n = 1;

    do
    {
        if (p == tok->loc)
        {
            tok->line_no = n;
            tok = tok->next;
        }
        if (*p == '\n')
        {
            n++;
        }
    } while (*p++);
}

Token *read_int_literal(char *start)
{
    char *p = start;
    int base = 10;
    if (!strncasecmp(p, "0x", 2) && isalnum(p[2]))
    {
        p += 2;
        base = 16;
    }
    else if (!strncasecmp(p, "0b", 2) && isalnum(p[2]))
    {
        p += 2;
        base = 2;
    }
    else if (*p == '0')
    {
        base = 8;
    }

    long val = strtoul(p, &p, base);
    if (isalnum(*p))
    {
        error_at(p, "invalid digit");
    }

    Token *tok = new_token(TK_NUM, start, p);
    tok->val = val;
    return tok;
}

Token *read_char_literal(char *start)
{
    char *p = start + 1;
    if (*p == '\0')
    {
        error_at(start, "unclosed char literal");
    }

    char c;
    if (*p == '\\')
    {
        c = read_escaped_char(&p, p + 1);
    }
    else
    {
        c = *p++;
    }

    char *end = strchr(p, '\'');
    if (!end)
    {
        error_at(p, "unclosed char literal");
    }

    Token *tok = new_token(TK_NUM, start, end + 1);
    tok->val = c;
    return tok;
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