#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>

// Common util methods
void error(char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

// Tokenizer
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

static bool starts_with(char *p, char *q)
{
    return strncmp(p, q, strlen(q)) == 0;
}

// Read a punctuator token from p and returns its length.
static int read_punct(char *p)
{
    if (starts_with(p, "==") || starts_with(p, "!=") || starts_with(p, "<=") || starts_with(p, ">="))
    {
        return 2;
    }

    return ispunct(*p) ? 1 : 0;
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

// Parser
typedef enum
{
    ND_ADD, // +
    ND_SUB, // -
    ND_MUL, // *
    ND_DIV, // /
    ND_NEG, // unary -
    ND_EQ,  // equal ==
    ND_NE,  // not equal !=
    ND_LT,  // less than <
    ND_LE,  // less than or equal to <=
    ND_NUM, // integer
} NodeKind;

typedef struct Node Node;

struct Node
{
    NodeKind kind;
    Node *lhs;
    Node *rhs;
    int val;
};

Node *new_node(NodeKind kind)
{
    Node *node = calloc(1, sizeof(Node));
    node->kind = kind;
    return node;
}

// binary operator +, -, *, /
Node *new_binary(NodeKind kind, Node *lhs, Node *rhs)
{
    Node *node = new_node(kind);
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

Node *new_unary(NodeKind kind, Node *expr)
{
    Node *node = new_node(kind);
    node->lhs = expr;
    return node;
}

// integer
Node *new_num(int val)
{
    Node *node = new_node(ND_NUM);
    node->val = val;
    return node;
}

/* Grammar
 * expr         = equality
 * equality     = relational ("==" relational | "!=" relational)*
 * relational   = add ("<" add | "<=" add | ">" add | ">=" add)*
 * add          = mul ("+" mul | "-" mul)*
 * mul          = unary ("*" unary | "/" unary)*
 * unary        = ("+" | "-")? unary | primary
 * primary      = num | "(" expr ")"
 */

static void print_node(Token *cur, const char *func)
{
    printf("# %s: %s\n", func, cur->loc);
}

static Node *expr(Token **cur, Token *tok);
static Node *equality(Token **cur, Token *tok);
static Node *relational(Token **cur, Token *tok);
static Node *add(Token **cur, Token *tok);
static Node *mul(Token **cur, Token *tok);
static Node *unary(Token **cur, Token *tok);
static Node *primary(Token **cur, Token *tok);

// Steps
// 1. Parse left hand side: mul0
// 2. While there is a valid operator, make current node to be lhs and add it as rhs
// 3. Keep applying the operation
// 4. Once all parsing is done, move pointer that tracks the current token.
//            +/-
//             |
//           -----
//          |     |
//         +/-    mul2
//          |
//        -----
//       |     |
//      mul0    mul1

static Node *expr(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    return equality(cur, tok);
}

static Node *equality(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    Node *node = relational(&tok, tok);

    for (;;)
    {
        if (equal(tok, "=="))
        {
            node = new_binary(ND_EQ, node, relational(&tok, tok->next));
            continue;
        }
        if (equal(tok, "!="))
        {
            node = new_binary(ND_NE, node, relational(&tok, tok->next));
            continue;
        }
        *cur = tok;
        return node;
    }
}

static Node *relational(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    Node *node = add(&tok, tok);

    for (;;)
    {
        if (equal(tok, "<"))
        {
            node = new_binary(ND_LT, node, add(&tok, tok->next));
            continue;
        }
        if (equal(tok, "<="))
        {
            node = new_binary(ND_LE, node, add(&tok, tok->next));
            continue;
        }
        if (equal(tok, ">"))
        {
            node = new_binary(ND_LT, add(&tok, tok->next), node);
            continue;
        }
        if (equal(tok, ">="))
        {
            node = new_binary(ND_LE, add(&tok, tok->next), node);
            continue;
        }
        *cur = tok;
        return node;
    }
}

static Node *add(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    Node *node = mul(&tok, tok);

    for (;;)
    {
        if (equal(tok, "+"))
        {
            node = new_binary(ND_ADD, node, mul(&tok, tok->next));
            continue;
        }
        if (equal(tok, "-"))
        {
            node = new_binary(ND_SUB, node, mul(&tok, tok->next));
            continue;
        }
        *cur = tok;
        return node;
    }
}

static Node *mul(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    Node *node = unary(&tok, tok);

    for (;;)
    {
        if (equal(tok, "*"))
        {
            node = new_binary(ND_MUL, node, unary(&tok, tok->next));
            continue;
        }
        if (equal(tok, "/"))
        {
            node = new_binary(ND_DIV, node, unary(&tok, tok->next));
            continue;
        }
        *cur = tok;
        return node;
    }
}

// Converts +primary, -primary to be 0 + primary, 0 - primary
static Node *unary(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    if (equal(tok, "+"))
    {
        Node *node = unary(&tok, tok->next);
        *cur = tok;
        return node;
    }

    if (equal(tok, "-"))
    {
        Node *node = unary(&tok, tok->next);
        *cur = tok;
        return new_unary(ND_NEG, node);
    }

    Node *node = primary(&tok, tok);
    *cur = tok;
    return node;
}

static Node *primary(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    if (tok->kind == TK_NUM)
    {
        Node *node = new_num(tok->val);
        *cur = tok->next;
        return node;
    }

    if (equal(tok, "("))
    {
        Node *node = expr(&tok, tok->next);
        *cur = skip(tok, ")");
        return node;
    }

    error("Expected an expression");
}

// Code Generator

// Generated Code is based on Stack machine.
// For example, add operator works as if take 2 inputs from stack top by popping 2 values,
// then add them and push the result back on top of stack.
// Add, Sub, Mul, Div can be done in the same way, pop 2 values from stack top, compute result and push it on top of stack.

//
// pop <reg>: pop top of stack into <reg>
// push <reg>: push <reg> value on top of stack
static void gen_code(Node *node)
{
    switch (node->kind)
    {
    case ND_NUM:
        printf("    push %d\n", node->val);
        return;
    case ND_NEG:
        gen_code(node->lhs);
        printf("    pop rax\n");
        printf("    neg rax\n");
        printf("    push rax\n");
        return;
    }

    // First compute lhs and rhs, top 2 values on stack shuold be rhs, lhs, ...
    gen_code(node->lhs);
    gen_code(node->rhs);

    printf("    pop rdi\n");
    printf("    pop rax\n");

    switch (node->kind)
    {
    case ND_ADD:
        printf("    add rax, rdi\n");
        break;
    case ND_SUB:
        printf("    sub rax, rdi\n");
        break;
    case ND_MUL:
        printf("    imul rax, rdi\n");
        break;
    case ND_DIV:
        printf("    cqo\n");
        printf("    idiv rdi\n");
        break;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
        // cmp s2, s1
        // set flags based on s1 - s2
        printf("    cmp rax, rdi\n");
        if (node->kind == ND_EQ)
        {
            printf("    sete al\n");
        }
        else if (node->kind == ND_NE)
        {
            printf("    setne al\n");
        }
        else if (node->kind == ND_LT)
        {
            printf("    setl al\n");
        }
        else if (node->kind == ND_LE)
        {
            printf("    setle al\n");
        }
        printf("    movzb rax, al\n");
    }

    printf("    push rax\n");
}

int main(int argc, char **argv)
{
    if (argc != 2)
    {
        fprintf(stderr, "%s: invalid number of arguments\n", argv[0]);
        return 1;
    }

    Token *tok = tokenize(argv[1]);
    Node *node = expr(&tok, tok);

    if (tok->kind != TK_EOF)
    {
        error("Extra token exists");
    }

    // Code generation
    printf(".intel_syntax noprefix\n");
    printf(".globl main\n");
    printf("main:\n");

    // Traverse AST to emit assembly.
    gen_code(node);

    printf("    pop rax\n");
    printf("    ret\n");
    return 0;
}