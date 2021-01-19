#ifndef NEROCC_H
#define NEROCC_H

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>

#define LOG 0

// Common util methods
void error(char *fmt, ...);

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

Token *new_token(TokenKind kind, char *start, char *end);

bool equal(Token *tok, char *op);

// Consumes the current token if it matches 's'
Token *skip(Token *tok, char *s);

int get_number(Token *tok);

bool starts_with(char *p, char *q);

// Read a punctuator token from p and returns its length.
int read_punct(char *p);

Token *tokenize(char *p);

// Parser
typedef enum
{
    ND_ADD,       // +
    ND_SUB,       // -
    ND_MUL,       // *
    ND_DIV,       // /
    ND_NEG,       // unary -
    ND_EQ,        // equal ==
    ND_NE,        // not equal !=
    ND_LT,        // less than <
    ND_LE,        // less than or equal to <=
    ND_EXPR_STMT, // Expression statement #follow not sure what this is for
    ND_NUM,       // integer
} NodeKind;

typedef struct Node Node;

struct Node
{
    NodeKind kind;
    Node *next;
    Node *lhs;
    Node *rhs;
    int val;
};

Node *new_node(NodeKind kind);

// binary operator +, -, *, /
Node *new_binary(NodeKind kind, Node *lhs, Node *rhs);

Node *new_unary(NodeKind kind, Node *expr);

// integer
Node *new_num(int val);

/* Grammar
 * program      = stmt*
 * stmt         = expr-stmt
 * expr-stmt    = expr;
 * expr         = equality
 * equality     = relational ("==" relational | "!=" relational)*
 * relational   = add ("<" add | "<=" add | ">" add | ">=" add)*
 * add          = mul ("+" mul | "-" mul)*
 * mul          = unary ("*" unary | "/" unary)*
 * unary        = ("+" | "-")? unary | primary
 * primary      = num | "(" expr ")"
 */

void print_node(Token *cur, const char *func);

// Entry point for parsing
Node *parse(Token *tok);

Node *stmt(Token **cur, Token *tok);
Node *expr_stmt(Token **cur, Token *tok);
Node *expr(Token **cur, Token *tok);
Node *equality(Token **cur, Token *tok);
Node *relational(Token **cur, Token *tok);
Node *add(Token **cur, Token *tok);
Node *mul(Token **cur, Token *tok);
Node *unary(Token **cur, Token *tok);
Node *primary(Token **cur, Token *tok);

// Code Generator

// Generated Code is based on Stack machine.
// For example, add operator works as if take 2 inputs from stack top by popping 2 values,
// then add them and push the result back on top of stack.
// Add, Sub, Mul, Div can be done in the same way, pop 2 values from stack top, compute result and push it on top of stack.

// Assembly codes
// pop <reg>: pop top of stack into <reg>
// push <reg>: push <reg> value on top of stack

void gen_code(Node *node);

void gen_stmt(Node *node);
void gen_expr(Node *node);
#endif