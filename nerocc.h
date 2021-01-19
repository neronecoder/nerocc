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
    TK_IDENT, // Identifiers
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
    ND_ASSIGN,    // =
    ND_EXPR_STMT, // Expression statement #follow not sure what this is for
    ND_VAR,       //Variable
    ND_NUM,       // integer
} NodeKind;

typedef struct Node Node;

struct Node
{
    NodeKind kind;
    Node *next;
    Node *lhs;
    Node *rhs;
    char name; // used if kind == ND_VAR
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
 * expr         = assign
 * assign       = equality ("=" assign)?
 * equality     = relational ("==" relational | "!=" relational)*
 * relational   = add ("<" add | "<=" add | ">" add | ">=" add)*
 * add          = mul ("+" mul | "-" mul)*
 * mul          = unary ("*" unary | "/" unary)*
 * unary        = ("+" | "-")? unary | primary
 * primary      = num | ident | "(" expr ")"
 */

void print_node(Token *cur, const char *func);

// Entry point for parsing
Node *parse(Token *tok);

Node *stmt(Token **cur, Token *tok);
Node *expr_stmt(Token **cur, Token *tok);
Node *expr(Token **cur, Token *tok);
Node *assign(Token **cur, Token *tok);
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

// Data Movement
// - mov S, D: move source to destination
// - push S: push source onto stack
// - pop D: pop top of stack into destination

// Arithmetic op
// Unary Op
// - inc D: Increment by 1
// - dec D: Decrement by 1
// - neg D: Arithmetic negation
// - not D: Bitwise complement
// Binary Op
// - imul S, D: Multiply destination by source
// pop <reg>: pop top of stack into <reg> => read value from rsp's memory address, then increment rsp
// push <reg>: push <reg> value on top of stack => decrement rsp, then write value to rsp's memory address

// Comparison and test
// - cmp S2, S1: Set condition codes according to S1 - S2
// - test S2, S1: Set condition codes according to S1 & S2

// Access condition codes
// - sete/setz D: Set if equal/zero
// - setne/setnz D: Set if not equal/non-zero
// - setg/setnle D: Set if greater (signed)
// - setge/setnl D: Set if greater or equal (signed)
// - setl/setnge D: Set if less (signed)
// - setle/setng D: Set if less or equal (signed)


// Function prologue & Function epilogue
/*
 * Function prologue code
 * ```
 * push rbp # Push rbp of caller onto stack
 * mov rsp, rbp # Copy rsp into rbp
 * ```
 * After prologue, we use rbp as a base memory address
 * 
 * Function epilogue code
 * ```
 * mov %rbp, %rsp # Move rsp value back from rbp
 * pop %rbp # Pop rbp out from stack
 * ```
 * 
 */ 
void gen_code(Node *node);

void gen_stmt(Node *node);
void gen_expr(Node *node);
void gen_lval(Node *node);
#endif