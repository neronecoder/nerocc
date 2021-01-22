#ifndef NEROCC_H
#define NEROCC_H

#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>

#define LOG 0

// Common util methods
void error(char *fmt, ...);

// Common definitions
typedef struct Type Type;
typedef struct Token Token;
typedef struct Node Node;
typedef struct Obj Obj;
typedef struct VarScope VarScope;
typedef struct Scope Scope;

// Type
typedef enum
{
    TY_CHAR,
    TY_INT,
    TY_PTR,
    TY_FUNC,
    TY_ARRAY,
} TypeKind;

struct Type
{
    TypeKind kind;

    int size; // sizeof() value

    Type *base;

    // Declaration
    Token *name;

    // Array
    int array_len;

    // Function type
    Type *return_ty;
    Type *params;
    Type *next;
};

extern Type *ty_char;
extern Type *ty_int;

bool is_integer(Type *ty);
Type *pointer_to(Type *base);
Type *func_type(Type *return_ty);
Type *array_of(Type *base, int len);
void add_type(Node *node);
Type *copy_type(Type *ty);

// Tokenizer
typedef enum
{
    TK_IDENT,   // Identifiers
    TK_KEYWORD, // Reserved keywords
    TK_PUNCT,   // Punctuators
    TK_STR,     // String literals
    TK_NUM,     // Numeric literals
    TK_EOF,     // End-of-file markers
} TokenKind;

struct Token
{
    TokenKind kind;
    Token *next;
    int val;
    char *loc;
    size_t len;
    Type *ty;  // Used if TK_STR
    char *str; // String literal contents including teminating '\0'
};

bool consume(Token **cur, Token *tok, char *str);

Token *new_token(TokenKind kind, char *start, char *end);

bool equal(Token *tok, char *op);

// Consumes the current token if it matches 's'
Token *skip(Token *tok, char *s);

int get_number(Token *tok);

bool starts_with(char *p, char *q);

// Read a punctuator token from p and returns its length.
int read_punct(char *p);

// Read an identifier token from p and returns its length.
int read_ident(char *p);

// check if the first character of identifier is valid
bool is_valid_ident1(char c);

// check if the second, ... characters of identifier are valid
bool is_valid_ident2(char c);

Token *tokenize_file(char *filename);
Token *tokenize(char *filename, char *p);
char *read_file(char *filename);

void convert_keyword(Token *tok);

Token *read_string_literal(char *p);
char *string_literal_end(char *p);
int read_escaped_char(char **new_pos, char *p);
int from_hex(char c);

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
    ND_ADDR,      // unary &
    ND_DEREF,     // unary *
    ND_RETURN,    // return
    ND_EXPR_STMT, // Expression statement # follow not sure what this is for
    ND_STMT_EXPR, // Statement expresiion # follow not sure what this is for
    ND_BLOCK,     // block {...}
    ND_FUNCALL,   // Function call
    ND_IF,        // if
    ND_FOR,       // for
    ND_WHILE,     // while
    ND_VAR,       // Objiable
    ND_NUM,       // integer
} NodeKind;

struct Node
{
    NodeKind kind; // Node kind
    Node *next;    // Next node
    Type *ty;      // Type after all evaluation.

    Node *lhs;
    Node *rhs;

    // "if" statement
    Node *cond;
    Node *then;
    Node *els;

    // "for" statement
    Node *init;
    Node *inc;

    // Block or statement expression
    Node *body;

    // Function call
    char *funcname;
    Node *args;

    Obj *var; // used if kind == ND_VAR
    int val;
};

// Variable or function
struct Obj
{
    Obj *next;
    char *name;
    Type *ty;
    bool is_local; // local or global/function

    // local variable
    int offset; // offset from rbp

    // global variable or function
    bool is_function;

    // Global variable
    char *init_data;

    // Function
    Obj *params;

    Node *body;
    Obj *locals;
    int stack_size;
};

struct VarScope
{
    VarScope *next;
    char *name;
    Obj *var;
};

struct Scope
{
    Scope *next;
    VarScope *vars;
};

// Functions for node creation
Node *new_node(NodeKind kind);

// binary operator +, -, *, /
Node *new_binary(NodeKind kind, Node *lhs, Node *rhs);

Node *new_unary(NodeKind kind, Node *expr);

// integer
Node *new_num(int val);

// identifier
Node *new_var_node(Obj *var);

// Functions for variable
Obj *new_var(char *name, Type *ty);
Obj *new_lvar(char *name, Type *ty);
Obj *new_gvar(char *name, Type *ty);
Obj *find_var(Token *tok);

void create_param_lvars(Type *param);

static char *new_unique_name();
static Obj *new_anon_gvar(Type *ty);
static Obj *new_string_literal(char *p, Type *ty);
char *get_ident(Token *tok);

/* Grammar
 * program              = (function-definition | global-variable)*
 * declspec             = "char" | "int"
 * declarator           = "*"* ident type-suffix
 * declaration          = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
 * function-definition  = declspec declarator "{" compound-stmt
 * stmt                 = "return" expr ";" 
 *                      | "{" compound-stmt
 *                      | "if" "(" expr ")" stmt ("else" stmt)?
 *                      | "for" "(" expr-stmt expr? ";" expr? ")" stmt
 *                      | "while" "(" expr ")" stmt
 *                      | exprexpr-stmt
 * compound-stmt        = (declaration | stmt)* "}"
 * expr-stmt            = expr? ";"
 * expr                 = assign
 * assign               = equality ("=" assign)?
 * equality             = relational ("==" relational | "!=" relational)*
 * relational           = add ("<" add | "<=" add | ">" add | ">=" add)*
 * add                  = mul ("+" mul | "-" mul)*
 * mul                  = unary ("*" unary | "/" unary)*
 * unary                = ("+" | "-" | "*" | "&")? unary | postfix
 * postfix              = primary ("[" expr "]")*
 * primary              = "(" "{" stmt+ "}" ")"
 *                      | "(" expr ")"
 *                      | "sizeof" unary
 *                      | ident func-args?
 *                      | str
 *                      | num 
 * type-suffix          = "(" func-params
 *                      | "[" num "]" type-suffix
 *                      | ɛ
 * func-args            = "(" (assign ("," assign)*)? ")"
 * func-params          = (param ("," param)*)? ")"
 * param                = declspec delarator
 */

void print_node(Token *cur, const char *func);

// Entry point for parsing
Obj *parse(Token *tok);

Token *function(Token *tok, Type *base_ty);
Type *declspec(Token **cur, Token *tok);
Type *declarator(Token **cur, Token *tok, Type *ty);
Node *declaration(Token **cur, Token *tok);
Token *global_variable(Token *tok, Type *base_ty);
Node *stmt(Token **cur, Token *tok);
Node *compound_stmt(Token **cur, Token *tok);
Node *expr_stmt(Token **cur, Token *tok);
Node *expr(Token **cur, Token *tok);
Node *assign(Token **cur, Token *tok);
Node *equality(Token **cur, Token *tok);
Node *relational(Token **cur, Token *tok);
Node *add(Token **cur, Token *tok);
Node *mul(Token **cur, Token *tok);
Node *unary(Token **cur, Token *tok);
Node *postfix(Token **cur, Token *tok);
Node *primary(Token **cur, Token *tok);
Type *type_suffix(Token **cur, Token *tok, Type *ty);
Type *func_params(Token **cur, Token *tok, Type *ty);
Node *func_args(Token **cur, Token *tok);

Node *add_with_type(Node *lhs, Node *rhs);
Node *sub_with_type(Node *lhs, Node *rhs);

bool is_function(Token *tok);
bool is_typename(Token *tok);

void enter_scope();
void leave_scope();
VarScope *push_scope(char *name, Obj *var);

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
void gen_code(Obj *prog, FILE *out);

void emit_data(Obj *prog);
void emit_text(Obj *prog);
void gen_stmt(Node *node);
void gen_expr(Node *node);
void gen_addr(Node *node);

void assign_lvar_offsets(Obj *prog);
int align_to(int offset, int align);
#endif