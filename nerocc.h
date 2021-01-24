#ifndef NEROCC_H
#define NEROCC_H

#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#define LOG 0

// Common definitions
typedef struct Type Type;
typedef struct Token Token;
typedef struct Node Node;
typedef struct Obj Obj;
typedef struct Member Member;
typedef struct VarScope VarScope;
typedef struct Scope Scope;
typedef struct TagScope TagScope;
typedef struct VarAttr VarAttr;

// Type
typedef enum
{
    TY_VOID,
    TY_BOOL,
    TY_CHAR,
    TY_SHORT,
    TY_INT,
    TY_LONG,
    TY_ENUM,
    TY_PTR,
    TY_FUNC,
    TY_ARRAY,
    TY_STRUCT,
    TY_UNION,
} TypeKind;

struct Type
{
    TypeKind kind;

    int size; // sizeof() value
    int align;

    Type *base;

    // Declaration
    Token *name;

    // Array
    int array_len;

    // Struct
    Member *members;

    // Function type
    Type *return_ty;
    Type *params;
    Type *next;
};

extern Type *ty_void;
extern Type *ty_bool;
extern Type *ty_char;
extern Type *ty_int;
extern Type *ty_short;
extern Type *ty_long;

Type *new_type(TypeKind kind, int size, int align);
bool is_integer(Type *ty);
Type *pointer_to(Type *base);
Type *func_type(Type *return_ty);
Type *array_of(Type *base, int len);
Type *enum_type();
void add_type(Node *node);
Type *copy_type(Type *ty);

Type *get_common_type(Type *ty1, Type *ty2);
void usual_arith_conv(Node **lhs, Node **rhs);

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
    int64_t val;
    char *loc;
    size_t len;
    Type *ty;  // Used if TK_STR
    char *str; // String literal contents including teminating '\0'

    int line_no; // Line number
};

bool consume(Token **cur, Token *tok, char *str);

Token *new_token(TokenKind kind, char *start, char *end);

bool equal(Token *tok, char *op);

// Consumes the current token if it matches 's'
Token *skip(Token *tok, char *s);

long get_number(Token *tok);

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
void add_line_numbers(Token *tok);

Token *read_char_literal(char *p);
Token *read_string_literal(char *p);
char *string_literal_end(char *p);
int read_escaped_char(char **new_pos, char *p);
int from_hex(char c);

// Input filename
extern char *current_filename;

extern char *current_input;

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
    ND_MEMBER,    // . (struct member access)
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
    ND_CAST,      // Type case
} NodeKind;

struct Node
{
    NodeKind kind; // Node kind
    Node *next;    // Next node
    Type *ty;      // Type after all evaluation.
    Token *tok;

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

    // Struct member access
    Member *member;

    // Function call
    char *funcname;
    Type *func_ty;
    Node *args;

    Obj *var; // used if kind == ND_VAR
    int64_t val;
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
    // True if it is function with body.
    bool is_definition;

    bool is_static;
    // Global variable
    char *init_data;

    // Function
    Obj *params;

    Node *body;
    Obj *locals;
    int stack_size;
};

// Scope for local, global variables or typedefs.
struct VarScope
{
    VarScope *next;
    char *name;
    Obj *var;

    Type *type_def;
    Type *enum_ty;
    int enum_val;
};

struct Scope
{
    Scope *next;

    // C has two block scopes, one is for variable/typedefs and the other is for struct/union/enum tag.
    VarScope *vars;
    TagScope *tags;
};

// Struct member
struct Member
{
    Member *next;
    Type *ty;
    Token *name;
    int offset;
};

// Scope for struct, union or enum tags.
struct TagScope
{
    TagScope *next;
    char *name;
    Type *ty;
};

// Variable attributes such as typedef or extern.
struct VarAttr
{
    bool is_typedef;
    bool is_static;
};

// Functions for node creation
Node *new_node(NodeKind kind, Token *tok);

// binary operator +, -, *, /
Node *new_binary(NodeKind kind, Node *lhs, Node *rhs, Token *tok);

Node *new_unary(NodeKind kind, Node *expr, Token *tok);
Node *new_cast(Node *expr, Type *ty);

// integer
Node *new_num(int64_t val, Token *tok);

Node *new_long(int64_t val, Token *tok);

// identifier
Node *new_var_node(Obj *var, Token *tok);

// Functions for variable
Obj *new_var(char *name, Type *ty);
Obj *new_lvar(char *name, Type *ty);
Obj *new_gvar(char *name, Type *ty);
VarScope *find_var(Token *tok);
Type *find_typedef(Token *tok);

Type *find_tag(Token *tok);

void create_param_lvars(Type *param);

static char *new_unique_name();
static Obj *new_anon_gvar(Type *ty);
static Obj *new_string_literal(char *p, Type *ty);
char *get_ident(Token *tok);

/* Grammar
 * program              = (function-definition | global-variable)*
 * declspec             = ("void" | "_Bool" | "char" | "int" | "short" | "long" | "typedef" | "static" | struct-decl | union-decl)+
 * declarator           = "*"* ("(" ident ")" | "(" declarator ")" | ident) type-suffix
 * declaration          = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
 * function-definition  = declspec declarator "{" compound-stmt
 * struct-decl          = "struct" struct-or-union-decl
 * union-decl           = "union" struct-or-union-decl
 * enum-specifier       = ident? "{" enum-list? "}" | ident ("{" enum-list? "}")?
 * enum-list            = ident ("=" num)? ("," ident ("=" num)?)*
 * struct-or-union-decl = ident? ("{" struct-members)?
 * struct-members       = (declspec declarator ("," declarator)* ";")*
 * stmt                 = "return" expr ";" 
 *                      | "{" compound-stmt
 *                      | "if" "(" expr ")" stmt ("else" stmt)?
 *                      | "for" "(" expr-stmt expr? ";" expr? ")" stmt
 *                      | "while" "(" expr ")" stmt
 *                      | exprexpr-stmt
 * compound-stmt        = ("typedef" | declaration | stmt)* "}"
 * expr-stmt            = expr? ";"
 * expr                 = assign
 * assign               = equality ("=" assign)?
 * equality             = relational ("==" relational | "!=" relational)*
 * relational           = add ("<" add | "<=" add | ">" add | ">=" add)*
 * add                  = mul ("+" mul | "-" mul)*
 * mul                  = cast ("*" cast | "/" cast)*
 * cast                 = "(" type-name ")" cast | unary
 * unary                = ("+" | "-" | "*" | "&")? cast | postfix
 * postfix              = primary ("[" expr "]" | "." ident | "->" ident)*
 * primary              = "(" "{" stmt+ "}" ")"
 *                      | "(" expr ")"
 *                      | "sizeof" "(" type-name ")"
 *                      | "sizeof" unary
 *                      | ident func-args?
 *                      | str
 *                      | num 
 * type-suffix          = "(" func-params
 *                      | "[" num "]" type-suffix
 *                      | ɛ
 * type-name            = declspec abstract-declarator
 * abstract-declarator  = "*"* ("(" abstract-declarator ")")? type-suffix
 * func-args            = "(" (assign ("," assign)*)? ")"
 * func-params          = (param ("," param)*)? ")"
 * param                = declspec delarator
 */

void print_node(Token *cur, const char *func);

// Entry point for parsing
Obj *parse(Token *tok);

Token *function(Token *tok, Type *base_ty, VarAttr *attr);
Type *declspec(Token **cur, Token *tok, VarAttr *attr);
Type *declarator(Token **cur, Token *tok, Type *ty);
Node *declaration(Token **cur, Token *tok, Type *base_ty);
Type *enum_specifier(Token **cur, Token *tok);

Type *struct_decl(Token **cur, Token *tok);
Type *union_decl(Token **cur, Token *tok);
Type *struct_or_union_decl(Token **cur, Token *tok);
void struct_members(Token **cur, Token *tok, Type *ty);
Node *struct_ref(Node *lhs, Token *tok);
Member *get_struct_member(Type *ty, Token *tok);
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
Node *cast(Token **cur, Token *tok);
Node *unary(Token **cur, Token *tok);
Node *postfix(Token **cur, Token *tok);
Node *primary(Token **cur, Token *tok);
Type *abstract_declarator(Token **cur, Token *tok, Type *ty);
Type *typename(Token **cur, Token *tok);
Type *type_suffix(Token **cur, Token *tok, Type *ty);
Type *func_params(Token **cur, Token *tok, Type *ty);

Node *funcall(Token **cur, Token *tok);

Node *add_with_type(Node *lhs, Node *rhs, Token *tok);
Node *sub_with_type(Node *lhs, Node *rhs, Token *tok);

bool is_function(Token *tok);
bool is_typename(Token *tok);
Token *parse_typedef(Token *tok, Type *base_ty);

void enter_scope();
void leave_scope();
VarScope *push_scope(char *name);

// Scope for struct or union tag
void push_tag_scope(Token *tok, Type *ty);

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

void load(Type *ty);
void store(Type *ty);
void store_gp(int r, int offset, int sz);
void cast_type(Type *from, Type *to);
int getTypeId(Type *ty);

void cmp_zero(Type *ty);

// Common util methods
void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);
#endif