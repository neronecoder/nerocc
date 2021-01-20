#include "nerocc.h"

Var *locals;

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

Node *new_num(int val)
{
    Node *node = new_node(ND_NUM);
    node->val = val;
    return node;
}

Node *new_var_node(Var *var)
{
    Node *node = new_node(ND_VAR);
    node->var = var;
    return node;
}

Var *new_var(char *name)
{
    Var *var = calloc(1, sizeof(Var));
    var->name = name;
    var->next = locals;
    locals = var;
    return var;
}

Var *find_var(Token *tok)
{
    for (Var *var = locals; var; var = var->next)
    {
        if (strlen(var->name) == tok->len && strncmp(tok->loc, var->name, tok->len) == 0)
        {
            return var;
        }
    }
    return NULL;
}

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

void print_node(Token *cur, const char *func)
{
    if (LOG)
    {
        fprintf(stderr, "# %s: %s\n", func, cur->loc);
    }
}

Function *parse(Token *tok)
{
    print_node(tok, __FUNCTION__);
    // program will be "{" compound_stmt, i.e. "{" stmt* "}"
    tok = skip(tok, "{");

    Function *prog = calloc(1, sizeof(Function));
    prog->body = compound_stmt(&tok, tok);
    prog->locals = locals;
    return prog;
}

// Steps (Ex. add)
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

Node *stmt(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    if (equal(tok, "return"))
    {
        Node *node = new_unary(ND_RETURN, expr(&tok, tok->next));
        *cur = skip(tok, ";");
        return node;
    }

    if (equal(tok, "{"))
    {
        return compound_stmt(cur, tok->next);
    }

    if (equal(tok, "if"))
    {
        Node *node = new_node(ND_IF);
        tok = skip(tok->next, "(");
        node->cond = expr(&tok, tok);
        tok = skip(tok, ")");

        node->then = stmt(&tok, tok);

        if (equal(tok, "else"))
        {
            node->els = stmt(&tok, tok->next);
        }

        *cur = tok;
        return node;
    }
    
    return expr_stmt(cur, tok);
}

Node *compound_stmt(Token **cur, Token *tok)
{
    Node head = {};
    Node *t = &head;

    while (!equal(tok, "}"))
    {
        t->next = stmt(&tok, tok);
        t = t->next;
    }

    Node *node = new_node(ND_BLOCK);
    node->body = head.next;
    *cur = tok->next;
    return node;
}

Node *expr_stmt(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    // For null statement, i.e. just ";"
    if (equal(tok, ";"))
    {
        *cur = tok->next;
        return new_node(ND_BLOCK);
    }
    Node *node = new_unary(ND_EXPR_STMT, expr(&tok, tok));
    *cur = skip(tok, ";");
    return node;
}

Node *expr(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    return assign(cur, tok);
}

Node *assign(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    Node *node = equality(&tok, tok);
    if (equal(tok, "="))
    {
        node = new_binary(ND_ASSIGN, node, assign(&tok, tok->next));
    }
    *cur = tok;
    return node;
}

Node *equality(Token **cur, Token *tok)
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

Node *relational(Token **cur, Token *tok)
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

Node *add(Token **cur, Token *tok)
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

Node *mul(Token **cur, Token *tok)
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
Node *unary(Token **cur, Token *tok)
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

Node *primary(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    if (tok->kind == TK_NUM)
    {
        Node *node = new_num(tok->val);
        *cur = tok->next;
        return node;
    }

    if (tok->kind == TK_IDENT)
    {
        Node *node = new_node(ND_VAR);
        Var *var = find_var(tok);
        if (!var)
        {
            var = new_var(strndup(tok->loc, tok->len));
        }
        *cur = tok->next;
        return new_var_node(var);
    }

    if (equal(tok, "("))
    {
        Node *node = expr(&tok, tok->next);
        *cur = skip(tok, ")");
        return node;
    }

    error("Expected an expression");
}