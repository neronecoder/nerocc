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

Var *new_var(char *name, Type *ty)
{
    Var *var = calloc(1, sizeof(Var));
    var->name = name;
    var->next = locals;
    var->ty = ty;
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

char *get_ident(Token *tok)
{
    return strndup(tok->loc, tok->len);
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

Type *declspec(Token **cur, Token *tok)
{
    *cur = skip(tok, "int");
    return ty_int;
}

Type *declarator(Token **cur, Token *tok, Type *ty)
{
    while (consume(&tok, tok, "*"))
    {
        ty = pointer_to(ty);
    }

    if (tok->kind != TK_IDENT)
    {
        error("Expected a variable name.");
    }

    ty->name = tok;
    *cur = tok->next;
    return ty;
}

Node *declaration(Token **cur, Token *tok)
{
    Type *base_ty = declspec(&tok, tok);

    Node head = {};
    Node *cur_node = &head;

    int cnt = 0;
    while (!equal(tok, ";"))
    {
        if (cnt++ > 0)
        {
            tok = skip(tok, ",");
        }

        Type *ty = declarator(&tok, tok, base_ty);
        Var *var = new_var(get_ident(ty->name), ty);

        if (!equal(tok, "="))
        {
            continue;
        }

        Node *lhs = new_var_node(var);
        Node *rhs = assign(&tok, tok->next);
        Node *node = new_binary(ND_ASSIGN, lhs, rhs);

        cur_node->next = new_unary(ND_EXPR_STMT, node);
        cur_node = cur_node->next;
    }

    Node *node = new_node(ND_BLOCK);
    node->body = head.next;
    *cur = tok->next;
    return node;
}

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

    if (equal(tok, "for"))
    {
        Node *node = new_node(ND_FOR);
        tok = skip(tok->next, "(");
        node->init = expr_stmt(&tok, tok);
        if (!equal(tok, ";"))
        {
            node->cond = expr(&tok, tok);
        }
        tok = skip(tok, ";");
        if (!equal(tok, ")"))
        {
            node->inc = expr(&tok, tok);
        }
        tok = skip(tok, ")");
        node->then = stmt(cur, tok);
        return node;
    }

    if (equal(tok, "while"))
    {
        Node *node = new_node(ND_WHILE);
        tok = skip(tok->next, "(");
        node->cond = expr(&tok, tok);
        tok = skip(tok, ")");

        node->then = stmt(cur, tok);
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
        if (equal(tok, "int"))
        {
            t->next = declaration(&tok, tok);
        }
        else
        {
            t->next = stmt(&tok, tok);
        }
        t = t->next;
        add_type(t);
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
            node = add_with_type(node, mul(&tok, tok->next));
            continue;
        }
        if (equal(tok, "-"))
        {
            node = sub_with_type(node, mul(&tok, tok->next));
            continue;
        }
        *cur = tok;
        return node;
    }
}

Node *add_with_type(Node *lhs, Node *rhs)
{
    add_type(lhs);
    add_type(rhs);

    // num + num
    if (is_integer(lhs->ty) && is_integer(rhs->ty))
    {
        return new_binary(ND_ADD, lhs, rhs);
    }

    // We don't allow ptr + ptr op
    if (lhs->ty->base && rhs->ty->base)
    {
        error("Invalid operands.");
    }

    // Canonicalize num + ptr to ptr + num.
    if (!lhs->ty->base && rhs->ty->base)
    {
        Node *tmp = lhs;
        lhs = rhs;
        rhs = tmp;
    }

    // multiply by 8
    rhs = new_binary(ND_MUL, rhs, new_num(8));
    return new_binary(ND_ADD, lhs, rhs);
}

Node *sub_with_type(Node *lhs, Node *rhs)
{
    add_type(lhs);
    add_type(rhs);

    // num - num
    if (is_integer(lhs->ty) && is_integer(rhs->ty))
    {
        return new_binary(ND_SUB, lhs, rhs);
    }

    // We don't allow num - ptr op
    if (is_integer(lhs->ty) && rhs->ty->base)
    {
        error("Invalid operands.");
    }

    // ptr - num
    if (lhs->ty->base && is_integer(rhs->ty))
    {
        rhs = new_binary(ND_MUL, rhs, new_num(8));
        add_type(rhs);
        Node *node = new_binary(ND_SUB, lhs, rhs);
        node->ty = lhs->ty;
        return node;
    }

    // ptr - ptr returns how many elements between them
    if (lhs->ty->base && rhs->ty->base)
    {
        Node *node = new_binary(ND_SUB, lhs, rhs);
        node->ty = ty_int;
        return new_binary(ND_DIV, node, new_num(8));
    }
    rhs = new_binary(ND_MUL, rhs, new_num(8));
    return new_binary(ND_ADD, lhs, rhs);
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

    if (equal(tok, "*"))
    {
        Node *node = unary(&tok, tok->next);
        *cur = tok;
        return new_unary(ND_DEREF, node);
    }

    if (equal(tok, "&"))
    {
        Node *node = unary(&tok, tok->next);
        *cur = tok;
        return new_unary(ND_ADDR, node);
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
            error("Undefined variable '%s'", strndup(tok->loc, tok->len));
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