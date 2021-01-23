#include "nerocc.h"

Obj *locals;
Obj *globals;

static Scope *scope = &(Scope){};

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

Node *new_var_node(Obj *var)
{
    Node *node = new_node(ND_VAR);
    node->var = var;
    return node;
}

Obj *new_lvar(char *name, Type *ty)
{
    Obj *var = new_var(name, ty);
    var->is_local = true;
    var->next = locals;
    locals = var;
    return var;
}

Obj *new_gvar(char *name, Type *ty)
{
    Obj *var = new_var(name, ty);
    var->is_local = false;
    var->next = globals;
    globals = var;
    return var;
}

Obj *new_var(char *name, Type *ty)
{
    Obj *var = calloc(1, sizeof(Obj));
    var->name = name;
    var->ty = ty;
    push_scope(name, var);
    return var;
}

Obj *find_var(Token *tok)
{
    for (Scope *sc = scope; sc; sc = sc->next)
    {
        for (VarScope *sc2 = sc->vars; sc2; sc2 = sc2->next)
        {
            if (equal(tok, sc2->name))
            {
                return sc2->var;
            }
        }
    }
    return NULL;
}

void enter_scope()
{
    Scope *sc = calloc(1, sizeof(Scope));
    sc->next = scope;
    scope = sc;
}

void leave_scope()
{
    scope = scope->next;
}

VarScope *push_scope(char *name, Obj *var)
{
    VarScope *sc = calloc(1, sizeof(VarScope));
    sc->name = name;
    sc->var = var;
    sc->next = scope->vars;
    scope->vars = sc;
    return sc;
}

void create_param_lvars(Type *param)
{
    if (param)
    {
        create_param_lvars(param->next);
        new_lvar(get_ident(param->name), param);
    }
}

static char *new_unique_name()
{
    static int id = 0;
    char *buf = calloc(1, 20);
    sprintf(buf, ".L..%d", id++);
    return buf;
}

static Obj *new_anon_gvar(Type *ty)
{
    return new_gvar(new_unique_name(), ty);
}

static Obj *new_string_literal(char *p, Type *ty)
{
    Obj *var = new_anon_gvar(ty);
    var->init_data = p;
    return var;
}

char *get_ident(Token *tok)
{
    return strndup(tok->loc, tok->len);
}

void print_node(Token *cur, const char *func)
{
    if (LOG)
    {
        fprintf(stderr, "# %s: %s\n", func, cur->loc);
    }
}

Obj *parse(Token *tok)
{
    print_node(tok, __FUNCTION__);
    globals = NULL;

    while (tok->kind != TK_EOF)
    {
        Type *base_ty = declspec(&tok, tok);

        // Function
        if (is_function(tok))
        {
            tok = function(tok, base_ty);
            continue;
        }

        // Global variable
        tok = global_variable(tok, base_ty);
    }
    return globals;
}

Token *function(Token *tok, Type *base_ty)
{
    Type *ty = declarator(&tok, tok, base_ty);

    Obj *func = new_gvar(get_ident(ty->name), ty);
    func->is_function = true;

    locals = NULL;

    enter_scope();

    create_param_lvars(ty->params);
    func->params = locals;

    tok = skip(tok, "{");

    func->body = compound_stmt(&tok, tok);
    func->locals = locals;
    leave_scope();
    return tok;
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
    if (equal(tok, "char"))
    {
        *cur = tok->next;
        return ty_char;
    }

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

    ty = type_suffix(cur, tok->next, ty);
    ty->name = tok;
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
        Obj *var = new_lvar(get_ident(ty->name), ty);

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

Token *global_variable(Token *tok, Type *base_ty)
{
    int cnt = 0;

    while (!consume(&tok, tok, ";"))
    {
        if (cnt++ > 0)
        {
            tok = skip(tok, ",");
        }

        Type *ty = declarator(&tok, tok, base_ty);
        new_gvar(get_ident(ty->name), ty);
    }
    return tok;
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

    enter_scope();

    while (!equal(tok, "}"))
    {
        if (is_typename(tok))
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

    leave_scope();

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
    rhs = new_binary(ND_MUL, rhs, new_num(lhs->ty->base->size));
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
        rhs = new_binary(ND_MUL, rhs, new_num(lhs->ty->base->size));
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
        return new_binary(ND_DIV, node, new_num(lhs->ty->base->size));
    }
    rhs = new_binary(ND_MUL, rhs, new_num(lhs->ty->base->size));
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

    Node *node = postfix(&tok, tok);
    *cur = tok;
    return node;
}

Node *postfix(Token **cur, Token *tok)
{
    Node *node = primary(&tok, tok);

    while (equal(tok, "["))
    {
        // x[y] is short for *(x+y)
        Token *start = tok;
        Node *idx = expr(&tok, tok->next);
        tok = skip(tok, "]");
        node = new_unary(ND_DEREF, add_with_type(node, idx));
    }
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

    if (tok->kind == TK_STR)
    {
        Obj *var = new_string_literal(tok->str, tok->ty);
        *cur = tok->next;
        return new_var_node(var);
    }

    if (equal(tok, "sizeof"))
    {
        Node *node = unary(cur, tok->next);
        add_type(node);
        return new_num(node->ty->size);
    }

    if (tok->kind == TK_IDENT)
    {
        // Check func call
        if (equal(tok->next, "("))
        {
            Node *node = new_node(ND_FUNCALL);
            node->funcname = strndup(tok->loc, tok->len);
            node->args = func_args(&tok, tok->next);
            *cur = tok;
            return node;
        }

        // Variable
        Node *node = new_node(ND_VAR);
        Obj *var = find_var(tok);
        if (!var)
        {
            error("Undefined variable '%s'", strndup(tok->loc, tok->len));
        }
        *cur = tok->next;
        return new_var_node(var);
    }

    if (equal(tok, "(") && equal(tok->next, "{"))
    {
        Node *node = new_node(ND_STMT_EXPR);
        node->body = compound_stmt(&tok, tok->next->next)->body;
        *cur = skip(tok, ")");
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

Type *type_suffix(Token **cur, Token *tok, Type *ty)
{
    if (equal(tok, "("))
    {
        return func_params(cur, tok->next, ty);
    }
    if (equal(tok, "["))
    {
        int sz = get_number(tok->next);
        tok = skip(tok->next->next, "]");
        ty = type_suffix(cur, tok, ty);
        return array_of(ty, sz);
    }
    *cur = tok;
    return ty;
}

Type *func_params(Token **cur, Token *tok, Type *ty)
{
    Type head = {};
    Type *cur_type = &head;

    while (!equal(tok, ")"))
    {
        if (cur_type != &head)
        {
            tok = skip(tok, ",");
        }

        Type *base_ty = declspec(&tok, tok);
        Type *ty = declarator(&tok, tok, base_ty);
        cur_type->next = copy_type(ty);
        cur_type = cur_type->next;
    }

    ty = func_type(ty);
    ty->params = head.next;
    *cur = skip(tok, ")");
    return ty;
}

Node *func_args(Token **cur, Token *tok)
{
    Node head = {};
    Node *cur_node = &head;

    tok = skip(tok, "(");
    int cnt = 0;
    while (!equal(tok, ")"))
    {
        if (cnt++ > 0)
        {
            tok = skip(tok, ",");
        }
        cur_node->next = assign(&tok, tok);
        cur_node = cur_node->next;
    }

    tok = skip(tok, ")");
    *cur = tok;
    return head.next;
}

bool is_function(Token *tok)
{
    if (equal(tok, ";"))
    {
        return false;
    }

    Type dummy = {};
    Type *ty = declarator(&tok, tok, &dummy);
    return ty->kind == TY_FUNC;
}

bool is_typename(Token *tok)
{
    return equal(tok, "char") || equal(tok, "int");
}