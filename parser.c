#include "nerocc.h"

Obj *locals;
Obj *globals;

static Scope *scope = &(Scope){};

// Points to the function object the parser is currently parsing.
static Obj *current_func;

Node *new_node(NodeKind kind, Token *tok)
{
    Node *node = calloc(1, sizeof(Node));
    node->kind = kind;
    node->tok = tok;
    return node;
}

// binary operator +, -, *, /
Node *new_binary(NodeKind kind, Node *lhs, Node *rhs, Token *tok)
{
    Node *node = new_node(kind, tok);
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

Node *new_unary(NodeKind kind, Node *expr, Token *tok)
{
    Node *node = new_node(kind, tok);
    node->lhs = expr;
    return node;
}

Node *new_cast(Node *expr, Type *ty)
{
    add_type(expr);
    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_CAST;
    node->tok = expr->tok;
    node->lhs = expr;
    node->ty = copy_type(ty);
    return node;
}

Node *new_num(int64_t val, Token *tok)
{
    Node *node = new_node(ND_NUM, tok);
    node->val = val;
    return node;
}

Node *new_long(int64_t val, Token *tok)
{
    Node *node = new_node(ND_NUM, tok);
    node->val = val;
    node->ty = ty_long;
    return node;
}

Node *new_var_node(Obj *var, Token *tok)
{
    Node *node = new_node(ND_VAR, tok);
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
    push_scope(name)->var = var;
    return var;
}

VarScope *find_var(Token *tok)
{
    for (Scope *sc = scope; sc; sc = sc->next)
    {
        for (VarScope *sc2 = sc->vars; sc2; sc2 = sc2->next)
        {
            if (equal(tok, sc2->name))
            {
                return sc2;
            }
        }
    }
    return NULL;
}

Type *find_typedef(Token *tok)
{
    if (tok->kind == TK_IDENT)
    {
        VarScope *sc = find_var(tok);
        if (sc)
        {
            return sc->type_def;
        }
    }
    return NULL;
}

Type *find_tag(Token *tok)
{
    for (Scope *sc = scope; sc; sc = sc->next)
    {
        for (TagScope *sc2 = sc->tags; sc2; sc2 = sc2->next)
        {
            if (equal(tok, sc2->name))
            {
                return sc2->ty;
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

VarScope *push_scope(char *name)
{
    VarScope *sc = calloc(1, sizeof(VarScope));
    sc->name = name;
    sc->next = scope->vars;
    scope->vars = sc;
    return sc;
}

void push_tag_scope(Token *tok, Type *ty)
{
    TagScope *sc = calloc(1, sizeof(TagScope));
    sc->name = strndup(tok->loc, tok->len);
    sc->ty = ty;
    sc->next = scope->tags;
    scope->tags = sc;
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
        VarAttr attr = {};
        Type *base_ty = declspec(&tok, tok, &attr);

        // Typedef
        if (attr.is_typedef)
        {
            tok = parse_typedef(tok, base_ty);
            continue;
        }
        // Function
        if (is_function(tok))
        {
            tok = function(tok, base_ty, &attr);
            continue;
        }

        // Global variable
        tok = global_variable(tok, base_ty);
    }
    return globals;
}

Token *function(Token *tok, Type *base_ty, VarAttr *attr)
{
    Type *ty = declarator(&tok, tok, base_ty);

    Obj *func = new_gvar(get_ident(ty->name), ty);
    func->is_function = true;
    func->is_definition = !consume(&tok, tok, ";");
    func->is_static = attr->is_static;
    if (!func->is_definition)
    {
        return tok;
    }

    current_func = func;
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

Type *declspec(Token **cur, Token *tok, VarAttr *attr)
{
    enum
    {
        VOID = 1 << 0,
        BOOL = 1 << 2,
        CHAR = 1 << 4,
        SHORT = 1 << 6,
        INT = 1 << 8,
        LONG = 1 << 10,
        OTHER = 1 << 12,
    };

    Type *ty = ty_int;

    int counter = 0;
    while (is_typename(tok))
    {
        // Handle storage class specifiers.
        if (equal(tok, "typedef") || equal(tok, "static"))
        {
            if (!attr)
            {
                error_tok(tok, "Storage class specifier is not allowed in this context.");
            }
            if (equal(tok, "typedef"))
            {
                attr->is_typedef = true;
            }
            else
            {
                attr->is_static = true;
            }
            if (!attr->is_typedef && !attr->is_static)
            {
                error_tok(tok, "typedef and static may not be used together.");
            }
            tok = tok->next;
            continue;
        }

        // Handle user-defined types.
        Type *ty2 = find_typedef(tok);
        if (equal(tok, "struct") || equal(tok, "union") || equal(tok, "enum") || ty2)
        {
            if (counter)
            {
                break;
            }
            if (equal(tok, "struct"))
            {
                ty = struct_decl(&tok, tok->next);
            }
            else if (equal(tok, "union"))
            {
                ty = union_decl(&tok, tok->next);
            }
            else if (equal(tok, "enum"))
            {
                ty = enum_specifier(&tok, tok->next);
            }
            else
            {
                ty = ty2;
                tok = tok->next;
            }

            counter += OTHER;
            continue;
        }

        // Handle built-in tyeps.

        if (equal(tok, "void"))
        {
            counter += VOID;
        }
        else if (equal(tok, "_Bool"))
        {
            counter += BOOL;
        }
        else if (equal(tok, "char"))
        {
            counter += CHAR;
        }
        else if (equal(tok, "short"))
        {
            counter += SHORT;
        }
        else if (equal(tok, "int"))
        {
            counter += INT;
        }
        else if (equal(tok, "long"))
        {
            counter += LONG;
        }
        else
        {
            error_tok(tok, "Invalid type.");
        }

        switch (counter)
        {
        case VOID:
            ty = ty_void;
            break;
        case BOOL:
            ty = ty_bool;
            break;
        case CHAR:
            ty = ty_char;
            break;
        case SHORT:
        case SHORT + INT:
            ty = ty_short;
            break;
        case INT:
            ty = ty_int;
            break;
        case LONG:
        case LONG + LONG:
        case LONG + INT:
            ty = ty_long;
            break;
        default:
            error_tok(tok, "Invalid type.");
        }
        tok = tok->next;
    }

    *cur = tok;

    return ty;
}

Type *declarator(Token **cur, Token *tok, Type *ty)
{
    while (consume(&tok, tok, "*"))
    {
        ty = pointer_to(ty);
    }

    if (equal(tok, "("))
    {
        Token *start = tok;
        Type dummy = {};
        declarator(&tok, start->next, &dummy);
        tok = skip(tok, ")");
        ty = type_suffix(cur, tok, ty);
        return declarator(&tok, start->next, ty);
    }

    if (tok->kind != TK_IDENT)
    {
        error_tok(tok, "Expected a variable name.");
    }

    ty = type_suffix(cur, tok->next, ty);
    ty->name = tok;
    return ty;
}

Node *declaration(Token **cur, Token *tok, Type *base_ty)
{
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
        if (ty->kind == TY_VOID)
        {
            error_tok(tok, "variable declared void.");
        }
        Obj *var = new_lvar(get_ident(ty->name), ty);

        if (!equal(tok, "="))
        {
            continue;
        }

        Node *lhs = new_var_node(var, tok);
        Node *rhs = assign(&tok, tok->next);
        Node *node = new_binary(ND_ASSIGN, lhs, rhs, tok);

        cur_node->next = new_unary(ND_EXPR_STMT, node, tok);
        cur_node = cur_node->next;
    }

    Node *node = new_node(ND_BLOCK, tok);
    node->body = head.next;
    *cur = tok->next;
    return node;
}

Type *enum_specifier(Token **cur, Token *tok)
{
    Type *ty = enum_type();

    // Read a struct tag.
    Token *tag = NULL;
    if (tok->kind == TK_IDENT)
    {
        tag = tok;
        tok = tok->next;
    }

    if (tag && !(equal(tok, "{")))
    {
        Type *ty = find_tag(tag);
        if (!ty)
        {
            error_tok(tag, "Unknown enum type.");
        }
        if (ty->kind != TY_ENUM)
        {
            error_tok(tag, "not an enum tag.");
        }
        *cur = tok;
        return ty;
    }

    tok = skip(tok, "{");

    // Read an enum-list.
    int i = 0;
    int val = 0;
    while (!equal(tok, "}"))
    {
        if (i++ > 0)
        {
            tok = skip(tok, ",");
        }

        char *name = get_ident(tok);
        tok = tok->next;

        if (equal(tok, "="))
        {
            val = get_number(tok->next);
            tok = tok->next->next;
        }

        VarScope *sc = push_scope(name);
        sc->enum_ty = ty;
        sc->enum_val = val++;
    }

    *cur = tok->next;
    if (tag)
    {
        push_tag_scope(tag, ty);
    }
    return ty;
}

Type *struct_decl(Token **cur, Token *tok)
{
    Type *ty = struct_or_union_decl(cur, tok);
    ty->kind = TY_STRUCT;

    // Assign offsets within the struct to members.
    int offset = 0;
    for (Member *mem = ty->members; mem; mem = mem->next)
    {
        offset = align_to(offset, mem->ty->align);
        mem->offset = offset;
        offset += mem->ty->size;

        if (ty->align < mem->ty->align)
        {
            ty->align = mem->ty->align;
        }
    }
    ty->size = align_to(offset, ty->align);
    return ty;
}

Type *union_decl(Token **cur, Token *tok)
{
    Type *ty = struct_or_union_decl(cur, tok);
    ty->kind = TY_UNION;

    for (Member *mem = ty->members; mem; mem = mem->next)
    {
        if (ty->align < mem->ty->align)
        {
            ty->align = mem->ty->align;
        }

        if (ty->size < mem->ty->size)
        {
            ty->size = mem->ty->size;
        }
    }
    ty->size = align_to(ty->size, ty->align);
    return ty;
}

Type *struct_or_union_decl(Token **cur, Token *tok)
{
    // Read a tag
    Token *tag = NULL;

    if (tok->kind == TK_IDENT)
    {
        tag = tok;
        tok = tok->next;
    }

    if (tag && !equal(tok, "{"))
    {
        Type *ty = find_tag(tag);
        if (!ty)
        {
            error_tok(tag, "Unknown struct type.");
        }
        *cur = tok;
        return ty;
    }

    // Construct a struct object
    Type *ty = calloc(1, sizeof(Type));
    struct_members(cur, tok->next, ty);
    ty->align = 1;

    // Register the struct type if a name was given.
    if (tag)
    {
        push_tag_scope(tag, ty);
    }
    return ty;
}

void struct_members(Token **cur, Token *tok, Type *ty)
{
    Member head = {};
    Member *cur_mem = &head;

    while (!equal(tok, "}"))
    {
        Type *base_ty = declspec(&tok, tok, NULL);
        int i = 0;

        while (!consume(&tok, tok, ";"))
        {
            if (i++ > 0)
            {
                tok = skip(tok, ",");
            }

            Member *mem = calloc(1, sizeof(Member));
            mem->ty = declarator(&tok, tok, base_ty);
            mem->name = mem->ty->name;
            cur_mem->next = mem;
            cur_mem = cur_mem->next;
        }
    }

    *cur = tok->next;
    ty->members = head.next;
}

Node *struct_ref(Node *lhs, Token *tok)
{
    add_type(lhs);
    if (lhs->ty->kind != TY_STRUCT && lhs->ty->kind != TY_UNION)
    {
        error_tok(lhs->tok, "Not a struct nor a union.");
    }

    Node *node = new_unary(ND_MEMBER, lhs, tok);
    node->member = get_struct_member(lhs->ty, tok);
    return node;
}

Member *get_struct_member(Type *ty, Token *tok)
{
    for (Member *mem = ty->members; mem; mem = mem->next)
    {
        if (mem->name->len == tok->len && !strncmp(mem->name->loc, tok->loc, tok->len))
        {
            return mem;
        }
    }
    error("No such member.");
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
        Node *node = new_node(ND_RETURN, tok);
        Node *exp = expr(&tok, tok->next);

        *cur = skip(tok, ";");

        add_type(exp);
        node->lhs = new_cast(exp, current_func->ty->return_ty);
        return node;
    }

    if (equal(tok, "{"))
    {
        return compound_stmt(cur, tok->next);
    }

    if (equal(tok, "if"))
    {
        Node *node = new_node(ND_IF, tok);
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
        Node *node = new_node(ND_FOR, tok);
        tok = skip(tok->next, "(");

        enter_scope();

        if (is_typename(tok))
        {
            Type *base_ty = declspec(&tok, tok, NULL);
            node->init = declaration(&tok, tok, base_ty);
        }
        else
        {
            node->init = expr_stmt(&tok, tok);
        }

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
        leave_scope();
        return node;
    }

    if (equal(tok, "while"))
    {
        Node *node = new_node(ND_WHILE, tok);
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
            VarAttr attr = {};
            Type *base_ty = declspec(&tok, tok, &attr);
            if (attr.is_typedef)
            {
                tok = parse_typedef(tok, base_ty);
                continue;
            }
            t->next = declaration(&tok, tok, base_ty);
        }
        else
        {
            t->next = stmt(&tok, tok);
        }
        t = t->next;
        add_type(t);
    }

    leave_scope();

    Node *node = new_node(ND_BLOCK, tok);
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
        return new_node(ND_BLOCK, tok);
    }
    Node *node = new_unary(ND_EXPR_STMT, expr(&tok, tok), tok);
    *cur = skip(tok, ";");
    return node;
}

Node *expr(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    Node *node = assign(&tok, tok);

    if (equal(tok, ","))
    {
        return new_binary(ND_COMMA, node, expr(cur, tok->next), tok);
    }
    *cur = tok;
    return node;
}

Node *assign(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    Node *node = equality(&tok, tok);
    if (equal(tok, "="))
    {
        return new_binary(ND_ASSIGN, node, assign(cur, tok->next), tok);
    }

    if (equal(tok, "+="))
    {
        return to_assign(add_with_type(node, assign(cur, tok->next), tok));
    }

    if (equal(tok, "-="))
    {
        return to_assign(sub_with_type(node, assign(cur, tok->next), tok));
    }

    if (equal(tok, "*="))
    {
        return to_assign(new_binary(ND_MUL, node, assign(cur, tok->next), tok));
    }

    if (equal(tok, "/="))
    {
        return to_assign(new_binary(ND_DIV, node, assign(cur, tok->next), tok));
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
            node = new_binary(ND_EQ, node, relational(&tok, tok->next), tok);
            continue;
        }
        if (equal(tok, "!="))
        {
            node = new_binary(ND_NE, node, relational(&tok, tok->next), tok);
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
            node = new_binary(ND_LT, node, add(&tok, tok->next), tok);
            continue;
        }
        if (equal(tok, "<="))
        {
            node = new_binary(ND_LE, node, add(&tok, tok->next), tok);
            continue;
        }
        if (equal(tok, ">"))
        {
            node = new_binary(ND_LT, add(&tok, tok->next), node, tok);
            continue;
        }
        if (equal(tok, ">="))
        {
            node = new_binary(ND_LE, add(&tok, tok->next), node, tok);
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
            node = add_with_type(node, mul(&tok, tok->next), tok);
            continue;
        }
        if (equal(tok, "-"))
        {
            node = sub_with_type(node, mul(&tok, tok->next), tok);
            continue;
        }
        *cur = tok;
        return node;
    }
}

Node *add_with_type(Node *lhs, Node *rhs, Token *tok)
{
    add_type(lhs);
    add_type(rhs);

    // num + num
    if (is_integer(lhs->ty) && is_integer(rhs->ty))
    {
        return new_binary(ND_ADD, lhs, rhs, tok);
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
    rhs = new_binary(ND_MUL, rhs, new_long(lhs->ty->base->size, tok), tok);
    return new_binary(ND_ADD, lhs, rhs, tok);
}

Node *sub_with_type(Node *lhs, Node *rhs, Token *tok)
{
    add_type(lhs);
    add_type(rhs);

    // num - num
    if (is_integer(lhs->ty) && is_integer(rhs->ty))
    {
        return new_binary(ND_SUB, lhs, rhs, tok);
    }

    // We don't allow num - ptr op
    if (is_integer(lhs->ty) && rhs->ty->base)
    {
        error("Invalid operands.");
    }

    // ptr - num
    if (lhs->ty->base && is_integer(rhs->ty))
    {
        rhs = new_binary(ND_MUL, rhs, new_long(lhs->ty->base->size, tok), tok);
        add_type(rhs);
        Node *node = new_binary(ND_SUB, lhs, rhs, tok);
        node->ty = lhs->ty;
        return node;
    }

    // ptr - ptr returns how many elements between them
    if (lhs->ty->base && rhs->ty->base)
    {
        Node *node = new_binary(ND_SUB, lhs, rhs, tok);
        node->ty = ty_int;
        return new_binary(ND_DIV, node, new_num(lhs->ty->base->size, tok), tok);
    }
    rhs = new_binary(ND_MUL, rhs, new_num(lhs->ty->base->size, tok), tok);
    return new_binary(ND_ADD, lhs, rhs, tok);
}

Node *mul(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    Node *node = cast(&tok, tok);

    for (;;)
    {
        if (equal(tok, "*"))
        {
            node = new_binary(ND_MUL, node, cast(&tok, tok->next), tok);
            continue;
        }
        if (equal(tok, "/"))
        {
            node = new_binary(ND_DIV, node, cast(&tok, tok->next), tok);
            continue;
        }
        *cur = tok;
        return node;
    }
}

Node *cast(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    if (equal(tok, "(") && is_typename(tok->next))
    {
        Token *start = tok;
        Type *ty = typename(&tok, tok->next);
        tok = skip(tok, ")");
        Node *node = new_cast(cast(cur, tok), ty);
        node->tok = start;
        return node;
    }

    return unary(cur, tok);
}

// Converts +primary, -primary to be 0 + primary, 0 - primary
Node *unary(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    if (equal(tok, "+"))
    {
        Node *node = cast(&tok, tok->next);
        *cur = tok;
        return node;
    }

    if (equal(tok, "-"))
    {
        Node *node = cast(&tok, tok->next);
        *cur = tok;
        return new_unary(ND_NEG, node, tok);
    }

    if (equal(tok, "*"))
    {
        Node *node = cast(&tok, tok->next);
        *cur = tok;
        return new_unary(ND_DEREF, node, tok);
    }

    if (equal(tok, "&"))
    {
        Node *node = cast(&tok, tok->next);
        *cur = tok;
        return new_unary(ND_ADDR, node, tok);
    }

    Node *node = postfix(&tok, tok);
    *cur = tok;
    return node;
}

Node *postfix(Token **cur, Token *tok)
{
    Node *node = primary(&tok, tok);

    for (;;)
    {
        if (equal(tok, "["))
        {
            // x[y] is short for *(x+y)
            Token *start = tok;
            Node *idx = expr(&tok, tok->next);
            tok = skip(tok, "]");
            node = new_unary(ND_DEREF, add_with_type(node, idx, tok), tok);
            continue;
        }

        if (equal(tok, "."))
        {
            node = struct_ref(node, tok->next);
            tok = tok->next->next;
            continue;
        }

        if (equal(tok, "->"))
        {
            // x->y is short for *(x).y
            node = new_unary(ND_DEREF, node, tok);
            node = struct_ref(node, tok->next);
            tok = tok->next->next;
            continue;
        }

        *cur = tok;
        return node;
    }
}

Node *primary(Token **cur, Token *tok)
{
    print_node(tok, __FUNCTION__);
    Token *start = tok;
    if (tok->kind == TK_NUM)
    {
        Node *node = new_num(tok->val, tok);
        *cur = tok->next;
        return node;
    }

    if (tok->kind == TK_STR)
    {
        Obj *var = new_string_literal(tok->str, tok->ty);
        *cur = tok->next;
        return new_var_node(var, tok);
    }

    if (equal(tok, "sizeof") && equal(tok->next, "(") && is_typename(tok->next->next))
    {
        Type *ty = typename(&tok, tok->next->next);
        *cur = skip(tok, ")");
        return new_num(ty->size, start);
    }

    if (equal(tok, "sizeof"))
    {
        Node *node = unary(cur, tok->next);
        add_type(node);
        return new_num(node->ty->size, tok);
    }

    if (tok->kind == TK_IDENT)
    {
        // Check func call
        if (equal(tok->next, "("))
        {
            return funcall(cur, tok);
        }

        // Variable or enum constant
        VarScope *sc = find_var(tok);
        if (!sc || (!sc->var && !sc->enum_ty))
        {
            error_tok(tok, "Undefined variable");
        }

        Node *node;
        if (sc->var)
        {
            node = new_var_node(sc->var, tok);
        }
        else
        {
            node = new_num(sc->enum_val, tok);
        }
        *cur = tok->next;
        return node;
    }

    if (equal(tok, "(") && equal(tok->next, "{"))
    {
        Node *node = new_node(ND_STMT_EXPR, tok);
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

Node *funcall(Token **cur, Token *tok)
{
    Token *start = tok;
    tok = tok->next->next;
    VarScope *sc = find_var(start);
    if (!sc)
    {
        error_tok(start, "implicit declaration of a function.");
    }

    if (!sc->var || sc->var->ty->kind != TY_FUNC)
    {
        error_tok(start, "Not a function.");
    }

    Type *ty = sc->var->ty;
    Type *param_ty = ty->params;

    Node head = {};
    Node *cur_node = &head;

    while (!equal(tok, ")"))
    {
        if (cur_node != &head)
        {
            tok = skip(tok, ",");
        }

        Node *arg = assign(&tok, tok);
        add_type(arg);

        if (param_ty)
        {
            if (param_ty->kind == TY_STRUCT || param_ty->kind == TY_UNION)
            {
                error_tok(arg->tok, "passing struct or union is not supported yet.");
            }
            arg = new_cast(arg, param_ty);
            param_ty = param_ty->next;
        }

        cur_node->next = arg;
        cur_node = cur_node->next;
    }

    *cur = skip(tok, ")");

    Node *node = new_node(ND_FUNCALL, start);
    node->funcname = strndup(start->loc, start->len);
    node->func_ty = ty;
    node->ty = ty->return_ty;
    node->args = head.next;
    return node;
}

Type *abstract_declarator(Token **cur, Token *tok, Type *ty)
{
    while (equal(tok, "*"))
    {
        ty = pointer_to(ty);
        tok = tok->next;
    }

    if (equal(tok, "("))
    {
        Token *start = tok;
        Type dummy = {};
        abstract_declarator(&tok, start->next, &dummy);
        tok = skip(tok, ")");
        ty = type_suffix(cur, tok, ty);
        return abstract_declarator(&tok, start->next, ty);
    }

    return type_suffix(cur, tok, ty);
}

Type *typename(Token **cur, Token *tok)
{
    Type *ty = declspec(&tok, tok, NULL);
    return abstract_declarator(cur, tok, ty);
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

        Type *base_ty = declspec(&tok, tok, NULL);
        Type *ty = declarator(&tok, tok, base_ty);
        cur_type->next = copy_type(ty);
        cur_type = cur_type->next;
    }

    ty = func_type(ty);
    ty->params = head.next;
    *cur = skip(tok, ")");
    return ty;
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
    return equal(tok, "void") || equal(tok, "_Bool") || equal(tok, "char") || equal(tok, "int") || equal(tok, "short") || equal(tok, "long") || equal(tok, "struct") || equal(tok, "union") || equal(tok, "typedef") || equal(tok, "enum") || equal(tok, "static") || find_typedef(tok);
}

Token *parse_typedef(Token *tok, Type *base_ty)
{
    int cnt = 0;
    while (!consume(&tok, tok, ";"))
    {
        if (cnt++ > 0)
        {
            tok = skip(tok, ",");
        }

        Type *ty = declarator(&tok, tok, base_ty);
        push_scope(get_ident(ty->name))->type_def = ty;
    }
    return tok;
}

// Convert `A op= B` to `tmp = &A, *tmp = *tmp op B`
// where tmp is a fresh pointer variable
Node *to_assign(Node *binary)
{
    add_type(binary->lhs);
    add_type(binary->rhs);

    Token *tok = binary->tok;

    Obj *var = new_lvar("", pointer_to(binary->lhs->ty));

    Node *expr1 = new_binary(ND_ASSIGN, new_var_node(var, tok), new_unary(ND_ADDR, binary->lhs, tok), tok);

    Node *expr2 = new_binary(ND_ASSIGN, new_unary(ND_DEREF, new_var_node(var, tok), tok), new_binary(binary->kind, new_unary(ND_DEREF, new_var_node(var, tok), tok), binary->rhs, tok), tok);

    return new_binary(ND_COMMA, expr1, expr2, tok);
}