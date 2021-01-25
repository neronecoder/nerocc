#include "nerocc.h"

Obj *locals;
Obj *globals;

static Scope *scope = &(Scope){};

// Points to the function object the parser is currently parsing.
static Obj *current_func;

// Lists of all goto statements and labels in the current function.
Node *gotos;
Node *labels;

// Current "goto" and "continue" jump tagets.
static char *break_label;
static char *cont_label;

// Points to a node representing a switch if we are parsing
// a switch statement. O/W, NULL.
Node *current_switch;

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

// Convert A++ to `(typeof A)((A+=1) - 1)`
Node *new_inc_dec(Node *node, Token *tok, int addend)
{
    add_type(node);
    return new_cast(add_with_type(to_assign(add_with_type(node, new_num(addend, tok), tok)), new_num(-addend, tok), tok), node->ty);
}

Initializer *new_initializer(Type *ty)
{
    Initializer *init = calloc(1, sizeof(Initializer));
    init->ty = ty;

    if (ty->kind == TY_ARRAY)
    {
        init->children = calloc(ty->array_len, sizeof(Initializer *));
        for (int i = 0; i < ty->array_len; i++)
        {
            init->children[i] = new_initializer(ty->base);
        }
    }
    return init;
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
    resolve_goto_labels();
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
        if (ty->size < 0)
        {
            error_tok(tok, "variable has incomplete type");
        }
        if (ty->kind == TY_VOID)
        {
            error_tok(tok, "variable declared void.");
        }
        Obj *var = new_lvar(get_ident(ty->name), ty);

        if (equal(tok, "="))
        {
            Node *expr = lvar_initializer(&tok, tok->next, var);
            cur_node->next = new_unary(ND_EXPR_STMT, expr, tok);
            cur_node = cur_node->next;
        }
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
            val = const_expr(&tok, tok->next);
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

    if (ty->size < 0)
    {
        return ty;
    }

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

    if (ty->size < 0)
    {
        return ty;
    }

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
        *cur = tok;

        Type *ty = find_tag(tag);
        if (ty)
        {
            return ty;
        }
        ty = struct_type();
        ty->size = 1;
        push_tag_scope(tag, ty);
        return ty;
    }

    tok = skip(tok, "{");

    // Construct a struct object
    Type *ty = struct_type();
    struct_members(cur, tok, ty);

    if (tag)
    {
        // If this is a redefinition, overwrite a previous type.
        // O/W, register the struct type.
        for (TagScope *sc = scope->tags; sc; sc = sc->next)
        {
            if (equal(tag, sc->name))
            {
                *sc->ty = *ty;
                return sc->ty;
            }
        }
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

    if (equal(tok, "switch"))
    {
        Node *node = new_node(ND_SWITCH, tok);
        tok = skip(tok->next, "(");
        node->cond = expr(&tok, tok);
        tok = skip(tok, ")");

        Node *sw = current_switch;
        current_switch = node;

        char *brk = break_label;
        break_label = node->break_label = new_unique_name();

        node->then = stmt(cur, tok);

        current_switch = sw;
        break_label = brk;
        return node;
    }

    if (equal(tok, "case"))
    {
        if (!current_switch)
        {
            error_tok(tok, "stray case");
        }

        Node *node = new_node(ND_CASE, tok);
        int val = const_expr(&tok, tok->next);
        tok = skip(tok, ":");
        node->label = new_unique_name();
        node->lhs = stmt(cur, tok);
        node->val = val;
        node->case_next = current_switch->case_next;
        current_switch->case_next = node;
        return node;
    }

    if (equal(tok, "default"))
    {
        if (!current_switch)
        {
            error_tok(tok, "stray default");
        }

        Node *node = new_node(ND_CASE, tok);
        tok = skip(tok->next, ":");
        node->label = new_unique_name();
        node->lhs = stmt(cur, tok);
        current_switch->default_case = node;
        return node;
    }

    if (equal(tok, "for"))
    {
        Node *node = new_node(ND_FOR, tok);
        tok = skip(tok->next, "(");

        enter_scope();

        char *brk = break_label;
        char *cont = cont_label;
        break_label = node->break_label = new_unique_name();
        cont_label = node->cont_label = new_unique_name();

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
        break_label = brk;
        cont_label = cont;
        return node;
    }

    if (equal(tok, "while"))
    {
        Node *node = new_node(ND_FOR, tok);
        tok = skip(tok->next, "(");
        node->cond = expr(&tok, tok);
        tok = skip(tok, ")");

        char *brk = break_label;
        char *cont = cont_label;
        break_label = node->break_label = new_unique_name();
        cont_label = node->cont_label = new_unique_name();

        node->then = stmt(cur, tok);
        break_label = brk;
        cont_label = cont;
        return node;
    }

    if (equal(tok, "goto"))
    {
        Node *node = new_node(ND_GOTO, tok);
        node->label = get_ident(tok->next);
        node->goto_next = gotos;
        gotos = node;
        *cur = skip(tok->next->next, ";");
        return node;
    }

    if (equal(tok, "break"))
    {
        if (!break_label)
        {
            error_tok(tok, "stray break");
        }
        Node *node = new_node(ND_GOTO, tok);
        node->unique_label = break_label;
        *cur = skip(tok->next, ";");
        return node;
    }

    if (equal(tok, "continue"))
    {
        if (!cont_label)
        {
            error_tok(tok, "stray continue");
        }

        Node *node = new_node(ND_GOTO, tok);
        node->unique_label = cont_label;
        *cur = skip(tok->next, ";");
        return node;
    }

    if (tok->kind == TK_IDENT && equal(tok->next, ":"))
    {
        Node *node = new_node(ND_LABEL, tok);
        node->label = get_ident(tok);
        node->unique_label = new_unique_name();
        node->lhs = stmt(cur, tok->next->next);
        node->goto_next = labels;
        labels = node;
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
        if (is_typename(tok) && !equal(tok->next, ":"))
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
    Node *node = conditional(&tok, tok);
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

    if (equal(tok, "%="))
    {
        return to_assign(new_binary(ND_MOD, node, assign(cur, tok->next), tok));
    }

    if (equal(tok, "&="))
    {
        return to_assign(new_binary(ND_BITAND, node, assign(cur, tok->next), tok));
    }

    if (equal(tok, "|="))
    {
        return to_assign(new_binary(ND_BITOR, node, assign(cur, tok->next), tok));
    }

    if (equal(tok, "^="))
    {
        return to_assign(new_binary(ND_BITXOR, node, assign(cur, tok->next), tok));
    }

    if (equal(tok, "<<="))
    {
        return to_assign(new_binary(ND_SHL, node, assign(cur, tok->next), tok));
    }

    if (equal(tok, ">>="))
    {
        return to_assign(new_binary(ND_SHR, node, assign(cur, tok->next), tok));
    }
    *cur = tok;
    return node;
}

Node *conditional(Token **cur, Token *tok)
{
    Node *cond = logor(&tok, tok);

    if (!equal(tok, "?"))
    {
        *cur = tok;
        return cond;
    }

    Node *node = new_node(ND_COND, tok);
    node->cond = cond;
    node->then = expr(&tok, tok->next);

    tok = skip(tok, ":");
    node->els = conditional(cur, tok);
    return node;
}

Node *logor(Token **cur, Token *tok)
{
    Node *node = logand(&tok, tok);
    while (equal(tok, "||"))
    {
        Token *start = tok;
        node = new_binary(ND_LOGOR, node, logand(&tok, tok->next), start);
    }

    *cur = tok;
    return node;
}

Node *logand(Token **cur, Token *tok)
{
    Node *node = bitor (&tok, tok);
    while (equal(tok, "&&"))
    {
        Token *start = tok;
        node = new_binary(ND_LOGAND, node, bitor (&tok, tok->next), start);
    }

    *cur = tok;
    return node;
}

Node * bitor (Token * *cur, Token *tok)
{
    Node *node = bitxor(&tok, tok);
    while (equal(tok, "|"))
    {
        Token *start = tok;
        node = new_binary(ND_BITOR, node, bitxor(&tok, tok->next), start);
    }

    *cur = tok;
    return node;
}

Node *bitxor(Token **cur, Token *tok)
{
    Node *node = bitand(&tok, tok);
    while (equal(tok, "^"))
    {
        Token *start = tok;
        node = new_binary(ND_BITXOR, node, bitand(&tok, tok->next), start);
    }

    *cur = tok;
    return node;
}

Node *bitand(Token **cur, Token *tok)
{
    Node *node = equality(&tok, tok);
    while (equal(tok, "&"))
    {
        Token *start = tok;
        node = new_binary(ND_BITAND, node, equality(&tok, tok->next), start);
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
    Node *node = shift(&tok, tok);

    for (;;)
    {
        if (equal(tok, "<"))
        {
            node = new_binary(ND_LT, node, shift(&tok, tok->next), tok);
            continue;
        }
        if (equal(tok, "<="))
        {
            node = new_binary(ND_LE, node, shift(&tok, tok->next), tok);
            continue;
        }
        if (equal(tok, ">"))
        {
            node = new_binary(ND_LT, shift(&tok, tok->next), node, tok);
            continue;
        }
        if (equal(tok, ">="))
        {
            node = new_binary(ND_LE, shift(&tok, tok->next), node, tok);
            continue;
        }
        *cur = tok;
        return node;
    }
}

Node *shift(Token **cur, Token *tok)
{
    Node *node = add(&tok, tok);

    for (;;)
    {
        Token *start = tok;
        if (equal(tok, "<<"))
        {
            node = new_binary(ND_SHL, node, add(&tok, tok->next), start);
            continue;
        }
        if (equal(tok, ">>"))
        {
            node = new_binary(ND_SHR, node, add(&tok, tok->next), start);
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

        if (equal(tok, "%"))
        {
            node = new_binary(ND_MOD, node, cast(&tok, tok->next), tok);
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

    if (equal(tok, "!"))
    {
        return new_unary(ND_NOT, cast(cur, tok->next), tok);
    }

    if (equal(tok, "~"))
    {
        return new_unary(ND_BITNOT, cast(cur, tok->next), tok);
    }

    // Read ++i as i+=1
    if (equal(tok, "++"))
    {
        return to_assign(add_with_type(unary(cur, tok->next), new_num(1, tok), tok));
    }

    // Read --i as i-=1
    if (equal(tok, "--"))
    {
        return to_assign(sub_with_type(unary(cur, tok->next), new_num(1, tok), tok));
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

        if (equal(tok, "++"))
        {
            node = new_inc_dec(node, tok, 1);
            tok = tok->next;
            continue;
        }

        if (equal(tok, "--"))
        {
            node = new_inc_dec(node, tok, -1);
            tok = tok->next;
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

    error_tok(tok, "Expected an expression");
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
        return array_dimensions(cur, tok->next, ty);
    }
    *cur = tok;
    return ty;
}

Type *array_dimensions(Token **cur, Token *tok, Type *ty)
{
    if (equal(tok, "]"))
    {
        ty = type_suffix(cur, tok->next, ty);
        return array_of(ty, -1);
    }

    int sz = const_expr(&tok, tok);
    tok = skip(tok, "]");
    ty = type_suffix(cur, tok, ty);
    return array_of(ty, sz);
}

int64_t const_expr(Token **cur, Token *tok)
{
    Node *node = conditional(cur, tok);
    return eval(node);
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

        Type *ty2 = declspec(&tok, tok, NULL);
        ty2 = declarator(&tok, tok, ty2);

        // "array of T" is converted to "pointer to T" only in the parameter context.
        // For example, *argv[] is converted to **argv by this.
        if (ty2->kind == TY_ARRAY)
        {
            Token *name = ty2->name;
            ty2 = pointer_to(ty2->base);
            ty2->name = name;
        }

        cur_type->next = copy_type(ty2);
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

void resolve_goto_labels()
{
    for (Node *x = gotos; x; x = x->goto_next)
    {
        for (Node *y = labels; y; y = y->goto_next)
        {
            if (!strcmp(x->label, y->label))
            {
                x->unique_label = y->unique_label;
                break;
            }
        }

        if (x->unique_label == NULL)
        {
            error_tok(x->tok->next, "use of undeclared label");
        }
    }
    gotos = labels = NULL;
}

Token *skip_excess_element(Token *tok)
{
    if (equal(tok, "{"))
    {
        tok = skip_excess_element(tok->next);
        return skip(tok, "}");
    }

    assign(&tok, tok);
    return tok;
}

// initializer = "{" initializer ("," initializer)* "}" | assign
void initializer2(Token **cur, Token *tok, Initializer *init)
{
    if (init->ty->kind == TY_ARRAY)
    {
        tok = skip(tok, "{");

        for (int i = 0; !consume(cur, tok, "}"); i++)
        {
            if (i > 0)
            {
                tok = skip(tok, ",");
            }
            if (i < init->ty->array_len)
            {
                initializer2(&tok, tok, init->children[i]);
            }
            else
            {
                tok = skip_excess_element(tok);
            }
        }
        *cur = skip(tok, "}");
        return;
    }

    init->expr = assign(cur, tok);
}

Initializer *initializer(Token **cur, Token *tok, Type *ty)
{
    Initializer *init = new_initializer(ty);
    initializer2(cur, tok, init);
    return init;
}

Node *init_desg_expr(InitDesg *desg, Token *tok)
{
    if (desg->var)
    {
        return new_var_node(desg->var, tok);
    }
    Node *lhs = init_desg_expr(desg->next, tok);
    Node *rhs = new_num(desg->idx, tok);
    return new_unary(ND_DEREF, add_with_type(lhs, rhs, tok), tok);
}

Node *create_lvar_init(Initializer *init, Type *ty, InitDesg *desg, Token *tok)
{
    if (ty->kind == TY_ARRAY)
    {
        Node *node = new_node(ND_NULL_EXPR, tok);
        for (int i = 0; i < ty->array_len; i++)
        {
            InitDesg desg2 = {desg, i};
            Node *rhs = create_lvar_init(init->children[i], ty->base, &desg2, tok);
            node = new_binary(ND_COMMA, node, rhs, tok);
        }
        return node;
    }

    if (!init->expr)
    {
        return new_node(ND_NULL_EXPR, tok);
    }

    Node *lhs = init_desg_expr(desg, tok);
    return new_binary(ND_ASSIGN, lhs, init->expr, tok);
}

// A variable definition with an initializer is a shorthand notation
// for a variable definition followed by assignments. This function
// generates assignment expressions for an initializer. For example,
// `int x[2][2] = {{6, 7}, {8, 9}}` is converted to the following
// expressions:
//
//   x[0][0] = 6
//   x[0][1] = 7
//   x[1][0] = 8
//   x[1][1] = 9
Node *lvar_initializer(Token **cur, Token *tok, Obj *var)
{
    Initializer *init = initializer(cur, tok, var->ty);
    InitDesg desg = {NULL, 0, var};

    // If a partial initializer list is given, the standard requires
    // that unspecified elements are set to 0. Here, we simply
    // zero-initialize the entire memory region of a variable before
    // initializing it with user-supplied values.
    Node *lhs = new_node(ND_MEMZERO, tok);
    lhs->var = var;
    Node *rhs = create_lvar_init(init, var->ty, &desg, tok);
    return new_binary(ND_COMMA, lhs, rhs, tok);
}

int64_t eval(Node *node)
{
    add_type(node);

    switch (node->kind)
    {
    case ND_ADD:
        return eval(node->lhs) + eval(node->rhs);
    case ND_SUB:
        return eval(node->lhs) - eval(node->rhs);
    case ND_MUL:
        return eval(node->lhs) * eval(node->rhs);
    case ND_DIV:
        return eval(node->lhs) / eval(node->rhs);
    case ND_NEG:
        return -eval(node->lhs);
    case ND_MOD:
        return eval(node->lhs) % eval(node->rhs);
    case ND_BITAND:
        return eval(node->lhs) & eval(node->rhs);
    case ND_BITOR:
        return eval(node->lhs) | eval(node->rhs);
    case ND_BITXOR:
        return eval(node->lhs) ^ eval(node->rhs);
    case ND_SHL:
        return eval(node->lhs) << eval(node->rhs);
    case ND_SHR:
        return eval(node->lhs) >> eval(node->rhs);
    case ND_EQ:
        return eval(node->lhs) == eval(node->rhs);
    case ND_NE:
        return eval(node->lhs) != eval(node->rhs);
    case ND_LT:
        return eval(node->lhs) < eval(node->rhs);
    case ND_LE:
        return eval(node->lhs) <= eval(node->rhs);
    case ND_COND:
        return eval(node->cond) ? eval(node->then) : eval(node->els);
    case ND_COMMA:
        return eval(node->rhs);
    case ND_NOT:
        return !eval(node->lhs);
    case ND_BITNOT:
        return ~eval(node->lhs);
    case ND_LOGAND:
        return eval(node->lhs) && eval(node->rhs);
    case ND_LOGOR:
        return eval(node->lhs) || eval(node->rhs);
    case ND_CAST:
        if (is_integer(node->ty))
        {
            switch (node->ty->size)
            {
            case 1:
                return (uint8_t)eval(node->lhs);
            case 2:
                return (uint16_t)eval(node->lhs);
            case 4:
                return (uint32_t)eval(node->lhs);
            }
        }
        return eval(node->lhs);
    case ND_NUM:
        return node->val;
    }

    error_tok(node->tok, "Not a compile-time constant");
}