#include "nerocc.h"

static int count()
{
    static int i = 1;
    return i++;
}

static char *argreg8[] = {"%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"};
static char *argreg16[] = {"%di", "%si", "%dx", "%cx", "%r8w", "%r9w"};
static char *argreg32[] = {"%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"};
static char *argreg64[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};
static Obj *current_func;

static FILE *output_file;

void println(char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(output_file, fmt, ap);
    va_end(ap);
    fprintf(output_file, "\n");
}

void load(Type *ty)
{
    if (ty->kind == TY_ARRAY || ty->kind == TY_STRUCT || ty->kind == TY_UNION)
    {
        return;
    }
    if (ty->size == 1)
    {
        println("    movsbq (%%rax), %%rax");
    }
    else if (ty->size == 2)
    {
        println("    movswq (%%rax), %%rax");
    }
    else if (ty->size == 4)
    {
        println("    movsxd (%%rax), %%rax");
    }
    else
    {
        println("    mov (%%rax), %%rax");
    }
}

void store(Type *ty)
{
    if (ty->kind == TY_STRUCT || ty->kind == TY_UNION)
    {
        for (int i = 0; i < ty->size; i++)
        {
            println("    mov %d(%%rdi), %%r8b", i);
            println("    mov %%r8b, %d(%%rax)", i);
        }
        return;
    }
    if (ty->size == 1)
    {
        println("    mov %%dil, (%%rax)");
    }
    else if (ty->size == 2)
    {
        println("    mov %%dx, (%%rax)");
    }
    else if (ty->size == 4)
    {
        println("    mov %%edi, (%%rax)");
    }
    else
    {
        println("    mov %%rdi, (%%rax)");
    }
}

void store_gp(int r, int offset, int sz)
{
    switch (sz)
    {
    case 1:
        println("   mov %s, -%d(%%rbp)", argreg8[r], offset);
        return;
    case 2:
        println("   mov %s, -%d(%%rbp)", argreg16[r], offset);
        return;
    case 4:
        println("   mov %s, -%d(%%rbp)", argreg32[r], offset);
        return;
    case 8:
        println("   mov %s, -%d(%%rbp)", argreg64[r], offset);
        return;
    }
    error("Size should be one of (1, 4, 8), %d given.", sz);
}

void gen_code(Obj *prog, FILE *out)
{
    output_file = out;
    assign_lvar_offsets(prog);
    emit_data(prog);
    emit_text(prog);
}

void emit_data(Obj *prog)
{
    for (Obj *var = prog; var; var = var->next)
    {
        if (var->is_function)
        {
            continue;
        }

        println("    .data");
        println("    .globl %s", var->name);
        println("%s:", var->name);

        if (var->init_data)
        {
            for (int i = 0; i < var->ty->size; i++)
            {
                println("    .byte %d", var->init_data[i]);
            }
        }
        else
        {
            println("    .zero %d", var->ty->size);
        }
    }
}

void emit_text(Obj *prog)
{
    for (Obj *func = prog; func; func = func->next)
    {
        if (!func->is_function || !func->is_definition)
        {
            continue;
        }
        println("    .globl %s", func->name);
        println("    .text");
        println("%s:", func->name);
        current_func = func;

        // Prolugue
        println("    push %%rbp");
        println("    mov %%rsp, %%rbp");
        println("    sub $%d, %%rsp", func->stack_size);

        // Save passed-by-register arguments to the stack
        int i = 0;
        for (Obj *var = func->params; var; var = var->next)
        {
            store_gp(i++, var->offset, var->ty->size);
        }
        // Emit code
        gen_stmt(func->body);

        // Epilogue
        println(".L.return.%s:", func->name);
        println("    mov %%rbp, %%rsp");
        println("    pop %%rbp");
        println("    ret");
    }
}

void gen_stmt(Node *node)
{
    println("    .loc 1 %d", node->tok->line_no);
    switch (node->kind)
    {
    case ND_RETURN:
        gen_expr(node->lhs);
        println("    pop %%rax");
        println("    jmp .L.return.%s", current_func->name);
        return;
    case ND_BLOCK:
        for (Node *n = node->body; n; n = n->next)
        {
            gen_stmt(n);
        }
        return;
    case ND_IF:
    {
        int c = count();
        gen_expr(node->cond);
        println("    pop %%rax");
        println("    cmp $0, %%rax");
        println("    je  .L.else.%d", c);
        gen_stmt(node->then);
        println("    jmp .L.end.%d", c);
        println(".L.else.%d:", c);

        if (node->els)
        {
            gen_stmt(node->els);
        }
        println(".L.end.%d:", c);
        return;
    }

    case ND_FOR:
    {
        int c = count();
        gen_stmt(node->init);
        println(".L.begin.%d:", c);
        if (node->cond)
        {
            gen_expr(node->cond);
            println("    pop %%rax");
            println("    cmp $0, %%rax");
            println("    je  .L.end.%d", c);
        }
        gen_stmt(node->then);
        if (node->inc)
        {
            gen_expr(node->inc);
            println("    pop %%rax");
        }
        println("    jmp .L.begin.%d", c);
        println(".L.end.%d:", c);
        return;
    }
    case ND_WHILE:
    {
        int c = count();
        println(".L.begin.%d:", c);
        gen_expr(node->cond);
        println("    pop %%rax");
        println("    cmp $0, %%rax");
        println("    je  .L.end.%d", c);

        gen_stmt(node->then);
        println("    jmp .L.begin.%d", c);
        println(".L.end.%d:", c);
        return;
    }
    case ND_EXPR_STMT:
        gen_expr(node->lhs);
        println("    pop %%rax");
        return;
    }
    error("Invalid statement.");
}

void gen_expr(Node *node)
{
    println("    .loc 1 %d", node->tok->line_no);
    switch (node->kind)
    {
    case ND_NUM:
        println("    push $%d", node->val);
        return;
    case ND_VAR:
    case ND_MEMBER:
        gen_addr(node);
        println("    pop %%rax");
        load(node->ty);
        println("    push %%rax");
        return;
    case ND_DEREF:
        gen_expr(node->lhs);
        println("    pop %%rax");
        load(node->ty);
        println("    push %%rax");
        return;
    case ND_ADDR:
        gen_addr(node->lhs);
        return;
    case ND_NEG:
        gen_expr(node->lhs);
        println("    pop %%rax");
        println("    neg %%rax");
        println("    push %%rax");
        return;
    case ND_ASSIGN:
        gen_addr(node->lhs);
        gen_expr(node->rhs);

        println("    pop %%rdi");
        println("    pop %%rax");
        store(node->ty);
        println("    push %%rdi");
        return;
    case ND_STMT_EXPR:
        for (Node *n = node->body; n; n = n->next)
        {
            gen_stmt(n);
        }
        println("    push %%rax");
        return;
    case ND_FUNCALL:
    {
        int nargs = 0;
        for (Node *arg = node->args; arg; arg = arg->next)
        {
            gen_expr(arg);
            println("    pop %%rax");
            println("    push %%rax");
            nargs++;
        }

        for (int i = nargs - 1; i >= 0; i--)
        {
            println("    pop %s", argreg64[i]);
        }
        println("    mov $0, %%rax");
        println("    call %s", node->funcname);
        println("    push %%rax");
        return;
    }
    }

    // First compute lhs and rhs, top 2 values on stack shuold be rhs, lhs, ...
    gen_expr(node->lhs);
    gen_expr(node->rhs);

    println("    pop %%rdi");
    println("    pop %%rax");

    char *ax, *di;

    if (node->lhs->ty->kind == TY_LONG || node->lhs->ty->base)
    {
        ax = "%rax";
        di = "%rdi";
    }
    else
    {
        ax = "%eax";
        di = "%edi";
    }

    switch (node->kind)
    {
    case ND_ADD:
        println("    add %s, %s", di, ax);
        break;
    case ND_SUB:
        println("    sub %s, %s", di, ax);
        break;
    case ND_MUL:
        println("    imul %s, %s", di, ax);
        break;
    case ND_DIV:
        if (node->lhs->ty->size == 8)
        {
            println("   cqo");
        }
        else
        {
            println("   cdq");
        }
        println("    idiv %s", di);
        break;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
        println("    cmp %s, %s", di, ax);
        if (node->kind == ND_EQ)
        {
            println("    sete %%al");
        }
        else if (node->kind == ND_NE)
        {
            println("    setne %%al");
        }
        else if (node->kind == ND_LT)
        {
            println("    setl %%al");
        }
        else if (node->kind == ND_LE)
        {
            println("    setle %%al");
        }
        println("    movzb %%al, %%rax");
    }

    println("    push %%rax");
}

void gen_addr(Node *node)
{
    switch (node->kind)
    {
    case ND_VAR:
    {
        if (node->var->is_local)
        {
            // Local variable
            println("    lea -%d(%%rbp), %%rax", node->var->offset);
        }
        else
        {
            // Global variable
            println("    lea %s(%%rip), %%rax", node->var->name);
        }
        println("    push %%rax");
        return;
    }
    case ND_MEMBER:
        gen_addr(node->lhs);
        println("    pop %%rax");
        println("    add $%d, %%rax", node->member->offset);
        println("    push %%rax");
        return;
    case ND_DEREF:
        gen_expr(node->lhs);
        return;
    }
}

void assign_lvar_offsets(Obj *prog)
{
    for (Obj *func = prog; func; func = func->next)
    {
        if (!func->is_function)
        {
            continue;
        }
        int offset = 0;
        for (Obj *var = func->locals; var; var = var->next)
        {
            offset += var->ty->size;
            offset = align_to(offset, var->ty->align);
            var->offset = offset;
        }
        func->stack_size = align_to(offset, 16);
    }
}

int align_to(int offset, int align)
{
    return (offset + align - 1) / align * align;
}