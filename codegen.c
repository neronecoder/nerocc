#include "nerocc.h"

static int count()
{
    static int i = 1;
    return i++;
}

void gen_code(Function *prog)
{
    assign_lvar_offsets(prog);

    // required statements
    printf(".globl main\n");
    printf("main:\n");

    // Prologue
    printf("    push %%rbp\n");
    printf("    mov %%rsp, %%rbp\n");
    printf("    sub $%d, %%rsp\n", prog->stack_size);

    gen_stmt(prog->body);

    // Jump here with return
    printf(".L.return:\n");
    // Epilogue
    printf("    mov %%rbp, %%rsp\n");
    printf("    pop %%rbp\n");
    printf("    ret\n");
}

void gen_stmt(Node *node)
{
    switch (node->kind)
    {
    case ND_RETURN:
        gen_expr(node->lhs);
        printf("    pop %%rax\n");
        printf("    jmp .L.return\n");
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
        printf("    pop %%rax\n");
        printf("    cmp $0, %%rax\n");
        printf("    je  .L.else.%d\n", c);
        gen_stmt(node->then);
        printf("    pop %%rax\n");
        printf("    jmp .L.end.%d\n", c);
        printf(".L.else.%d:\n", c);

        if (node->els)
        {
            gen_stmt(node->els);
            printf("pop %%rax\n");
        }
        printf(".L.end.%d:\n", c);
        return;
    }

    case ND_FOR:
    {
        int c = count();
        gen_stmt(node->init);
        printf(".L.begin.%d:\n", c);
        if (node->cond)
        {
            gen_expr(node->cond);
            printf("    pop %%rax\n");
            printf("    cmp $0, %%rax\n");
            printf("    je  .L.end.%d\n", c);
        }
        gen_stmt(node->then);
        if (node->inc)
        {
            gen_expr(node->inc);
            printf("    pop %%rax\n");
        }
        printf("    jmp .L.begin.%d\n", c);
        printf(".L.end.%d:\n", c);
        return;
    }
    case ND_WHILE:
    {
        int c = count();
        printf(".L.begin.%d:\n", c);
        gen_expr(node->cond);
        printf("    pop %%rax\n");
        printf("    cmp $0, %%rax\n");
        printf("    je  .L.end.%d\n", c);

        gen_stmt(node->then);
        printf("    jmp .L.begin.%d\n", c);
        printf(".L.end.%d:\n", c);
        return;
    }
    case ND_EXPR_STMT:
        gen_expr(node->lhs);
        printf("    pop %%rax\n");
        return;
    }
    error("Invalid statement.");
}

void gen_expr(Node *node)
{
    switch (node->kind)
    {
    case ND_NUM:
        printf("    push $%d\n", node->val);
        return;
    case ND_VAR:
        gen_lval(node);
        printf("    pop %%rax\n");
        printf("    mov (%%rax), %%rax\n");
        printf("    push %%rax\n");
        return;
    case ND_NEG:
        gen_expr(node->lhs);
        printf("    pop %%rax\n");
        printf("    neg %%rax\n");
        printf("    push %%rax\n");
        return;
    case ND_ASSIGN:
        gen_lval(node->lhs);
        gen_expr(node->rhs);

        printf("    pop %%rdi\n");
        printf("    pop %%rax\n");
        printf("    mov %%rdi, (%%rax)\n");
        printf("    push %%rdi\n");
        return;
    }

    // First compute lhs and rhs, top 2 values on stack shuold be rhs, lhs, ...
    gen_expr(node->lhs);
    gen_expr(node->rhs);

    printf("    pop %%rdi\n");
    printf("    pop %%rax\n");

    switch (node->kind)
    {
    case ND_ADD:
        printf("    add %%rdi, %%rax\n");
        break;
    case ND_SUB:
        printf("    sub %%rdi, %%rax\n");
        break;
    case ND_MUL:
        printf("    imul %%rdi, %%rax\n");
        break;
    case ND_DIV:
        printf("    cqo\n");
        printf("    idiv %%rdi\n");
        break;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
        printf("    cmp %%rdi, %%rax\n");
        if (node->kind == ND_EQ)
        {
            printf("    sete %%al\n");
        }
        else if (node->kind == ND_NE)
        {
            printf("    setne %%al\n");
        }
        else if (node->kind == ND_LT)
        {
            printf("    setl %%al\n");
        }
        else if (node->kind == ND_LE)
        {
            printf("    setle %%al\n");
        }
        printf("    movzb %%al, %%rax\n");
    }

    printf("    push %%rax\n");
}

void gen_lval(Node *node)
{
    if (node->kind != ND_VAR)
    {
        error("Left hand side of the assignment is not a variable.");
    }
    printf("    mov %%rbp, %%rax\n");
    printf("    sub $%d, %%rax\n", node->var->offset);
    printf("    push %%rax\n");
}

void assign_lvar_offsets(Function *prog)
{
    int offset = 0;
    for (Var *var = prog->locals; var; var = var->next)
    {
        offset += 8;
        var->offset = offset;
    }
    prog->stack_size = align_to(offset, 16);
}

int align_to(int offset, int align)
{
    return (offset + align - 1) / align * align;
}