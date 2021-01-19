#include "nerocc.h"

void gen_code(Node *node)
{
    // required statements
    printf(".globl main\n");
    printf("main:\n");

    // Prologue
    printf("    push %%rbp\n");
    printf("    mov %%rsp, %%rbp\n");
    printf("    sub $208, %%rsp\n");

    Node *cur = node;
    while (cur)
    {
        gen_stmt(cur);
        cur = cur->next;
    }

    // Epilogue
    printf("    mov %%rbp, %%rsp\n");
    printf("    pop %%rbp\n");
    printf("    ret\n");
}

void gen_stmt(Node *node)
{
    if (node->kind == ND_EXPR_STMT)
    {
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
    int offset = (node->name - 'a' + 1) * 8;
    printf("    mov %%rbp, %%rax\n");
    printf("    sub $%d, %%rax\n", offset);
    printf("    push %%rax\n");
}