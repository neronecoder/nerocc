#include "nerocc.h"

void gen_code(Node *node)
{
    switch (node->kind)
    {
    case ND_NUM:
        printf("    push %d\n", node->val);
        return;
    case ND_NEG:
        gen_code(node->lhs);
        printf("    pop rax\n");
        printf("    neg rax\n");
        printf("    push rax\n");
        return;
    }

    // First compute lhs and rhs, top 2 values on stack shuold be rhs, lhs, ...
    gen_code(node->lhs);
    gen_code(node->rhs);

    printf("    pop rdi\n");
    printf("    pop rax\n");

    switch (node->kind)
    {
    case ND_ADD:
        printf("    add rax, rdi\n");
        break;
    case ND_SUB:
        printf("    sub rax, rdi\n");
        break;
    case ND_MUL:
        printf("    imul rax, rdi\n");
        break;
    case ND_DIV:
        printf("    cqo\n");
        printf("    idiv rdi\n");
        break;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
        // cmp s2, s1
        // set flags based on s1 - s2
        printf("    cmp rax, rdi\n");
        if (node->kind == ND_EQ)
        {
            printf("    sete al\n");
        }
        else if (node->kind == ND_NE)
        {
            printf("    setne al\n");
        }
        else if (node->kind == ND_LT)
        {
            printf("    setl al\n");
        }
        else if (node->kind == ND_LE)
        {
            printf("    setle al\n");
        }
        printf("    movzb rax, al\n");
    }

    printf("    push rax\n");
}
