#include "nerocc.h"

int main(int argc, char **argv)
{
    if (argc != 2)
    {
        fprintf(stderr, "%s: invalid number of arguments\n", argv[0]);
        return 1;
    }

    Token *tok = tokenize(argv[1]);
    Node *node = expr(&tok, tok);

    if (tok->kind != TK_EOF)
    {
        error("Extra token exists");
    }

    // Code generation
    printf(".intel_syntax noprefix\n");
    printf(".globl main\n");
    printf("main:\n");

    // Traverse AST to emit assembly.
    gen_code(node);

    printf("    pop rax\n");
    printf("    ret\n");
    return 0;
}