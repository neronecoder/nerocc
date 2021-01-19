#include "nerocc.h"

int main(int argc, char **argv)
{
    if (argc != 2)
    {
        fprintf(stderr, "%s: invalid number of arguments\n", argv[0]);
        return 1;
    }

    Token *tok = tokenize(argv[1]);
    Node *node = parse(tok);

    // Traverse AST to emit assembly.
    gen_code(node);
    return 0;
}