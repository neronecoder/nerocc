#include "nerocc.h"

static char *opt_o;
static char *input_path;

void usage(int status)
{
    fprintf(stderr, "nerocc [ -o <path> ] <file>\n");
    exit(status);
}

void parse_args(int argc, char **argv)
{
    for (int i = 1; i < argc; i++)
    {
        if (!strcmp(argv[i], "--help") || !strcmp(argv[i], "-h"))
        {
            usage(0);
        }

        if (!strcmp(argv[i], "-o"))
        {
            if (!argv[++i])
            {
                usage(1);
            }
            opt_o = argv[i];
            continue;
        }

        if (!strncmp(argv[i], "-o", 2))
        {
            opt_o = argv[i] + 2;
            continue;
        }

        if (argv[i][0] == '-' && argv[i][1] != '\0')
        {
            error("Unknown argument: %s", argv[i]);
        }

        input_path = argv[i];
    }

    if (!input_path)
    {
        error("No input files");
    }
}

FILE *open_file(char *path)
{
    if (!path || strcmp(path, "-") == 0)
    {
        return stdout;
    }

    FILE *out = fopen(path, "w");
    if (!out)
    {
        error("Cannot open output file: %s: %s", path, strerror(errno));
    }
    return out;
}

int main(int argc, char **argv)
{
    parse_args(argc, argv);

    Token *tok = tokenize_file(input_path);
    Obj *prog = parse(tok);

    // Traverse AST to emit assembly.
    FILE *out = open_file(opt_o);
    fprintf(out, ".file 1 \"%s\"\n", input_path);
    gen_code(prog, out);
    return 0;
}