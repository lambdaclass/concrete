/*
 * cgrep_c -- a grep-like tool in C
 *
 * Usage: cgrep_c [flags] pattern file
 *   -n  print line numbers
 *   -c  count matching lines only
 *   -v  invert match (print non-matching lines)
 *   -i  case-insensitive matching
 *
 * Roughly mirrors the Concrete cgrep implementation:
 * reads the entire file, splits into lines, does substring search.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Case-insensitive substring search (no external libs). */
static const char *strcasestr_local(const char *haystack, const char *needle)
{
    if (!*needle)
        return haystack;
    size_t nlen = strlen(needle);
    for (; *haystack; haystack++) {
        if (strncasecmp(haystack, needle, nlen) == 0)
            return haystack;
    }
    return NULL;
}

static int grep_file(const char *filename, const char *pattern,
                     int show_numbers, int count_only,
                     int invert, int case_insensitive,
                     int show_filename)
{
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        fprintf(stderr, "cgrep_c: cannot open '%s'\n", filename);
        return -1;
    }

    /* Read entire file into memory, matching Concrete's read_to_string. */
    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    rewind(fp);

    char *buf = malloc((size_t)fsize + 1);
    if (!buf) {
        fclose(fp);
        fprintf(stderr, "cgrep_c: out of memory\n");
        return -1;
    }
    size_t nread = fread(buf, 1, (size_t)fsize, fp);
    buf[nread] = '\0';
    fclose(fp);

    int match_count = 0;
    int line_num = 1;
    char *line_start = buf;

    for (char *p = buf; ; p++) {
        if (*p == '\n' || *p == '\0') {
            char saved = *p;
            *p = '\0';  /* terminate the line in-place */

            const char *found;
            if (case_insensitive)
                found = strcasestr_local(line_start, pattern);
            else
                found = strstr(line_start, pattern);

            int matched = (found != NULL);
            int should_print = (matched && !invert) || (!matched && invert);

            if (should_print) {
                match_count++;
                if (!count_only) {
                    if (show_filename)
                        printf("%s:", filename);
                    if (show_numbers)
                        printf("%d:", line_num);
                    printf("%s\n", line_start);
                }
            }

            if (saved == '\0')
                break;

            line_num++;
            line_start = p + 1;
        }
    }

    if (count_only) {
        if (show_filename)
            printf("%s:", filename);
        printf("%d\n", match_count);
    }

    free(buf);
    return match_count;
}

int main(int argc, char **argv)
{
    int show_numbers = 0;
    int count_only   = 0;
    int invert       = 0;
    int case_insens  = 0;

    int argi = 1;

    /* Parse flags -- each flag is a separate argument like -n, -c, etc. */
    while (argi < argc && argv[argi][0] == '-' && strlen(argv[argi]) >= 2) {
        const char *flag = argv[argi];
        for (int j = 1; flag[j]; j++) {
            switch (flag[j]) {
            case 'n': show_numbers = 1; break;
            case 'c': count_only   = 1; break;
            case 'v': invert       = 1; break;
            case 'i': case_insens  = 1; break;
            default:
                fprintf(stderr, "cgrep_c: unknown flag '-%c'\n", flag[j]);
                return 1;
            }
        }
        argi++;
    }

    int remaining = argc - argi;
    if (remaining < 2) {
        fprintf(stderr, "Usage: cgrep_c [-n] [-c] [-v] [-i] <pattern> <file>...\n");
        return 1;
    }

    const char *pattern = argv[argi];
    int num_files = argc - argi - 1;
    int show_filename = (num_files > 1);
    int total_matches = 0;

    for (int fi = argi + 1; fi < argc; fi++) {
        int m = grep_file(argv[fi], pattern, show_numbers, count_only,
                          invert, case_insens, show_filename);
        if (m > 0)
            total_matches += m;
    }

    return (total_matches > 0) ? 0 : 1;
}
