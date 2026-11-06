# Bug 206: one definition of "a rendered error diagnostic without a code", shared by run_tests.sh
# (applied to the output of every run_err fixture) and check_diagnostic_codes_present.sh (its
# negative controls). The human renderer prints `<file>: error[<pass>]: (E1234) <message>`.
#
# codeless_error_lines: read compiler output on stdin, print each error line that carries no
# `(E<4 digits>)` code right after its `error[<pass>]:` prefix. Prints nothing when all are coded.
# A header is the line's FIRST `error[<pass>]: `, either at the start of the line or after a
# `<path>.con[:line[:col]]: ` prefix (the path may hold spaces), so a source snippet (`  3 | ...`) or
# a message that quotes another header is not read as one. The code must follow that first one.
codeless_error_lines() {
  awk '{
    i = index($0, "error[");
    if (i == 0) next;
    pre = substr($0, 1, i - 1); rest = substr($0, i);
    if (pre != "" && pre !~ /\.con(:[0-9]+(:[0-9]+)?)?: $/) next;
    # the renderer source lines (` 3 | ...`) and context lines (`  = ...`) are not headers; a
    # header prefix is the file path verbatim, so nothing else is excluded
    if (pre ~ /^ *[0-9]+ *\|/ || pre ~ /^ += /) next;
    if (rest !~ /^error\[[a-z0-9-]+\]: /) next;
    sub(/^error\[[a-z0-9-]+\]: /, "", rest);
    if (rest !~ /^\(E[0-9][0-9][0-9][0-9]\) /) print
  }'
}
