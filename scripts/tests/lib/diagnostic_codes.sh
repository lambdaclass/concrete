# Bug 206: one definition of "a rendered error diagnostic without a code", shared by run_tests.sh
# (applied to the output of every run_err fixture) and check_diagnostic_codes_present.sh (its
# negative controls). The human renderer prints `<file>: error[<pass>]: (E1234) <message>`.
#
# codeless_error_lines: read compiler output on stdin, print each error line that carries no
# `(E<4 digits>)` code right after its `error[<pass>]:` prefix. Prints nothing when all are coded.
codeless_error_lines() {
  grep -E 'error\[[a-z0-9-]+\]: ' | grep -vE 'error\[[a-z0-9-]+\]: \(E[0-9]{4}\) ' || true
}
