Trace process creation and structurally reject CLONE_UNTRACED and unknown clone
flags. A real child successfully creates executable anonymous memory without any
child mapping record, reproducing the strace -f escape; the parent clone must fail
with its exact unaudited-child diagnostic.
