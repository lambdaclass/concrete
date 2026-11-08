### Differential failure precedence and admission

Track native failure, timeout and completion independently, collect completed evaluator observations
before every exit, and atomically publish evaluator completion. Build and observer setup failures
stay FAIL even alongside timeouts. BIR admission depends on the generated supported fragment and
well-formedness; backend emission refusal fails with saved replay metadata. Focused controls cover
both candidate loops, mismatch preservation under control timeout, and a selective emission refusal
that leaves yield and aggregate construct coverage intact.
