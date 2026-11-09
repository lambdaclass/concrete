The spec/compiler differential now journals candidate stage entry and completion, catches
ordinary driver IO exceptions with candidate context, and reconciles unmarked abnormal worker
exits before later timeouts or cleanup. Durable bundles retain all available stage artifacts.
Controls cover lowering, emission and assembly exits in batch and replay, including failure
followed by timeout and standalone saved-command replay. Retry PID boundaries prevent an
interrupted attempt from contaminating a completed retry. No language rule changes.
