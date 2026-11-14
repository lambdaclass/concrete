Share the answer-indexed dispatcher transition adapter between AnswerStep, agreement laws
and their removed/added-dispatcher controls. AnswerStep now specializes Dispatches to step;
all transition conditions live in the adapter constructors. Keep the independent Rules and
overlapping-rule controls unchanged. Regenerate the exact full theorem axiom inventory.

Structural cause: a separate agreement adapter duplicated the production relation, allowing
its conditions and evidence to drift. Structural owner: F09 Exec correspondence; specialization
of one adapter now makes production and agreement conditions identical by definition.
No language rule or held owner decision is introduced.
