### Preserve pending call loans across mixed issuance

Ordinary issuance refuses to end a loan queued as a call argument.
Every legal issuance step preserves the ownership ledger and pending
buffer together. Downward lending inserts one pending child, and
standalone parameter binding uses the identity issued for that child.

Issuance steps and traces have unique successors by inversion of the
legal rules. The exact transition laws now have accepted instances and
kernel-checked controls for both false acceptance and false refusal.
The landing inventory pins those statements and controls.
