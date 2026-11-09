Gate C now exercises ISS-0042 registry qualification through the shared public
scheduled-evidence validator, collector, append and observer using its actual
committed daily registry, with identical clean/hostile exact-diagnostic controls.
At ac8b47567565ca0e7c008b1b5b88982cb9a19f04 the existing protocol fixture
qualified both its unregistered sample gate and scaling-daily with one invented
control and no artifacts as PASS. After the gate-schedule merge the C controls
refuse those substitutions with `FAIL registry: unregistered gate substitute`
and `FAIL registry: unregistered control scaling-daily/invented` respectively.
The structural owner is R-0676; C keeps no separate evidence parser or verdict.
