"""Single typed reduction owner for scheduled execution and qualification."""
from enum import Enum
from collections.abc import Iterable

class Status(str, Enum):
    PASS = 'PASS'
    MISSING = 'MISSING'
    INCOMPLETE = 'INCOMPLETE'
    FAIL = 'FAIL'

    def __str__(self):
        return self.value


def reduce_outcomes(outcomes: Iterable[Status | str], *, empty: Status = Status.PASS) -> Status:
    """FAIL is absorbing, including across attempts; caller chooses absence policy."""
    values = {Status(value) for value in outcomes}
    for status in (Status.FAIL, Status.INCOMPLETE, Status.MISSING, Status.PASS):
        if status in values:
            return status
    return Status(empty)


def execution_status(gates, *, unavailable=False, missing=False) -> Status:
    return reduce_outcomes([*(gate['status'] for gate in gates),
                            *(control['status'] for gate in gates for control in gate.get('controls', [])),
                            *([Status.MISSING] if missing else []),
                            *([Status.INCOMPLETE] if unavailable else [])])
