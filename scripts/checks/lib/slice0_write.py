"""Write stdin to a new file in the private native build directory."""

import sys

# Drain the producer before opening the file so a destination error can be
# reported without breaking the producer's input pipe.
data = sys.stdin.buffer.read()
try:
    with open(sys.argv[1], "xb") as output:
        output.write(data)
        output.flush()
except OSError as error:
    sys.exit(str(error))
