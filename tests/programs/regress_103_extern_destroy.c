/* C side of regress_103_extern_destroy.con: counts the destroy calls that reach C. */
#include <stdlib.h>
static int destroyed = 0;
void *widget_create(void) { return malloc(1); }
void widget_destroy(void *w) { free(w); destroyed++; }
int widget_destroyed(void) { return destroyed; }
