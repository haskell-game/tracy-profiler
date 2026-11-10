#include <stddef.h>
#include <stdint.h>

#include "TracyC.h"

#define POINTER_BYTES sizeof(void*)

_Static_assert(offsetof(struct ___tracy_source_location_data, name) == 0, "SourceLocationData name offset");
_Static_assert(offsetof(struct ___tracy_source_location_data, function) == POINTER_BYTES, "SourceLocationData function offset");
_Static_assert(offsetof(struct ___tracy_source_location_data, file) == 2 * POINTER_BYTES, "SourceLocationData file offset");
_Static_assert(offsetof(struct ___tracy_source_location_data, line) == 3 * POINTER_BYTES, "SourceLocationData line offset");
_Static_assert(offsetof(struct ___tracy_source_location_data, color) == 3 * POINTER_BYTES + 4, "SourceLocationData color offset");
_Static_assert(sizeof(struct ___tracy_source_location_data) == 3 * POINTER_BYTES + 8, "SourceLocationData size");
_Static_assert(_Alignof(struct ___tracy_source_location_data) == _Alignof(void*), "SourceLocationData alignment");
