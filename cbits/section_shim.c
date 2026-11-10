#include <stddef.h>
#include <stdint.h>

#include "TracyC.h"

uint32_t hs_tracy_section_enter(uint16_t category, const char* text, size_t size)
{
    return ___tracy_section_enter(category, "%.*s", (int)size, text ? text : "");
}

void hs_tracy_section_setup(uint16_t category, const char* text, size_t size)
{
    ___tracy_section_setup(category, "%.*s", (int)size, text ? text : "");
}
