#include <stddef.h>
#include <stdint.h>

#include "TracyC.h"

static void pack(TracyCZoneCtx ctx, uint64_t* out)
{
    out[0] = (uint64_t)ctx.id | ((uint64_t)(uint32_t)ctx.active << 32);
    out[1] = ctx.connectionId;
}

static TracyCZoneCtx unpack(uint64_t idAndActive, uint64_t connectionId)
{
    TracyCZoneCtx ctx;
    ctx.id = (uint32_t)idAndActive;
    ctx.active = (int32_t)(idAndActive >> 32);
    ctx.connectionId = connectionId;
    return ctx;
}

void hs_tracy_emit_zone_begin(const struct ___tracy_source_location_data* srcloc, int32_t active, uint64_t* out)
{
    pack(___tracy_emit_zone_begin(srcloc, active), out);
}

void hs_tracy_emit_zone_begin_callstack(const struct ___tracy_source_location_data* srcloc, int32_t depth, int32_t active, uint64_t* out)
{
    pack(___tracy_emit_zone_begin_callstack(srcloc, depth, active), out);
}

void hs_tracy_emit_zone_begin_alloc(uint64_t srcloc, int32_t active, uint64_t* out)
{
    pack(___tracy_emit_zone_begin_alloc(srcloc, active), out);
}

void hs_tracy_emit_zone_begin_alloc_callstack(uint64_t srcloc, int32_t depth, int32_t active, uint64_t* out)
{
    pack(___tracy_emit_zone_begin_alloc_callstack(srcloc, depth, active), out);
}

void hs_tracy_emit_zone_end(uint64_t idAndActive, uint64_t connectionId)
{
    ___tracy_emit_zone_end(unpack(idAndActive, connectionId));
}

void hs_tracy_emit_zone_text(uint64_t idAndActive, uint64_t connectionId, const char* txt, size_t size)
{
    ___tracy_emit_zone_text(unpack(idAndActive, connectionId), txt, size);
}

void hs_tracy_emit_zone_name(uint64_t idAndActive, uint64_t connectionId, const char* txt, size_t size)
{
    ___tracy_emit_zone_name(unpack(idAndActive, connectionId), txt, size);
}

void hs_tracy_emit_zone_color(uint64_t idAndActive, uint64_t connectionId, uint32_t color)
{
    ___tracy_emit_zone_color(unpack(idAndActive, connectionId), color);
}

void hs_tracy_emit_zone_value(uint64_t idAndActive, uint64_t connectionId, uint64_t value)
{
    ___tracy_emit_zone_value(unpack(idAndActive, connectionId), value);
}
