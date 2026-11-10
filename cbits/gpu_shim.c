#include <stddef.h>
#include <stdint.h>

#include "TracyC.h"

void hs_tracy_emit_gpu_zone_begin(const struct ___tracy_source_location_data* srcloc, uint16_t queryId, uint8_t context)
{
    struct ___tracy_gpu_zone_begin_data data = { .srcloc = (uint64_t)(uintptr_t)srcloc, .queryId = queryId, .context = context };
    ___tracy_emit_gpu_zone_begin(data);
}

void hs_tracy_emit_gpu_zone_begin_callstack(const struct ___tracy_source_location_data* srcloc, int32_t depth, uint16_t queryId, uint8_t context)
{
    struct ___tracy_gpu_zone_begin_callstack_data data = { .srcloc = (uint64_t)(uintptr_t)srcloc, .depth = depth, .queryId = queryId, .context = context };
    ___tracy_emit_gpu_zone_begin_callstack(data);
}

void hs_tracy_emit_gpu_zone_begin_alloc(uint64_t srcloc, uint16_t queryId, uint8_t context)
{
    struct ___tracy_gpu_zone_begin_data data = { .srcloc = srcloc, .queryId = queryId, .context = context };
    ___tracy_emit_gpu_zone_begin_alloc(data);
}

void hs_tracy_emit_gpu_zone_begin_alloc_callstack(uint64_t srcloc, int32_t depth, uint16_t queryId, uint8_t context)
{
    struct ___tracy_gpu_zone_begin_callstack_data data = { .srcloc = srcloc, .depth = depth, .queryId = queryId, .context = context };
    ___tracy_emit_gpu_zone_begin_alloc_callstack(data);
}

void hs_tracy_emit_gpu_zone_end(uint16_t queryId, uint8_t context)
{
    struct ___tracy_gpu_zone_end_data data = { .queryId = queryId, .context = context };
    ___tracy_emit_gpu_zone_end(data);
}

void hs_tracy_emit_gpu_time(int64_t gpuTime, uint16_t queryId, uint8_t context)
{
    struct ___tracy_gpu_time_data data = { .gpuTime = gpuTime, .queryId = queryId, .context = context };
    ___tracy_emit_gpu_time(data);
}

void hs_tracy_emit_gpu_new_context(int64_t gpuTime, float period, uint8_t context, uint8_t flags, uint8_t type)
{
    struct ___tracy_gpu_new_context_data data = { .gpuTime = gpuTime, .period = period, .context = context, .flags = flags, .type = type };
    ___tracy_emit_gpu_new_context(data);
}

void hs_tracy_emit_gpu_context_name(uint8_t context, const char* name, uint16_t len)
{
    struct ___tracy_gpu_context_name_data data = { .context = context, .name = name, .len = len };
    ___tracy_emit_gpu_context_name(data);
}

void hs_tracy_emit_gpu_calibration(int64_t gpuTime, int64_t cpuDelta, uint8_t context)
{
    struct ___tracy_gpu_calibration_data data = { .gpuTime = gpuTime, .cpuDelta = cpuDelta, .context = context };
    ___tracy_emit_gpu_calibration(data);
}

void hs_tracy_emit_gpu_time_sync(int64_t gpuTime, uint8_t context)
{
    struct ___tracy_gpu_time_sync_data data = { .gpuTime = gpuTime, .context = context };
    ___tracy_emit_gpu_time_sync(data);
}

void hs_tracy_emit_gpu_zone_begin_serial(const struct ___tracy_source_location_data* srcloc, uint16_t queryId, uint8_t context)
{
    struct ___tracy_gpu_zone_begin_data data = { .srcloc = (uint64_t)(uintptr_t)srcloc, .queryId = queryId, .context = context };
    ___tracy_emit_gpu_zone_begin_serial(data);
}

void hs_tracy_emit_gpu_zone_begin_callstack_serial(const struct ___tracy_source_location_data* srcloc, int32_t depth, uint16_t queryId, uint8_t context)
{
    struct ___tracy_gpu_zone_begin_callstack_data data = { .srcloc = (uint64_t)(uintptr_t)srcloc, .depth = depth, .queryId = queryId, .context = context };
    ___tracy_emit_gpu_zone_begin_callstack_serial(data);
}

void hs_tracy_emit_gpu_zone_begin_alloc_serial(uint64_t srcloc, uint16_t queryId, uint8_t context)
{
    struct ___tracy_gpu_zone_begin_data data = { .srcloc = srcloc, .queryId = queryId, .context = context };
    ___tracy_emit_gpu_zone_begin_alloc_serial(data);
}

void hs_tracy_emit_gpu_zone_begin_alloc_callstack_serial(uint64_t srcloc, int32_t depth, uint16_t queryId, uint8_t context)
{
    struct ___tracy_gpu_zone_begin_callstack_data data = { .srcloc = srcloc, .depth = depth, .queryId = queryId, .context = context };
    ___tracy_emit_gpu_zone_begin_alloc_callstack_serial(data);
}

void hs_tracy_emit_gpu_zone_end_serial(uint16_t queryId, uint8_t context)
{
    struct ___tracy_gpu_zone_end_data data = { .queryId = queryId, .context = context };
    ___tracy_emit_gpu_zone_end_serial(data);
}

void hs_tracy_emit_gpu_time_serial(int64_t gpuTime, uint16_t queryId, uint8_t context)
{
    struct ___tracy_gpu_time_data data = { .gpuTime = gpuTime, .queryId = queryId, .context = context };
    ___tracy_emit_gpu_time_serial(data);
}

void hs_tracy_emit_gpu_new_context_serial(int64_t gpuTime, float period, uint8_t context, uint8_t flags, uint8_t type)
{
    struct ___tracy_gpu_new_context_data data = { .gpuTime = gpuTime, .period = period, .context = context, .flags = flags, .type = type };
    ___tracy_emit_gpu_new_context_serial(data);
}

void hs_tracy_emit_gpu_context_name_serial(uint8_t context, const char* name, uint16_t len)
{
    struct ___tracy_gpu_context_name_data data = { .context = context, .name = name, .len = len };
    ___tracy_emit_gpu_context_name_serial(data);
}

void hs_tracy_emit_gpu_calibration_serial(int64_t gpuTime, int64_t cpuDelta, uint8_t context)
{
    struct ___tracy_gpu_calibration_data data = { .gpuTime = gpuTime, .cpuDelta = cpuDelta, .context = context };
    ___tracy_emit_gpu_calibration_serial(data);
}

void hs_tracy_emit_gpu_time_sync_serial(int64_t gpuTime, uint8_t context)
{
    struct ___tracy_gpu_time_sync_data data = { .gpuTime = gpuTime, .context = context };
    ___tracy_emit_gpu_time_sync_serial(data);
}
