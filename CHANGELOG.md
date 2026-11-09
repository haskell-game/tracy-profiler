# Changelog for `tracy-profiler`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.2.0.0 - 2026-11-09

Now targets Tracy 0.15. The library, `tracy-capture` and `tracy-profiler` must all come from that release.

- **Breaking:** removed the `secure` argument from `System.Tracy.Memory.alloc`, `free`, `allocNamed` and `freeNamed`.
  Tracy dropped the distinction and always uses the secure code path.
- **Breaking:** the matching `System.Tracy.FFI.emitMemory*` imports lost their `secure` argument.
- **Breaking:** `System.Tracy.FFI.emitMessage`, `emitMessageL`, `emitMessageC` and `emitMessageLC` are replaced by `emitLogString` and `emitLogStringL`, following the upstream API.
  The `System.Tracy.message*` wrappers keep their types and log at the info severity.
- **Breaking:** raised the `base` lower bound to 4.18, the first release with `Foreign.C.ConstPtr`.
- Added `MessageSeverity` to `System.Tracy.FFI.Types`.
- Text given to `message`, `messageC`, `appInfo`, `Zone.text` and `Zone.name` is now cut at 65534 bytes instead of tripping a Tracy assertion.
  The cut may split a multibyte character.
- `on_demand` builds now go through a small C shim, because since Tracy 0.14 the zone context in that mode has a size the GHC FFI can't pass by value.
  `TracyCZoneCtx` has a different representation under that flag.
- Fixed `fiberLeave` not compiling with `fibers` and `enable` both set.
- Zones now work on the non-threaded RTS without `zones_unsafe`.
  The bound-thread check only applies when the runtime has bound threads.
  Zones from different Haskell threads must not overlap there, or the capture will fail.
- Fixed the Makefile build paths and pinned its clone to the supported Tracy release.
- The Makefile now builds the client library in release mode.
  Existing checkouts need `make clean && make all` to pick this up.

## 0.1.2.1 - 2025-11-02

- Fix `-enable` build without having libtracyclient installed.

## 0.1.2.0 - 2025-11-01

- Fixed byte order in IsLabel Color instance.
- Move library dependencies under a flag.
  The package takes extra effort to remove traces of itself unless enabled.
- Added a safety net around zones.
  Controlled with flags: zones_pedantic (disable to silently skip) and zones_unsafe (enable to remove the check).
- Added `withSrcLoc` which avoids CPP.
- Renamed `withSrcLoc_` to `withSrcLocImpl`, don't use.
- `Added System.Tracy.Memory` wrappers (sans the callback variants).

## 0.1.1.0 - 2025-10-30

- Fixes for copypasta in `#ifndef TRACE_ENABLE` sections.
- `withProfiler` and `withSrcLoc_` now require `MonadUnliftIO`.

## 0.1.0.0 - 2025-10-30

Initial release.
