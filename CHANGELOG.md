# Changelog for `tracy-profiler`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.2.0.0 - 2026-11-09

Now targets Tracy 0.15. The library, `tracy-capture` and `tracy-profiler` must all come from that release.

### Breaking

- Removed the `secure` argument from `System.Tracy.Memory.alloc`, `free`, `allocNamed` and `freeNamed`.
  Tracy dropped the distinction and always uses the secure code path.
- The matching `System.Tracy.FFI.emitMemory*` imports lost their `secure` argument.
- `System.Tracy.FFI.emitMessage`, `emitMessageL`, `emitMessageC` and `emitMessageLC` are replaced by `emitLogString` and `emitLogStringL`, following the upstream API.
  The `System.Tracy.message*` wrappers keep their types and log at the info severity.
- `System.Tracy.FFI.TracyCZoneCtx` has a different representation under the `on_demand` flag.
- Raised the `base` lower bound to 4.18, the first release with `Foreign.C.ConstPtr`.

### Added

- `logString` and `logStringL`, which take a `MessageSeverity`.
- `System.Tracy.Section`, `System.Tracy.Lock`, `System.Tracy.SharedLock` and `System.Tracy.SourceLocation`.
- `getTime`, `beginSamplingProfiling`, `endSamplingProfiling` and `System.Tracy.Memory.discard`.
- `enabled`, which tells whether the package was built with the `enable` flag.
- `make DEBUG=1` builds everything in debug mode in `upstream/_build-debug`.

### Fixed

- `on_demand` builds now go through a small C shim, because since Tracy 0.14 the zone context in that mode has a size the GHC FFI can't pass by value.
- Zones now work on the non-threaded RTS without `zones_unsafe`.
  The bound-thread check only applies when the runtime has bound threads.
  Zones from different Haskell threads must not overlap there, or the capture will fail.
- Text given to `message`, `messageC`, `appInfo`, `Zone.text` and `Zone.name` is now cut at 65534 bytes instead of tripping a Tracy assertion.
  The cut may split a multibyte character.
- `fiberLeave` now compiles with `fibers` and `enable` both set.
- `System.Tracy.FFI.Types` no longer goes stale after changing the `enable` or `on_demand` flags in an existing build directory.
- The Makefile's paths now match where it clones Tracy, and the clone is pinned to the supported release.
- The Makefile now builds in release mode by default.
  Its `--config Release` had no effect with CMake's Makefile generator, so the client library was built without optimizations.
- The Makefile now builds the client library without Tracy's crash handler.
  The handler treats `SIGPIPE` as a crash, and GHC's threaded runtime sends that signal to cancel `interruptible` foreign calls, which aborted the program while a profiler was connected.
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
