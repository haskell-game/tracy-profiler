# Changelog for `tracy-profiler`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- Fixed byte order in IsLabel Color instance.
- Move library dependencies under a flag.
  The package takes extra effort to remove traces of itself unless enabled.
- Added a safety net around zones.
  Controlled with flags: zones_pedantic (disable to silently skip) and zones_unsafe (enable to remove the check).

## 0.1.1.0 - 2025-10-30

- Fixes for copypasta in `#ifndef TRACE_ENABLE` sections.
- `withProfiler` and `withSrcLoc_` now require `MonadUnliftIO`.

## 0.1.0.0 - 2025-10-30

Initial release.
