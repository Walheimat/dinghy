# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v0.2.1]

### Added

- `emacs-package.mk` now exposes `DINGHY_VERSION` that is set to to
  the current version.

### Changed

- The `undercover` text result is no longer `cat`ed to stdout.
- The dist goal no longer runs `cask build` as it isn't necessary.

## [v0.2.0]

### Added

- Usage section to README.
- Goal `pacify` that uses `dinghy-pacify` package (extracted from my
  configuration).
- A GitHub workflow.

### Changed

- Variable `UPDATE_VERSION_DEPS` was renamed to `UPDATE_VERSION_FILES`
  and now defaults to `Cask` and the package file.
- Script `update-version` no longer uses the last two tags; it is now
  called with the value of `CURRENT_PACKAGE_VERSION` set in the
  Makefile as the target version. It now loops over all files
  specified in `UPDATE_VERSION_FILES`.
- Goal `local-test` was removed in favor of a conditional in `test`
  that will `cat` the coverage results if `CI` is not set and the file
  exists.

## [v0.1.0]

Initial version as an extraction of my Emacs packages.
