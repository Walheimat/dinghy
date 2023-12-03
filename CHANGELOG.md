# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

### [Unreleased]

### Added

- Goal `upgrade-bydi` to remove the package and re-install it.
- Goal `test-tagged` that allows passing `TEST_TAG` to only run tests
  with that tag. The reverse goal `test-not-tagged` was also added.
- Goal `test-selector` that allows passing `TEST_SELECTOR_STRING` to
  only run tests matching that selector string.
- Goal `commits` to setup `commitlint` with `husky` (`clean-commits`
  to remove again).

### Changed

- Tests are now run using `ert-run-tests-batch-and-exit` by default.
  To use `ert-runner`, env `ERT_RUNNER` must be set.
- Goal `coverage` was renamed to `test-coverage`.

### Fixed

- The `ERT_RUN` variant now also loads the test helper first (and
  once) only.

## [v0.2.2]

### Added

- Install goals communicate that they come from `dinghy`.

### Changed

- Package `dinghy-pacify` was moved to the `scripts` directory.
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
