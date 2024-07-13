# Changelog

## [0.4.3](https://github.com/Walheimat/dinghy/compare/v0.4.2...v0.4.3) (2024-07-13)


### Bug Fixes

* **cask:** set distinct Emacs as env instead ([fea4851](https://github.com/Walheimat/dinghy/commit/fea485126265c01bf31f2312ef135bfd71dc4212))

## [0.4.2](https://github.com/Walheimat/dinghy/compare/v0.4.1...v0.4.2) (2024-02-10)


### Bug Fixes

* **emacs:** also update Makefile by default ([f01bd73](https://github.com/Walheimat/dinghy/commit/f01bd73c34130baec1cc7a3c844522686dbee319))
* **tests:** provide coverage for both files ([27f18b9](https://github.com/Walheimat/dinghy/commit/27f18b979f8712c25b68d1322307347acd4244ad))

## [0.4.1](https://github.com/Walheimat/dinghy/compare/v0.4.0...v0.4.1) (2024-01-28)


### Features

* **ci:** add semantic-release goal ([3848217](https://github.com/Walheimat/dinghy/commit/3848217b7d006399218e52509caa5c439f9a6cdf))
* **makefile:** add goal update-next-version ([bd78359](https://github.com/Walheimat/dinghy/commit/bd783595fe1e5d7d520ff4ad1ecb5c1b56ad6695))
* **outline:** use compatible comment headings ([b9852fd](https://github.com/Walheimat/dinghy/commit/b9852fde2222d4f72653594f9f2f9f7f16675948))
* **update-version:** update changelog file ([ef8efe3](https://github.com/Walheimat/dinghy/commit/ef8efe32eee66e18fff5b35fd59aba09206e4cd4))

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Functionality from by package `bydi` to setup paths for tests and
  other testing libraries was moved here under `dinghy-rope`.
- Goal `update-version` now also updates a changelog file if it has
  the current changes under a matching heading. This is "0.4.1"
  by default, and the changelog file is CHANGELOG.md. These can be
  customized using `UPDATE_VERSION_CHANGELOG_FILE` and
  `UPDATE_VERSION_CHANGELOG_HEADING`.

### Changed

- Directory `scripts` was renamed to `src`. It is set by
  `DINGHY_SRC_DIR`.

## [v0.3.0]

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
