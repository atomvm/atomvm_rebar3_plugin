# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.7.3] (unreleased)

- Added support for compiling "bootstrap" erlang files that `rebar3` otherwise cannot compile.

## [0.7.2] (2023.10.24)

- Updated to depend on `atomvm_packbeam` version `0.7.1`, to make use of `packbeam_api` changes.
- Added tests for `packbeam`, `esp32`, and `stm32` tasks.
- Generate `ex_doc` documentation instead of `edoc`.
- Added `version` task to print the version of the plugin to the console

## [0.7.1] (2023.10.18)

- Fixed a bug whereby a missing `atomvm_rebar3_plugin` entry in `rebar.config` would crash the `packbeam` task.

## [0.7.0] (2023.10.18)

- Moved atomvm tasks under the `atomvm` namespace (with support for deprecated tasks in the default namespace)
- Added `utf2create` and `pico_flash` tasks, for Raspberry Pico support
- Added support for setting options in `rebar.config`
- Added `--list` (`-l`) option to `packbeam` to to display contents of generated AVM files.

## [0.6.1] (2023.07.16)

### Added

- Added `stm32_flash` rebar3 task
- Added `-r|--remove_lines` command line option to `packbeam` task

### Changed
- Updated dependency on `atomvm_packbeam` 0.6 or later
- Changed default to not remove lines from generated AVM files

## [0.6.0] (2022.12.18)

### Added
- Added ability to include `<<"Line">>` chunks in BEAM files in generated AVM files

### Changed
- Updated dependency on `atomvm_packbeam` 0.6.0

## [0.5.1] (2022.08.31)

### Fixed
- Fixed Hex dependency on atomvm_packbeam 0.5.0

## [0.5.0] (2022.08.28)

### Added
- Added packing of application bin file to packbeam file.

## [0.4.1] (2022.06.19)

### Changed
- Updated dependency on `atomvm_packbeam` 0.4.1

## [0.4.0] (2022.05.21)

### Added
- Added `erlfmt` plugin and formatted code.
- Added `--chip` option to `esp32_flash` task

### Fixed
- Fixed a bug that prevented files in directories inside of the `priv` directory to be included in packbeam files.

## [0.3.0] (2022.05.18)

### Changed
- Updated dependency on `atomvm_packbeam` `0.3.0`

## [0.2.0] (?)

### Added
- Added plugin template for generating applications
- Added support for deployment to hex
- Added support for `--start` flag

### Changed
- Updated default flash location

## [0.1.0] (2020.05.17)
- Initial Release
