<!---
  Copyright 2023 Fred Dushin <fred@dushin.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# `atomvm_rebar3_plugin` Update Instructions

## (unreleased)

- Pico 2 (RP2350) devices are recognized and now work with default parameters. Specifying device
path and uf2 flavor for these chipsets is no longer necessary.
- The `esp32_flash` task now reads the application `offset` from the partition table on the device.
If you are using a custom partition table that does not use `main.avm` for the application partition
name you should supply the name used with the `app_partition` parameter. An `offset` may optionally
be supplied to assure the offset of the application partition matches the expected offset, this may
be helpful to assure that specific applications are only flashed to devices with a custom build of
AtomVM.
- The `esp32_flash` task now auto-discovers any attached ESP32 device if the `port` is omitted.
Previously this was hard-coded to `/dev/ttyUSB0`, which did not match many devices or work at all on
MacOS or other platforms.

## 0.6.* -> 0.7.*

- The `atomvm_rebar3_plugin` tasks have been moved into the `atomvm` namespace (from the [`rebar3`](https://rebar3.org) `default` namespace).  The "legacy" tasks in the `default` namespace are deprecated, and users will be issued a warning when used.  Be sure to use the `atomvm` namespace in any future usage of this plugin, as the deprecated tasks may be removed without warning.  E.g., `rebar3 atomvm packbeam ...`

- The default behavior of not generating line number information in BEAM files has changed.  By default, line number information will be generated in BEAM files.  You can remove line number information using from BEAM files by using the `-r` (or `--remove_lines`) flags to the `packbeam` task.  Note that in versions 0.6 of this tool, the `--include_lines` flag was ignored due to a bug in the code.
