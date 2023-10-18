<!---
  Copyright 2023 Fred Dushin <fred@dushin.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# `atomvm_rebar3_plugin` Update Instructions

## 0.6.* -> 0.7.*

- The `atomvm_rebar3_plugin` tasks have been moved into the `atomvm` namespace (from the [`rebar3`](https://rebar3.org) `default` namespace).  The "legacy" tasks in the `default` namespace are deprecated, and users will be issued a warning when used.  Be sure to use the `atomvm` namespace in any future usage of this plugin, as the deprecated tasks may be removed without warning.  E.g., `rebar3 atomvm packbeam ...``
- The default behavior of not generating line number information in BEAM files has changed.  By default, line number information will be generated in BEAM files.  You can remove line number information using from BEAM files by using the `-r` (or `--remove_lines`) flags to the `packbeam` task.  Note that in versions 0.6 of this tool, the `--include_lines` flag was ignored due to a bug in the code.
