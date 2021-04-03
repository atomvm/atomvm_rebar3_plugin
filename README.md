# atomvm_rebar3_plugin

A `rebar3` plugin for simplifying development of Erlang applications targeted for the <a href="http://github.com/bettio/AtomVM">AtomVM</a> Erlang abstract machine.

This `rebar3` plugin provides the following targets:

* `packbeam`  Generate AtomVM packbeam files from your `rebar3` project and its dependencies.
* `esp32_flash`  Flash AtomVM packbeam files to ESP32 devices over a serial connection.

The `atomvm_rebar3_plugin` plugin makes use of the <a href="https://github.com/fadushin/packbeam">packbeam</a> tool, internally.

## Getting Started

Create a `rebar3` project (app or lib), e.g.,

    shell$ rebar3 new lib mylib
    ===> Writing mylib/src/mylib.erl
    ===> Writing mylib/src/mylib.app.src
    ===> Writing mylib/rebar.config
    ===> Writing mylib/.gitignore
    ===> Writing mylib/LICENSE
    ===> Writing mylib/README.md

Add the plugin to the rebar config:

    {plugins, [
        {atomvm_rebar3_plugin, {git, "https://github.com/fadushin/atomvm_rebar3_plugin.git", {branch, "master"}}}
    ]}.

> Note.  Check the latest tag in the [`atomvm_rebar3_plugin`](https://github.com/fadushin/atomvm_rebar3_plugin.git) repository to get the latest version.

Create a file called main.erl in the `src` directory with the contents:

    -module(main).
    -export([start/0]).

    start() ->
        ok.

### `packbeam` target

The `packbeam` target is used to generated an AtomVM packbeam (`.avm`) file.

    shell$ ll
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    A rebar plugin to create packbeam files
    Usage: rebar3 packbeam [-e] [-f] [-p] [-s <start>]

    -e, --external  External AVM modules
    -f, --force     Force rebuild
    -p, --prune     Prune unreferenced BEAM files
    -s, --start     Start module

E.g.,

    shell$ rebar3 packbeam
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    ===> Verifying dependencies...
    ===> Compiling mylib
    ===> AVM file written to : mylib.avm

When using this target, an AVM file with the project name will be created in `_build/<profile>/lib/`, .e.g.,

    shell$ ls -l _build/default/lib/mylib.avm
    -rw-rw-r--  1 frege  wheel  8780 May 15 1895 22:03 _build/default/lib/mylib.avm

If your project has any erlang dependencies, the `packbeam` target will include any BEAM files or priv files from the dependent projects in the final AVM file.

If your project (or any of its dependencies) has multiple modules that export a `start/0` entry-point function, you can specify which module to use as the entry-point for your application via the `-s` (or `--start`) option:

    shell$ rebar3 packbeam -s my_start_module
    ...

Using this option will ensure that the generated AVM file with use `my_start_module` to start the application.

You may use the `-p` option (or `--prune`) to prune unnecessary beam files when creating AVM files.  Pruning unnecessary files can make your AVM files smaller, leading to faster development cycles and more free space on flash media.  Pruning is not enabled by default.  Note that if you use the prune option, your project (or at least one of its dependencies) _must_ have a `start/0` entry-point.  Otherwise, you should treat your project as a library, suitable for inclusion in a different AtomVM project.

The `packbeam` target will use timestamps to determine whether a rebuild is necessary.  However, timestamps may not be enough to trigger a rebuild, for example, if a dependency was added or removed.  You can force a rebuild of AVM file by adding the `-f` flag (or `--force`), with no arguments.  All AVM files, including AVM files for dependencies, will be rebuilt regardless of timestamps.

The `packbeam` target depends on the `compile` target, so any changes to modules in the project will automatically get rebuilt when running the `packbeam` target.

#### External Dependencies

If you already have AVM modules are not available via `rebar3`, you can direct the `packbeam` target to these AVM files via the `-e` (or `--external`) flag, e.g.,

    shell$ rebar3 packbeam -e <path-to-avm-1> -e <path-to-avm-2> ...
    ===> Fetching packbeam (from {git,"https://github.com/fadushin/packbeam.git",
                        {branch,"master"}})
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    ===> Compiling atomvm_rebar3_plugin
    ===> Verifying dependencies...
    ===> Compiling mylib
    ===> AVM file written to : mylib.avm

### `esp32-flash` target

You may use the `esp32_flash` target to flash the generated AtomVM packbeam application to the flash storage on an ESP32 device connected over a serial connection.

    shell$ rebar3 help esp32_flash
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    A rebar plugin to flash packbeam to ESP32 devices
    Usage: rebar3 esp32_flash [-e] [-p] [-b] [-o]

    -e, --esptool  Path to esptool.py
    -p, --port     Device port (default /dev/ttyUSB0)
    -b, --baud     Baud rate (default 115200)
    -o, --offset   Offset (default 0x110000)

The `esp32_flash` will use the `esptool.py` command to flash the ESP32 device.  This tool is available via the <a href="https://docs.espressif.com/projects/esp-idf/en/latest/esp32/">IDF SDK</a>, or directly via <a href="https://github.com/espressif/esptool">github</a>.  The `esptool.py` command is also avalable via many package managers (e.g., MacOS Homebrew).

By default, the `esp32_flash` target will assume the `esptool.py` command is available on the user's executable path.  Alternatively, you may specify the full path to the `esptool.py` command via the `-e` (or `--esptool`) option

By default, the `esp32_flash` target will write to port `/dev/ttyUSB0` at a baud rate of `115200`.  You may control the port and baud settings for connecting to your ESP device via the `-port` and `-baud` options to the `esp32_flash` target, e.g.,

    shell$ rebar3 esp32_flash --port /dev/tty.SLAB_USBtoUART --baud 921600
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    ===> Verifying dependencies...
    ===> Compiling mylib
    ===> esptool.py --chip esp32 --port /dev/tty.SLAB_USBtoUART --baud 921600 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect 0x110000 /home/frege/mylib/_build/default/lib/mylib.avm
    esptool.py v2.1
    Connecting........_
    Chip is ESP32D0WDQ6 (revision 1)
    Uploading stub...
    Running stub...
    Stub running...
    Changing baud rate to 921600
    Changed.
    Configuring flash size...
    Auto-detected Flash size: 4MB
    Wrote 16384 bytes at 0x00110000 in 0.2 seconds (615.0 kbit/s)...
    Hash of data verified.

    Leaving...
    Hard resetting...

Alternatively, the following environment variables may be used to control the above settings:

* ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_ESPTOOL
* ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_PORT
* ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_BAUD
* ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_OFFSET

Any setting specified on the command line take precendence over environment variable settings, which in turn take precedence over the default values specified above.

The `esp32_flash` target depends on the `packbeam` target, so any changes to modules in the project will get rebuilt before being flashed to the device.

# AtomVM App Template

The `atomvm_rebar3_plugin` contains template definitions for generating skeletal `rebar3` projects.

The best way to make use of this template is to include the `atomvm_rebar3_plugin` in your `~/.config/rebar3/rebar.config` file, e.g.,

    %% TODO set a tag
    {plugins, [
        {atomvm_rebar3_plugin, {git, "https://github.com/fadushin/atomvm_rebar3_plugin.git", {branch, "master"}}}
    ]}.

You can then generate a minimal AtomVM application as follows:

    shell$ rebar3 new atomvm_app myapp
    ===> Writing myapp/LICENSE
    ===> Writing myapp/rebar.config
    ===> Writing myapp/src/myapp.erl
    ===> Writing myapp/src/myapp.app.src
    ===> Writing myapp/README.md

This target will create a simple `rebar3` project with a minimal AtomVM application in the `myapp` directory.

Change to the `myapp` directory and issue the `packbeam` target to the `rebar3` command:

    shell$ cd myapp
    shell$ rebar3 packbeam
    ===> Fetching atomvm_rebar3_plugin (from {git,"https://github.com/fadushin/atomvm_rebar3_plugin.git",
                                    {branch,"master"}})
    ===> Fetching packbeam (from {git,"https://github.com/fadushin/atomvm_packbeam.git",
                        {branch,"master"}})
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    ===> Verifying dependencies...
    ===> Compiling myapp
    ===> AVM file written to : myapp.avm

An AtomVM AVM file is created in the `_build/default/lib` directory:

    shell$ ls -l _build/default/lib/myapp.avm
    -rw-rw-r--  1 frege  wheel  328 Jan 1 1970 00:01 _build/default/lib/myapp.avm
