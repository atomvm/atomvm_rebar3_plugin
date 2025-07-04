<!--
 Copyright 2020-2023 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->
# atomvm_rebar3_plugin

A [`rebar3`](https://rebar3.org) plugin for simplifying development of Erlang applications targeted for the [AtomVM](http://github.com/atomvm/AtomVM) Erlang abstract machine.

## Quick Start

Create or edit the `$HOME/.config/rebar3/rebar.config` file to include the `atomvm_rebar3_plugin` [`rebar3`](https://rebar3.org) plugin:

    {plugins, [
        atomvm_rebar3_plugin
    ]}.

From a working directory, issue the following command:

    shell$ rebar3 new atomvm_app myapp
    ===> Writing myapp/.gitignore
    ===> Writing myapp/LICENSE
    ===> Writing myapp/rebar.config
    ===> Writing myapp/README.md
    ===> Writing myapp/src/myapp.erl
    ===> Writing myapp/src/myapp.app.src

This task will create a simple [`rebar3`](https://rebar3.org) project with a minimal AtomVM application in the `myapp` directory.

Change to the `myapp` directory and issue the `packbeam` task to the `rebar3` command:

    shell$ cd myapp
    shell$ rebar3 packbeam
    ===> Fetching atomvm_rebar3_plugin
    ===> Fetching packbeam
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    ===> Verifying dependencies...
    ===> Compiling myapp
    ===> AVM file written to .../myapp/_build/default/lib/myapp.avm

An AtomVM AVM file named `myapp.avm` is created in the `_build/default/lib` directory:

    shell$ ls -l _build/default/lib/myapp.avm
    -rw-rw-r--  1 joe  wheel  328 Jan 1 1970 00:01 _build/default/lib/myapp.avm

If you have it installed, you can use the [`packbeam`](https://github.com/atomvm/atomvm_packbeam) tool to list the contents of this generated file:

    shell$ packbeam list _build/default/lib/myapp.avm
    myapp.beam * [384]
    myapp/priv/application.bin [220]

See the various flash tasks described below for information about how to flash the generated AVM file to your device.

## `atomvm_rebar3_plugin` tasks

The [`rebar3`](https://rebar3.org) plugin provides the following tasks under the `atomvm` namespace:

* `packbeam`  Generate AtomVM packbeam files from your [`rebar3`](https://rebar3.org) project and its dependencies.
* `esp32_flash`  Flash AtomVM packbeam files to ESP32 devices over a serial connection.
* `stm32_flash`  Flash AtomVM packbeam files to STM32 devices over a serial connection.
* `uf2create`   Generate a u2f binary from an AtomVM packbeam file.
* `pico_flash`  Flash "packed" uf2 files to RP2040 (RPi Pico) devices by copying to FATfs.
* `version`  Print the version of the [`atomvm_rebar3_plugin`](https://atomvm.github.io/atomvm_rebar3_plugin) to the console.
* `bootstrap`  Compile Erlang files that `rebar3` otherwise cannot compile.  Typically, such files include modules from the OTP `kernel` or `stdlib` application that `rebar3` uses internally for its own implementation.
* `dialyzer`  Use dialyzer for static analysis of AtomVM applications.

> IMPORTANT!  Some of the above tasks were previously located  under the default [`rebar3`](https://rebar3.org) namespace; however, the commands under the default namespace have been DEPRECATED.  Users will get a  warning message on the console when using deprecated tasks, and any deprecated tasks may be removed in the future without warning.  Be sure to migrate any scripts or code you have to use the `atomvm` namespace.

The tasks listed above are described in more detail below.

### Configuration

The tasks supported by this plugin support the following modes of configuration, ordered from highest to lowest in terms of their precedence:

1. Command-line arguments
1. `rebar.config` settings
1. Environment variables
1. Hard-wired defaults

Specifically, any options defined on the command line override options by the same name in `rebar.config`, which in turn override any corresponding environment variable settings, and so forth.

Configuration items for specific tasks are described in detail below for each of the supported tasks.

Any `rebar.config` settings are defined in the project's `rebar.config` project file, and are defined as property lists under the `atomvm_rebar3_plugin` key.  The properties in this list are themselves property lists, using the task name as a key, with task-specific configuration for each entry.

A typical `rebar.config` entry for this plugin therefore takes the form:

    {atomvm_rebar3_plugin, [
        {packbeam, [...]},
        {esp32_flash, [...]},
        ...
    ]}.

Configuration in `rebar.config` is optional but can be useful in some cases.  For example, the flash tasks depend on the `packbeam` task, to ensure that the AVM file is up to date before flashing.  However, if the AVM file is rebuilt, the flash task has no way to tell the `packbeam` task any task-specific properties it should use as part of the rebuild.  If they are defined in `rebar.config` (or in environment variables), however, they will be used during an implicit rebuild of the AVM file.

### The `packbeam` task

The `packbeam` task is used to generated an AtomVM packbeam (`.avm`) file.

    shell$ rebar3 help atomvm packbeam

    Use this plugin to create an AtomVM packbeam file from your rebar3 project.

    Usage: rebar3 atomvm packbeam [-e <external>] [-f <force>] [-p <prune>]
                                [-s <start>] [-r <remove_lines>]

    -e, --external      External AVM modules
    -f, --force         Force rebuild
    -p, --prune         Prune unreferenced BEAM files
    -s, --start         Start module
    -r, --remove_lines  Remove line information from generated AVM files
                        (off by default)
    -l, --list          List the contents of AVM files after creation

E.g.,

    shell$ rebar3 atomvm packbeam
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    ===> Compiling packbeam
    ===> Verifying dependencies...
    ===> Compiling myapp
    ===> AVM file written to .../myapp/_build/default/lib/myapp.avm

When using this task, an AVM file with the project name will be created in `_build/<profile>/lib/`, .e.g.,

    shell$ ls -l _build/default/lib/myapp.avm
    -rw-rw-r--  1 joe  wheel  8780 May 15 1895 22:03 _build/default/lib/myapp.avm

If your project has any erlang dependencies, the `packbeam` task will include any BEAM files or `priv` files from the dependent projects in the final AVM file.

If your project (or any of its dependencies) has multiple modules that export a `start/0` entry-point function, you can specify which module to use as the entry-point for your application via the `--start` (or `-s`) option:

    shell$ rebar3 atomvm packbeam --start my_start_module
    ...

Using this option will ensure that the generated AVM file with use `my_start_module` to start the application.

You may use the `--prune` option (or `-p`) to prune unnecessary beam files when creating AVM files.  Pruning unnecessary files can make your AVM files smaller, leading to faster development cycles and more free space on flash media.  Pruning is not enabled by default.  Note that if you use the prune option, your project (or at least one of its dependencies) _must_ have a `start/0` entry-point.  Otherwise, you should treat your project as a library, suitable for inclusion in a different AtomVM project.

By default, line number information is included in generated AVM files.  Including line number information is useful for debugging and locating the source of application crashes.  However, adding line number information makes AVM files larger and in some cases may have an impact on memory usage. You can remove line number information from your AVM files via the `--remove_lines` (or `-r`) flag, if, for example, you are prepared to deploy your application into production.

The `packbeam` task will use timestamps to determine whether a rebuild is necessary.  However, timestamps may not be enough to trigger a rebuild, for example, if a dependency was added or removed.  You can force a rebuild of AVM file by adding the `--force` flag (or `-f`), with no arguments.  All AVM files, including AVM files for dependencies, will be rebuilt regardless of timestamps.

If you would like to view the contents of the AVM file after you have created it, use the `--list` (of `-l`) flag to display the entries of the file on the console.  Entries which export a `start/0` function are marked with an asterisk (`*`).  All entries include their size in bytes, wrapped in square brackets (`[]`).

    shell$ rebar3 atomvm packbeam -l
    ...
    ===> AVM file written to .../myapp/_build/default/lib/myapp.avm
    AVM contents
    ============
    myapp.beam * [384]
    myapp/priv/application.bin [228]

The following table enumerates the properties that may be defined in your project's `rebar.config`
file for this task.  Use `packbeam` as the key for any properties defined for this task.

> Note that the `--list` flag is only operative when the AVM file has been written.  Use the `--force` (`-f`) flag to force a rebuild of the AVM file, if desired.

| Key | Type | Description |
|-----|------|-------------|
| `force` | `boolean()` | Always force a rebuild of the AVM file, even if up to date |
| `prune` | `boolean()` | Prune unnecessary BEAM files from generated AVM |
| `start` | `atom()` | The start module |
| `remove_lines` | `boolean()` | Remove line number information from generated AVM files.  |
| `list` | `boolean()` | List the AVM file contents when generating AVM files.  |

Example:

    {atomvm_rebar3_plugin, [{packbeam, [prune, {start, main}]}]}.

Any setting specified on the command line take precedence over settings in `rebar.config`, which in turn take precedence over environment variable settings, which in turn take precedence over the default values specified above.

The `packbeam` task depends on the `compile` task, so any changes to modules in the project will automatically get rebuilt when running the `packbeam` task.

#### External Dependencies

If you already have AVM modules are not available via `rebar3`, you can direct the `packbeam` task to these AVM files via the `--external` (or `-e`) flag, e.g.,

    shell$ rebar3 atomvm packbeam -e <path-to-avm-1> -e <path-to-avm-2> ...
    ===> Fetching packbeam
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    ===> Verifying dependencies...
    ===> Compiling myapp
    ===> AVM file written to .../myapp/_build/default/lib/myapp.avm

#### Building OTP Applications

You can use the `packbeam` task to build AtomVM applications that implements the OTP `application` behavior, and the `atomvm_rebar3_plugin` will create an AVM file that contains boot information to start your application automatically when AtomVM starts.

For example, a module that implements the OTP `application` behavior might look as follows:

    %% erlang
    -module(myapp_app).

    -export([start/2, stop/1]).

    start(_Type, Args) ->
        io:format("Starting myapp_app ...~n"),
        myapp_sup:start(Args).

    stop(_State) ->
        myapp_sup:stop_children().

(assume `myapp_sup` is also a part of your OTP application).

And the application configuration file (e.g., `myapp.app.src`) should include the application module (`myapp_app`) under it's `mod` entry:

    {
        application, myapp, [
            {description, "My AtomVM application"},
            {vsn, "0.1.0"},
            {registered, []},
            {applications, [
                kernel,
                stdlib
            ]},
            {env,[]},
            {mod, {myapp_app, []}},
            {modules, []},
            {licenses, ["Apache-2.0"]},
            {links, []}
        ]
    }.

If you specify `init` as the start module, then an AVM file will be created:

    shell$ rebar3 atovm packbeam -p -s init
    ===> Analyzing applications...
    ===> Compiling atomvm_rebar3_plugin
    ===> Compiling packbeam
    ...
    ===> Analyzing applications...
    ===> Compiling myapp
    ===> AVM file written to .../myapp/_build/default/lib/myapp.avm

This AVM file will contain the `init.beam` module, along with a boot script (`init/priv/start.boot`), which will be used by the `init.beam` module to start your application automatically.

For example:

    shell$ packbeam list _build/default/lib/myapp.avm
    init.beam * [1428]
    myapp_worker.beam [596]
    myapp_sup.beam [572]
    myapp_app.beam [416]
    myapp/priv/application.bin [288]
    init/priv/start.boot [56]
    myapp/priv/example.txt [24]

Running this AVM file will boot the `myapp` application automatically, without having to write an entrypoint module.

### The `esp32-flash` task

You may use the `esp32_flash` task to flash the generated AtomVM packbeam application to the flash storage on an ESP32 device connected over a serial connection.

    shell$ rebar3 help atomvm esp32_flash

    Use this plugin to flash an AtomVM packbeam file to an ESP32 device.

    Usage: rebar3 atomvm esp32_flash [-e <esptool>] [-c <chip>] [-p <port>]
                                    [-b <baud>] [-o <offset>]

    -e, --esptool  Path to esptool.py
    -c, --chip     ESP chip (default auto)
    -p, --port     Device port (default /dev/ttyUSB0)
    -b, --baud     Baud rate (default 115200)
    -o, --offset   Offset (default 0x210000)

The `esp32_flash` task will use the `esptool.py` command to flash the ESP32 device.  This tool is available via the <a href="https://docs.espressif.com/projects/esp-idf/en/latest/esp32/">IDF SDK</a>, or directly via <a href="https://github.com/espressif/esptool">github</a>.  The `esptool.py` command is also available via many package managers (e.g., MacOS Homebrew).

By default, the `esp32_flash` task will assume the `esptool.py` command is available on the user's executable path.  Alternatively, you may specify the full path to the `esptool.py` command via the `-e` (or `--esptool`) option

By default, the `esp32_flash` task will write to port `/dev/ttyUSB0` at a baud rate of `115200`.  You may control the port and baud settings for connecting to your ESP device via the `-port` and `-baud` options to the `esp32_flash` task, e.g.,

    shell$ rebar3 atomvm esp32_flash --port /dev/tty.SLAB_USBtoUART --baud 921600
    ...
    ===> esptool.py --chip esp32 --port /dev/tty.SLAB_USBtoUART --baud 921600 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect 0x110000 /home/joe/myapp/_build/default/lib/myapp.avm
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

The following table enumerates the properties that may be defined in your project's `rebar.config` file for this task.  Use `esp32_flash` as the key for any properties defined for this task.

| Key | Type | Description |
|-----|------|-------------|
| `esptool` | `string()` | Path to the `esptool.py` tool, if not already in user's `PATH` |
| `chip` | `string()` | ESP32 chip type |
| `port` | `string()` | Device port on which the ESP32 can be located |
| `baud` | `integer()` | Device BAUD rate |
| `offset` | `string()` | Offset into which to write AtomVM application |

Example:

    {atomvm_rebar3_plugin, [{esp32_flash, [{baud, 921600}]}]}.

Alternatively, the following environment variables may be used to control the above settings:

* `ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_ESPTOOL`
* `ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_CHIP`
* `ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_PORT`
* `ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_BAUD`
* `ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_OFFSET`

Any setting specified on the command line take precedence over settings in `rebar.config`, which in turn take precedence over environment variable settings, which in turn take precedence over the default values specified above.

The `esp32_flash` task depends on the `packbeam` task, so the packbeam file will get automatically built if any changes have been made to its dependencies.

### The `stm32_flash` task

You may use the `stm32_flash` task to flash the generated AtomVM packbeam application to the flash storage on an STM32 device connected to an st-link.

#### Preparing an application for flashing

The stm32 builds of AtomVM do not include a library partition and the [AtomVM](https://github.com/atomvm/AtomVM) `atomvmlib.avm` core library is not flashed to the device. Instead the application should be compiled and packed along with `atomvmlib.avm` before flashing, for example:

    shell$ rebar3 atomvm packbeam -p -e /path/to/atomvmlib.avm

You can acquire the latest [AtomVM](https://github.com/atomvm/AtomVM) `atomvmlib.avm` core library from the [Release](https://github.com/atomvm/AtomVM/releases) section of the [AtomVM](https://github.com/atomvm/AtomVM) Github repository.  Note that the version of the `atomvmlib.avm` core library *must* match the version of the [AtomVM](https://github.com/atomvm/AtomVM) virtual machine image you have flashed to the device.

#### Flashing an application to a stm32 device

You may use the `stm32_flash` task to flash the generated AtomVM packbeam application to the flash storage on an STM32 device connected to an st-link.

    shell$ rebar3 help atomvm stm32_flash

    Use this plugin to flash an AtomVM packbeam file to an STM32 device.

    Usage: rebar3 atomvm stm32_flash [-s <stflash>] [-o <offset>]

    -s, --stflash  Path to st-flash
    -o, --offset   Offset (default 0x8080000)

The `stm32_flash` will use the `st-flash` tool from the open source (bsd-3 licensed) [stlink](https://github.com/stlink-org/stlink) suite of stm32 utilities to flash the STM32 device. This tool is available on [github](https://github.com/stlink-org/stlink), and in many package managers.

By default, the `stm32_flash` task will assume the `st-flash` command is available on the user's executable path.  Alternatively, you may specify the full path to the `st-flash` command via the `-s` (or `--stflash`) option

    shell$ rebar3 atomvm stm32_flash --stflash /usr/bin/st-flash --offset 0x8080000
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling stm32_hello
    ===> st-flash --reset write /home/atomvm/AtomVM/stm32_hello/_build/default/lib/stm32_hello.avm 0x8080000

    st-flash 1.7.0
    2023-07-09T21:42:26 INFO common.c: F42x/F43x: 256 KiB SRAM, 2048 KiB flash in at least 16 KiB pages.
    file /home/atomvm/AtomVM/stm32_hello/_build/default/lib/stm32_hello.avm md5 checksum: 5747b8eab41a3696097eb386c785e, stlink checksum: 0x00154e50
    2023-07-09T21:42:26 INFO common.c: Attempting to write 29304 (0x7278) bytes to stm32 address: 134742016 (0x8080000)
    EraseFlash - Sector:0x8 Size:0x20000 2023-07-09T21:42:28 INFO common.c: Flash page at addr: 0x08080000 erased
    2023-07-09T21:42:28 INFO common.c: Finished erasing 1 pages of 131072 (0x20000) bytes
    2023-07-09T21:42:28 INFO common.c: Starting Flash write for F2/F4/F7/L4
    2023-07-09T21:42:28 INFO flash_loader.c: Successfully loaded flash loader in sram
    2023-07-09T21:42:28 INFO flash_loader.c: Clear DFSR
    2023-07-09T21:42:28 INFO common.c: enabling 32-bit flash writes
    2023-07-09T21:42:29 INFO common.c: Starting verification of write complete
    2023-07-09T21:42:29 INFO common.c: Flash written and verified! jolly good!

The following table enumerates the properties that may be defined in your project's `rebar.config` file for this task.  Use `stm32_flash` as the key for any properties defined for this task.

| Key | Type | Description |
|-----|------|-------------|
| `stflash` | `string()` | Path to the `st-flash` tool, if not already in user's `PATH` |
| `offset` | `string()` | Offset into which to write AtomVM application |

Example:

    {atomvm_rebar3_plugin, [{stm32_flash, [{offset, "0x230000"}]}]}.

Alternatively, the following environment variables may be used to control the above settings:

* `ATOMVM_REBAR3_PLUGIN_STM32_STFLASH`
* `ATOMVM_REBAR3_PLUGIN_STM32_FLASH_OFFSET`

Any setting specified on the command line take precedence over entries in `rebar.config`, which in turn take precedence over environment variable settings, which in turn take precedence over the default values specified above.

The `stm32_flash` task depends on the `packbeam` task, so the packbeam file will get automatically built if any changes have been made to its dependencies.

### The `pico_flash` task

#### Flashing an application to a pico (rp2040) device

You may use the `pico_flash` task to copy the generated AtomVM packbeam application in uf2 format to the flash storage on an Pico device connected to usb. It is not necessary to push the `BOOTSEL` button while plugging in the Pico to usb, instead provide the path of the device to reset. On Linux this is typically `/dev/ttyACM0` (the same device used to monitor serial), on MacOS it is a cu.usbmodem device matching `/dev/cu.usbmodem14*` (not the /dev/tty.usbmodem14___ device used for serial monitoring). It is highly recommended
that `picotool` is installed in your PATH. If it is found in the PATH the automatic `BOOTSEL` and mode can be activated even if you have an active terminal monitor (tmux, minicom, or screen) in another terminal it will
be disconnected automatically. With `picotool` in your PATH the device has a higher success rate of resetting and entering `application` mode after flashing. Without `picotool` some small applications do not always
trigger a reset into `application` mode after flashing with `rebar3 atomvm pico_flash`.

    shell$ rebar3 help atomvm pico_flash

    Use this plugin to convert an AtomVM packbeam file to a rp2040 a uf2 file and copy to an rp2040 devices.

    Usage: rebar3 atomvm pico_flash [-p <path>] [-r <reset>]

    -p, --path   Path to pico device (Defaults Linux:
                /run/media/${USER}/RPI-RP2, MacOS: /Volumes/RPI-RP2)
    -r, --reset  Path to serial device to reset before flashing (Defaults
                Linux: /dev/ttyACM0, MacOS: /dev/cu.usbmodem14*)

The `pico_flash` task depends on the `uf2create` task which in turn depends on the `packbeam` task, so in most cases it is not necessary to execute either of those tasks if the default settings are used, as any changes to modules in the project will get rebuilt before being flashed to the device.

    shell$ rebar3 atomvm pico_flash
    ===> Fetching atomvm_rebar3_plugin v0.7.5
    ===> Fetching rebar3_hex v7.0.6
    ===> Fetching hex_core v0.8.4
    ===> Fetching verl v1.1.1
    ===> Analyzing applications...
    ===> Compiling hex_core
    ===> Compiling verl
    ===> Compiling rebar3_hex
    ===> Fetching atomvm_packbeam v0.7.4
    ===> Fetching rebar3_proper v0.12.1
    ===> Analyzing applications...
    ===> Compiling rebar3_proper
    ===> Analyzing applications...
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling hello_world
    ===> AVM file written to /home/joe/projects/hello_world/_build/default/lib/hello_world/hello_world.avm
    ===> Resetting device at path /dev/ttyACM0
    ===> Waiting for the device at path /run/media/${USER}/RPI-RP2 to settle and mount...
    ===> Copying /home/joe/projects/hello/_build/default/lib/hello.uf2 to /run/media/${USER}/RPI-RP2...

    '/home/joe/projects/hello_world/_build/default/lib/hello_world.uf2' -> '/run/media/joe/RPI-RP2/hello_world.uf2'

If your pico uses a different device path or mount directory supply the full path needed for your device:

    shell$ rebar3 atomvm pico_flash --path /mnt/pico --reset /dev/cu.usbmodem1411202

> Note: If you use the `pico_flash` task while you are still connected to a serial monitoring application such as `minicom` or `screen` the `pico_flash` task will attempt to locate [`picotool`](https://github.com/raspberrypi/picotool) in the users PATH. If `picotool` is found it will be used to reboot the pico, disconnecting the serial monitor in the process, and the device will be put into `BOOTSEL` mode after it is rebooted. If `picotool` is not available the user will be informed, and reminded that manually closing the serial monitor is necessary before using `pico_flash`.

> Warning: There is currently a known bug that occurs when the VM has exited, the processor is left in as hung state. The device will take unusually long attempting to reset, and fail to mount the FAT partition within 30 seconds and `pico_flash` will fail. There is work under way to fix this bug, but in the meantime it can be worked around by unplugging the pico from usb and plug it back in again, before repeating the flash procedure.

The following table enumerates the properties that may be defined in your project's `rebar.config` file for this task.  Use `pico_flash` as the key for any properties defined for this task.

| Key | Type | Description |
|-----|------|-------------|
| `path` | `string()` | Path to pico device |
| `reset` | `string()` | Path to serial device to reset before flashing |
| `picotool` | `string()` | Path to picotool executable |

Example:

    {atomvm_rebar3_plugin, [{pico_flash, [{reset, "/dev/cu.usbmodem1411202"}]}]}.

Alternatively, the following environment variables may be used to control the above settings:

* `ATOMVM_REBAR3_PLUGIN_PICO_MOUNT_PATH` | `ATOMVM_PICO_MOUNT_PATH`
* `ATOMVM_REBAR3_PLUGIN_PICO_RESET_DEV` | `ATOMVM_PICO_RESET_DEV`
* `ATOMVM_REBAR3_PLUGIN_PICOTOOL` | `ATOMVM_PICOTOOL`

Any setting specified on the command line take precedence over entries in `rebar.config`, which in turn take precedence over environment variable settings, which in turn take precedence over the default values specified above.

> Note the setting `ATOMVM_REBAR3_PLUGIN_PICO_MOUNT_PATH` and `ATOMVM_REBAR3_PLUGIN_PICO_RESET_PATH` take precedence, but `ATOMVM_PICO_MOUNT_PATH` and `ATOMVM_PICO_RESET_DEV` are also honoured as a fallback, so that the same environment setting can be shared with the [ExAtomVM](https://github.com/atomvm/exatomvm) `mix` plugin.

The `pico_flash` task depends on the `uf2create` task (which in turn depends on the `packbeam`  task), so the so the application will be packed and re-formatted if any changes have been made to dependencies.

### The `uf2create` task

The `uf2create` task is used to generated an uf2 binary suitable for running on a Pico (RP2040) device from an AtomVM packbeam (`.avm`) file.

    shell$ rebar3 help atomvm uf2create

    Use this plugin to create Raspberry Pico uf2 files from an AtomVM packbeam file.

    Usage: rebar3 atomvm uf2create [-f <family_id>] [-o <output>]
                               [-s <start>] [-i <input>]

      -f, --family_id  Flavor of uf2 file to create (default rp2040)
      -o, --output      Output path/name
      -s, --start       Start address for the uf2 binary (default 0x10180000)
      -i, --input       Input avm file to convert to uf2

It should not be necessary to use this tool before using `pico_flash`, unless you have built a custom VM that requires changing the start address of the uf2 binary. If the application has not been compiled, or packed with packbeam, these steps will be run first using the default settings for `packbeam`.

The following table enumerates the valid `family_id` options:

| Key         | Description |
|-------------|-------------|
| `rp2040`    | Original Raspberry Pi Pico and Pico-W |
| `rp2035`    | Raspberry Pi Pico 2 |
| `data`      | Raspberry Pi Pico 2 |
| `universal` | Universal format for both `rp2040` and `rp2035` |

> Note the convenience of universal uf2 binaries comes with the expense of being twice the size, as both versions are included in the universal uf2.

The following table enumerates the properties that may be defined in your project's `rebar.config` file for this task.  Use `uf2create` as the key for any properties defined for this task.

| Key | Type | Description |
|-----|------|-------------|
| `start` | `string()` | Start address for the uf2 binary |
| `family_id` | `string()` | The family\_id or flavor of uf2 to build |

Example:

    {atomvm_rebar3_plugin, [{uf2create, [{start, "0x10180000"},{family_id,universal}]}]}.

Alternatively, the following environment variables may be used to control the above settings:

* `ATOMVM_REBAR3_PLUGIN_UF2CREATE_START` | `ATOMVM_PICO_APP_START`
* `ATOMVM_REBAR3_PLUGIN_UF2_FAMILY` | `ATOMVM_PICO_UF2_FAMILY`

> Note the settings `ATOMVM_REBAR3_PLUGIN_UF2CREATE_START` and `ATOMVM_REBAR3_PLUGIN_UF2_FAMILY` take precedence, but `ATOMVM_PICO_APP_START` and`ATOMVM_PICO_UF2_FAMILY`  will also be used, if set, so that the same environment setting can be shared with the [ExAtomVM](https://github.com/atomvm/exatomvm) `mix` plugin.

Any setting specified on the command line take precedence over entries in `rebar.config`, which in turn take precedence over environment variable settings, which in turn take precedence over the default values specified above.

The `uf2create` task depends on the `packbeam` task, so the packbeam file will get automatically built if any changes have been made to its dependencies.

### The `dialyzer` task

Use the `dialyzer` task to check applications for type discrepancies. To use the example just run:

```shell
$ rebar3 atomvm dialyzer
```

The `atomvm` launcher script should be in your PATH. This is taken care of with a MacOS install using MacPorts, or
Homebrew. Linux users will need to build from source and use the cmake "install" target. If you have installed AtomVM
to a non-standard location (i.e. /otp/atomvm) and the `atomvm` launcher script is not in your PATH you must supply the
install location containing the AtomVM libraries using the `--atomvm_root` option when using this task. You may also
use the path to the build directory used for a generic_unix build of the AtomVM BEAM libraries.

Example for a non-standard instal when `atomvm` launcher is not in PATH:

```
$ rebar3 atomvm dialyzer --atomvm_root /opt/atomvm
```

Example using a development build directory:

```
$ rebar3 atomvm dialyzer --atomvm_root /home/joe/work/AtomVM/build
```

This option may also be supplied in the `atomvm_rebar3_plugin` configuration options in rebar.conf. Example:

```erlang
    {atomvm_rebar3_plugin, [{dialyzer, [{atomvm_root, "/opt/atomvm"}]}]}.
```

Options available from the command line, or rebar3.conf are:

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `base_plt_location` |`string()` \| `binary()` | "$HOME/.cache/rebar3" | Location of base PLT |
| `plt_location` | `string()` \| `binary()` | build profile base directory | Location of application PLT |
| `plt_prefix` | `string()` | application name | The base name of the application plt |
| `atomvm_root` | `string()` \| `binary()` | detected | If the `atomvm` launcher script is in PATH this will be detected automatically, otherwise specify the atomvm base directory containing the atomvm libraries |

### The `version` task

use the `version` task to print the current version of the [`atomvm_rebar3_plugin`](https://atomvm.github.io/atomvm_rebar3_plugin) to the console.

    shell$ rebar3 atomvm version
    0.7.5

### The `bootstrap` task

Use the `bootstrap` task to compile Erlang files that `rebar3` is otherwise unable to compile.  Typically, such files include modules from the OTP `kernel` or `stdlib` application that `rebar3` uses internally for its own implementation.

> Note.  The default `rebar3` `compile` task has the unfortunate feature (bug?) that it will load files that it compiles, which can be problematic if you are compiling modules that share names with modules that `rebar3` is using as part of its own implementation.

    shell$ rebar3 help atomvm bootstrap

    This plugin is used internally by the atomvm packbeam task to compile
    modules that cannot be compiled directly by rebar.

    Users typically have no reason to use this task directly.

    Usage: rebar3 atomvm tstrap [-b <bootstrap_dir>] [-f <force>]

    -b, --bootstrap_dir  Bootstrap directory
    -f, --force         Force rebuild

To use this task, place files that you would like to have compiled by the task in the `bootstrap` directory of your rebar3 project (or in a directory of your choosing -- see below).

    shell$ ls bootstrap
    application.erl

Files in this directory will be compiled and included in any generated PackBEAM files.

> Note.  The `bootstrap` task is used internally by the [`atomvm_rebar3_plugin`](https://atomvm.github.io/atomvm_rebar3_plugin) when the `packbeam` task is run.  Users typically do not have a need to run this task manually.

The following table enumerates the properties that may be defined in your project's `rebar.config` file for this task.  Use `bootstrap` as the key for any properties defined for this task.

| Key | Type | Description |
|-----|------|-------------|
| `force` | `boolean()` | Always force recompilation of bootstrap files, even if up to date |
| `bootstrap_dir` | `string() \| undefined` | (Optional) path to a directory containing bootstrap files.  By default, the `bootstrap` task will use the `bootstrap` directory in the top-level project directory.  The path may be relative (to where the command is run) or absolute. |

Example:

    {atomvm_rebar3_plugin, [
        {bootstrap, [
            {bootstrap_dir, "/path/to/bootstrap_dir"}, force
        ]}
    ]}.

Any setting specified on the command line take precedence over entries in `rebar.config`, which in turn take precedence over the default values specified above.



## AtomVM App Template

The `atomvm_rebar3_plugin` contains template definitions for generating skeletal `rebar3` projects.

The best way to make use of this template is to include the `atomvm_rebar3_plugin` in your `$HOME/.config/rebar3/rebar.config` file, e.g.,

    {plugins, [
        atomvm_rebar3_plugin
    ]}.

You can then generate a minimal AtomVM application as follows:

    shell$ rebar3 new atomvm_app myapp
    ===> Writing myapp/.gitignore
    ===> Writing myapp/LICENSE
    ===> Writing myapp/rebar.config
    ===> Writing myapp/README.md
    ===> Writing myapp/src/myapp.erl
    ===> Writing myapp/src/myapp.app.src

This task will create a simple `rebar3` project with a minimal AtomVM application in the `myapp` directory.

Change to the `myapp` directory and issue the `packbeam` task to the `rebar3` command:

    shell$ cd myapp
    shell$ rebar3 packbeam
    ===> Fetching atomvm_rebar3_plugin
    ===> Fetching packbeam
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    ===> Verifying dependencies...
    ===> Compiling myapp
    ===> AVM file written to .../myapp/_build/default/lib/myapp.avm

An AtomVM AVM file is created in the `_build/default/lib` directory:

    shell$ ls -l _build/default/lib/myapp.avm
    -rw-rw-r--  1 joe  wheel  328 Jan 1 1970 00:01 _build/default/lib/myapp.avm
