# atomvm_rebar3_plugin

A `rebar3` plugin for simplifying development of Erlang applications targeted for the <a href="http://github.com/bettio/AtomVM">AtomVM</a> Erlang abstract machine.

This `rebar3` plugin provides the following targets under the `atomvm` namespace:

* `packbeam`  Generate AtomVM packbeam files from your `rebar3` project and its dependencies.
* `esp32_flash`  Flash AtomVM packbeam files to ESP32 devices over a serial connection.
* `stm32_flash`  Flash AtomVM packbeam files to STM32 devices over a serial connection.
* `uf2create`   Generate a u2f binary from an AtomVM packbeam file.
* `pico_flash`  Flash "packed" uf2 files to RP2040 (RPi Pico) devices by copying to FATfs.

> Note. The above targets were previously under the default namespace; however, the commands under the default namespace have been DEPRECATED.

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
        ...
        atomvm_rebar3_plugin
        ...
    ]}.

> Note.  Check the latest version in the [`atomvm_rebar3_plugin`](https://hex.pm/packages/atomvm_rebar3_plugin) Hex repository.

Create a file called main.erl in the `src` directory with the contents:

    -module(main).
    -export([start/0]).

    start() ->
        ok.

Use the `packbeam` target to create an AVM file:

    shell$ rebar3 atomvm packbeam
    ===> Analyzing applications...
    ===> Compiling atomvm_rebar3_plugin
    ===> Compiling packbeam
    ...
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling mylib
    ===> AVM file written to : mylib.avm

The generated AVM file can be found in `_build/default/lib/mylib.avm`.

You can use the [`packbeam`](https://github.com/atomvm/atomvm_packbeam) tool to list the contents of this generated file:

    shell$ packbeam list _build/default/lib/mylib.avm
    main.beam * [320]
    mylib.beam [228]
    mylib/priv/application.bin [228]

## The `packbeam` target

The `packbeam` target is used to generated an AtomVM packbeam (`.avm`) file.

    shell$ rebar3 help atovm packbeam
    ...
    A rebar plugin to create packbeam files
    Usage: rebar3 atomvm packbeam [-e] [-f] [-p] [-i] [-r] [-s <start>]

    -e, --external       External AVM modules
    -f, --force          Force rebuild
    -p, --prune          Prune unreferenced BEAM files
    -i, --include_lines  Include line information in generated AVM files
                         (deprecated)
    -r, --remove_lines   Remove line information from generated AVM files
                         (off by default)
    -s, --start          Start module
E.g.,

    shell$ rebar3 atomvm packbeam
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

If your project (or any of its dependencies) has multiple modules that export a `start/0` entry-point function, you can specify which module to use as the entry-point for your application via the `--start` (or `-s`) option:

    shell$ rebar3 atomvm packbeam --start my_start_module
    ...

Using this option will ensure that the generated AVM file with use `my_start_module` to start the application.

You may use the `--prune` option (or `-p`) to prune unnecessary beam files when creating AVM files.  Pruning unnecessary files can make your AVM files smaller, leading to faster development cycles and more free space on flash media.  Pruning is not enabled by default.  Note that if you use the prune option, your project (or at least one of its dependencies) _must_ have a `start/0` entry-point.  Otherwise, you should treat your project as a library, suitable for inclusion in a different AtomVM project.

The `packbeam` target will use timestamps to determine whether a rebuild is necessary.  However, timestamps may not be enough to trigger a rebuild, for example, if a dependency was added or removed.  You can force a rebuild of AVM file by adding the `--force` flag (or `-f`), with no arguments.  All AVM files, including AVM files for dependencies, will be rebuilt regardless of timestamps.

The `packbeam` target depends on the `compile` target, so any changes to modules in the project will automatically get rebuilt when running the `packbeam` target.

### Building OTP Applications

You can use the `packbeam` target to build AtomVM applications that implements the OTP `application` behavior, and the `atomvm_rebar3_plugin` will create an AVM file that contains boot information to start your application automatically when AtomVM starts.

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

And the application configuration file (e.g., `myapp.app.src`) should include the application mdoule (`myapp_app`) under it's `mod` entry:

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
            {licenses, ["Apache 2.0"]},
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
    ===> AVM file written to : myapp.avm

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

### External Dependencies

If you already have AVM modules are not available via `rebar3`, you can direct the `packbeam` target to these AVM files via the `-e` (or `--external`) flag, e.g.,

    shell$ rebar3 atomvm packbeam -e <path-to-avm-1> -e <path-to-avm-2> ...
    ===> Fetching packbeam
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    ===> Compiling atomvm_rebar3_plugin
    ===> Verifying dependencies...
    ===> Compiling mylib
    ===> AVM file written to : mylib.avm

### Line Number Information

By default, running the packbeam target will generate line number information in packbeam files.  This information is used at runtime to generate stracktrace information about the file location of frames in a stacktrace.  This information can be essential for debugging, but for finalized projects, some users may want to strip line information from applications, in order to save on memory at runtime and space on flash.

You can use the `-r` or `--remove_lines` flags to packbeam, to remove line information from packbeam files.

## The `esp32-flash` target

You may use the `esp32_flash` target to flash the generated AtomVM packbeam application to the flash storage on an ESP32 device connected over a serial connection.

    shell$ rebar3 help atomvm esp32_flash
    ...
    A rebar plugin to flash packbeam to ESP32 devices
    Usage: rebar3 esp32_flash [-e] [-p] [-b] [-o]

    -e, --esptool  Path to esptool.py
    -c, --chip     ESP chip (default esp32)
    -p, --port     Device port (default /dev/ttyUSB0)
    -b, --baud     Baud rate (default 115200)
    -o, --offset   Offset (default 0x210000)

The `esp32_flash` will use the `esptool.py` command to flash the ESP32 device.  This tool is available via the <a href="https://docs.espressif.com/projects/esp-idf/en/latest/esp32/">IDF SDK</a>, or directly via <a href="https://github.com/espressif/esptool">github</a>.  The `esptool.py` command is also avalable via many package managers (e.g., MacOS Homebrew).

By default, the `esp32_flash` target will assume the `esptool.py` command is available on the user's executable path.  Alternatively, you may specify the full path to the `esptool.py` command via the `-e` (or `--esptool`) option

By default, the `esp32_flash` target will write to port `/dev/ttyUSB0` at a baud rate of `115200`.  You may control the port and baud settings for connecting to your ESP device via the `-port` and `-baud` options to the `esp32_flash` target, e.g.,

    shell$ rebar3 atomvm esp32_flash --port /dev/tty.SLAB_USBtoUART --baud 921600
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
* ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_CHIP
* ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_PORT
* ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_BAUD
* ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_OFFSET

Any setting specified on the command line take precedence over environment variable settings, which in turn take precedence over the default values specified above.

The `esp32_flash` target depends on the `packbeam` target, so any changes to modules in the project will get rebuilt before being flashed to the device.

## The `stm32_flash` target

### Preparing an application for flashing
The stm32 builds of AtomVM do not include a library partition and atomvmlib.avm is not flashed to the device. Instead the application should be compiled and packed along with atomvmlib.avm before flashing, for example:

    shell$ rebar3 atomvm packbeam -p -e /path/to/atomvmlib.avm

Consult `rebar3 help packbeam` for other options.

### Flashing an application to a stm32 device
You may use the `stm32_flash` target to flash the generated AtomVM packbeam application to the flash storage on an STM32 device connected to an st-link.

    shell$ rebar3 help atomvm stm32_flash
    A rebar plugin to flash packbeam to STM32 devices
    Usage: rebar3 stm32_flash [-s] [-o]

    -s, --stflash  Path to st-flash
    -o, --offset   Offset (default 0x8080000)

The `stm32_flash` will use the `st-flash` tool from the open source (bsd-3 liscensed) [stlink](https://github.com/stlink-org/stlink) suite of stm32 utilites to flash the STM32 device. This tool is available on [github](https://github.com/stlink-org/stlink), and in many package managers.

By default, the `stm32_flash` target will assume the `st-flash` command is available on the user's executable path.  Alternatively, you may specify the full path to the `st-flash` command via the `-s` (or `--stflash`) option

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

Alternatively, the following environment variables may be used to control the above settings:

* ATOMVM_REBAR3_PLUGIN_STM32_STFLASH
* ATOMVM_REBAR3_PLUGIN_STM32_FLASH_OFFSET

Any setting specified on the command line take precedence over environment variable settings, which in turn take precedence over the default values specified above.

## The `pico_flash` target

### Flashing an application to a pico (rp2040) device
You may use the `pico_flash` target to copy the generated AtomVM packbeam application in uf2 format to the flash storage on an Pico device connected to usb. It is not necessary to push the `BOOTSEL` button while plugging in the Pico to usb, instead provide the path of the device to reset. On Linux this is typically `/dev/ttyACM0` (the same device used to monitor serial), on MacOS it is a cu.usbmodem device matching `/dev/cu.usbmodem14*` (not the /dev/tty.usbmodem14___ device used for serial monitoring).

    shell$ rebar3 help atomvm pico_flash
    A rebar plugin to convert packbeam to uf2 and copy to rp2040 (Raspberry Pi Pico) devices
    Usage: rebar3 atomvm pico_flash [-p <path>] [-r <reset>]

      -p, --path   Path to pico device (Defaults Linux:
                   /run/media/${USER}/RPI-RP2, MacOS: /Volumes/RPI-RP2)
      -r, --reset  Path to serial device to reset before flashing (Defaults
                   Linux: /dev/ttyACM0, MacOS: /dev/cu.usbmodem14*)

The `pico_flash` target depends on the `uf2create` target which in turn depends on `packbeam`, so in most cases it is not necessary to execute either of those targets if the default settings are used, as any changes to modules in the project will get rebuilt before being flashed to the device.

    shell$ rebar3 atomvm pico_flash
    ===> Fetching atomvm_rebar3_plugin v0.7.0
    ===> Fetching rebar3_hex v7.0.6
    ===> Fetching hex_core v0.8.4
    ===> Fetching verl v1.1.1
    ===> Analyzing applications...
    ===> Compiling hex_core
    ===> Compiling verl
    ===> Compiling rebar3_hex
    ===> Fetching atomvm_packbeam v0.6.0
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

> Warning: There is currently a known bug that occurs when the VM is compiled with the `-DAVM_WAIT_FOR_USB_CONNECT` cmake option. If you have previously connected to the tty serial port with `screen`, `minicom`, or similar and have disconnected or closed the session, the device will take unusually long to reset and fail to mount the FAT partition within 30 seconds and `pico_flash` will fail. This can be worked around by unplugging the pico from usb and plug it back in again, before repeating the flash procedure.

## The `uf2create` target

The `uf2create` target is used to generated an uf2 binary suitable for running on a Pico (RP2040) device from an AtomVM packbeam (`.avm`) file.

    shell$ rebar3 help atomvm uf2create
    A rebar plugin to create uf2 files from packbeam files
    Usage: rebar3 atomvm uf2create [-o <output>] [-s <start>] [-i <input>]

      -o, --output  Output path/name
      -s, --start   Start address for the uf2 binary (default 0x10180000)
      -i, --input   Input avm file to covert to uf2

It should not be necessary to use this tool before using `pico_flash`, unless you have built a custom VM that requires changing the start address of the uf2 binary. If the application has not been compiled, or packed with packbeam, these steps will be run first using the default settings for `packbeam`.

## AtomVM App Template

The `atomvm_rebar3_plugin` contains template definitions for generating skeletal `rebar3` projects.

The best way to make use of this template is to include the `atomvm_rebar3_plugin` in your `~/.config/rebar3/rebar.config` file, e.g.,

    %% TODO set a tag
    {plugins, [
        atomvm_rebar3_plugin
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
    ===> Fetching atomvm_rebar3_plugin
    ===> Fetching packbeam
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    ===> Verifying dependencies...
    ===> Compiling myapp
    ===> AVM file written to : myapp.avm

An AtomVM AVM file is created in the `_build/default/lib` directory:

    shell$ ls -l _build/default/lib/myapp.avm
    -rw-rw-r--  1 frege  wheel  328 Jan 1 1970 00:01 _build/default/lib/myapp.avm
