# atomvm_rebar3_plugin

A `rebar3` plugin for simplifying development of Erlang applications targeted for the <a href="http://github.com/bettio/AtomVM">AtomVM</a> Erlang abstract machine.

This `rebar3` plugin provides targets for generation AtomVM packbeam files from your `rebar3` project and its dependencies and flashing them to ESP32 devices.

The `atomvm_rebar3_plugin` plugin makes use of the <a href="https://github.com/fadushin/packbeam">packbeam</a> tool.

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

> TODO define a suitable tag

Create a file called main.erl in the `src` directory with the contents:

    -module(main).
    -export([start/0]).

    start() ->
        ok.

### `packbeam` target

The `packbeam` target is used to generated an AtomVM packbeam (`.avm`) file.  E.g.,

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

Currently, the AtomVM `eavmlib.avm` and `estdlib.avm` modules are not available via `rebar3`.  However, if you have them built as part of the AtomVM project, you can direct the `packbeam` target to these AVM files via the `-e` (or `--external`) flag, e.g.,

    shell$ rebar3 packbeam -e <path-to>/AtomVM/build/libs/eavmlib/src/eavmlib.avm -e <path-to>/AtomVM/build/libs/estdlib/src/estdlib.avm -f
    ===> Fetching packbeam (from {git,"https://github.com/fadushin/packbeam.git",
                        {branch,"master"}})
    ===> Compiling packbeam
    ===> Compiling atomvm_rebar3_plugin
    ===> Compiling atomvm_rebar3_plugin
    ===> Verifying dependencies...
    ===> Compiling mylib
    ===> AVM file written to : mylib.avm

An AtomVM AVM file can be found under `_build/default/lib`.

    shell$ packbeam list -in _build/default/lib/mylib.avm
    main.beam * [264]
    mylib.beam [228]
    atomvm.beam [412]
    console.beam [840]
    esp.beam [872]
    gpio.beam [600]
    i2c.beam [824]
    http_server.beam [3852]
    json_encoder.beam [996]
    logger.beam [3708]
    network_fsm.beam [4008]
    spi.beam [580]
    timer_manager.beam [1968]
    timestamp_util.beam [692]
    uart.beam [556]
    calendar.beam [776]
    gen_server.beam [2324]
    gen_statem.beam [2036]
    gen_udp.beam [1260]
    gen_tcp.beam [1972]
    inet.beam [600]
    io_lib.beam [2112]
    io.beam [448]
    lists.beam [1704]
    proplists.beam [360]
    string.beam [436]
    timer.beam [276]
    erlang.beam [572]

> Note.  The `packbeam` tool can be created by running `make escript` in the `_build/default/plugins/packbeam` directory.

You may use the `-p` option (or `--prune`) to prune uncessary beam files when creating AVM files.  Pruning unecessary files can make your AVM files smaller, leading to faster development cycles and more free space on flash media.  Pruning is not enabled by default.  Note that if you use the prune option, your project (or at least one of its dependencies) _must_ have a `start/0` entrypoint.  Otherwise, you should treat your project as a library, suitable for inclusion in a different AtomVM project.

The `packbeam` target will use timestamps to determine whether a rebuild is necessary.  However, timestamps may not be enough to trigger a rebuild, for example, if a dependency was added or removed.  You can force a rebuild of AVM file by adding the `-f` flag (or `--force`), with no arguments.  All AVM files, including AVM files for dependencies, will be rebuilt regardless of timestamps.

### `esp32-flash` target

> TODO not yet implemented

You may use the `esp32-flash` target to flash the generated AtomVM packbeam application to the flash storage on an ESP32 device connected over a serial connection.

    $ rebar3 esp32-flash
    <Plugin Output>

By default, the `esp32-flash` target will write to port `/dev/ttyUSB0` at a baud rate of `115200`.

You may control the port and baud settings for connecting to your ESP device via the `-port` and `-baud` options to the `esp32-flash` target, e.g.,

    $ rebar3 esp32-flash -port /dev/tty.SLAB_USBtoUART -baud 921600
    <Plugin Output>

Alternatively, the following environment variables may be used to control the same settings:

* ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_PORT
* ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_BAUD

Any setting specified on the command line take precendence over environment variable settings, which in turn take precedence over the default values specified above.


## AtomVM projects

> TODO not net implemented

You can create an AtomVM project using one of two templates.

The `atomvm-app` will create a skeleton project you can use as a starting point for an AtomVM project.

    shell$ rebar3 new atomvm-app name=myapp

Alternatively, the `atomvm-lib` template can be used to create an AtomVM library, suitable for inclusion in different AtomVM applications.

    shell$ rebar3 new atomvm-lib name=mylib
