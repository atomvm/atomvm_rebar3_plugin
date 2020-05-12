# atomvm_rebar3_plugin

A rebar plugin for simplifying development of Erlang applications targeted for the <a href="http://github.com/bettio/AtomVM">AtomVM</a> Erlang abstract machine.

This rebar3 plugin provides targets for generation AtomVM packbeam files from your rebar3 project and its dependencies and flashing them to ESP32 devices.

## Usage

Add the plugin to your rebar config:

    {plugins, [
        {atomvm_rebar3_plugin, {git, "https://github.com/fadushin/atomvm_rebar3_plugin.git", {branch, "master"}}}
    ]}.

> TODO define a suitable tag

### AtomVM projects

> TODO not net implemented

You can create an AtomVM project using one of two templates.

The `atomvm-app` will create a skeleton project you can use as a starting point for an AtomVM project.

    shell$ rebar3 new atomvm-app name=myapp

Alternatively, the `atomvm-lib` template can be used to create an AtomVM library, suitable for inclusion in different AtomVM applications.

    shell$ rebar3 new atomvm-lib name=mylib


### `packbeam` target

You may use the `packbeam` target to generate an AtomVM packbeam file containing your application and its dependencies.

    $ rebar3 packbeam
    <Plugin Output>


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
