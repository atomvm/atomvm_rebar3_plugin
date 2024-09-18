<!--
 Copyright 2023 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# otp_application

Welcome to the `otp_application` AtomVM application.

This application demonstrates how to build and deploy an AtomVM application that leverages the OTP application behavior.

This application consists of the `otp_application_app` module, which implements the OTP [Application](https://www.erlang.org/doc/design_principles/applications) behavior, the `otp_application_sup` module, which implements the OTP [Supervisor](https://www.erlang.org/doc/design_principles/sup_princ), and the `otp_application_worker` module, which implements the OTP [GenServer](https://www.erlang.org/doc/design_principles/gen_server_concepts) behavior.

The application and its dependencies are automatically started when the AtomVM application is run.  The worker module in this example is designed to send a tick message to itself once/second.  After 5 ticks, the application deliberately crashes and is restarted by the application's supervisor.

Note the definition of the application in the `otp_application.app.src` file:

    {application, otp_application, [
        {description, "An AtomVM application"},
        {vsn, "0.1.0"},
        {registered, []},
        {mod, {otp_application_app, #{crash_interval => 5}}},
        {applications, [
            kernel, stdlib
        ]},
        {env,[]},
        {modules, []},
        {licenses, ["Apache-2.0"]},
        {links, []}
    ]}.

Specifically, the `mod` entry tells the OTP framework which application entrypoint to load the run when AtomVM starts.

Note also the `packbeam` properties of the `atomvm_rebar3_plguin` entry in the `rebar3.config` file:

    {atomvm_rebar3_plugin, [
        {packbeam, [application, prune]}
    ]}.

This property tell the `atomvm_rebar3_plugin` to treat this project as an OTP application.  In this case, the project is expected to implement the OTP application behavior.  In addition, users need not (and should not) specify a start entrypoint for the application; starting the OTP application and its dependencies is performed automatically by the OTP framework.

## Building and running this application

To build the application, issue the `packbeam` task under the `atomvm` namespace to the [`rebar3`](https://rebar3.org) tool:

    shell$ rebar3 atomvm packbeam

If you have AtomVM installed on a generic UNIX host, you can run this application on the command line, supplying the generated AtomVM AVM file as an argument:

    shell$ atomvm _build/default/lib/otp_application.avm
    Starting otp_application_app with start type normal and start args #{crash_interval => 5} ...
    Starting otp_application_sup with args #{crash_interval => 5} ...
    Application otp_application started
    tick
    tick
    tick
    tick
    tick
    boom!
    CRASH
    ======
    pid: <0.5.0>

    Stacktrace:
    [{otp_application_worker,handle_info,2,[{file,"/home/joe/atomvm_rebar3_plugin/examples/otp_application/src/otp_application_worker.erl"},{line,56}]},{gen_server,loop,2,[{file,"/home/runner/AtomVM/libs/estdlib/bootstrap/gen_server.erl"},{line,468}]}]

For more information about how to build and flash this application on other platforms, see the [`atomvm_rebar3_plugin`](https://github.com/atomvm/atomvm_rebar3_plugin) Github repository.
