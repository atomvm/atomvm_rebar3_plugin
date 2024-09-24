#!/bin/sh
##
## Copyright (c) dushin.net
## All rights reserved.
##
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

set -e

curr_dir=$(cd $(dirname $0) && pwd)
test_dir="${curr_dir}/driver"

unset ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_ESPTOOL
unset ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_CHIP
unset ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_PORT
unset ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_BAUD
unset ATOMVM_REBAR3_PLUGIN_ESP32_FLASH_OFFSET

unset ATOMVM_REBAR3_PLUGIN_STM32_STFLASH
unset ATOMVM_REBAR3_PLUGIN_STM32_FLASH_OFFSET

unset ATOMVM_REBAR3_PLUGIN_PICO_MOUNT_PATH
unset ATOMVM_REBAR3_PLUGIN_PICO_RESET_DEV

unset ATOMVM_REBAR3_PLUGIN_UF2CREATE_START

cd "${test_dir}"
rebar3 escriptize
./_build/default/bin/driver -r "$(pwd)" "$@"
