#!/usr/bin/env sh
##
## Copyright (c) Winford (UncleGrumpy) <winford@object.stream>
## All rights reserved.
##
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

binary=${ATOMVM_REBAR3_PLUGIN_PARTITION_DATA:=${ATOMVM_REBAR3_PLUGIN_ESP32_PARTITION_DUMP}}

while [ "${#}" -gt 0 ]; do
    case ${1} in
        --port | -p ) shift
            if [ "${1}" = "bad" ]; then
                exit 2
            fi ;;
        read_flash ) shift
            if [ "${1}" = "0x8000" ] && { [ "${2}" = "0xC00" ] || [ "${2}" = "0xc00" ]; }; then
                target=${3}
                cp "${binary}" "${target}"
                unset ATOMVM_REBAR3_PLUGIN_PARTITION_DATA
                exit 0
            fi ;;
        write_flash ) exit 0 ;;
    esac
    shift
done

echo "${@}"

exit 0
