#!/bin/sh
##
## Copyright (c) Winford (UncleGrumpy) <winford@object.stream>
## All rights reserved.
##
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

while [ "${1}" != "" ]; do
    case ${1} in
        read_flash )    shift
                        if ( [ ${1} = "0x8000" ] && [ ${2} = "0xc00" ] ); then
                            target=${3}
                            cp ${ATOMVM_REBAR3_PLUGIN_PARTITION_DATA:=${ATOMVM_REBAR3_PLUGIN_ESP32_PARTITION_DUMP}} ${target}
                            unset ATOMVM_REBAR3_PLUGIN_PARTITION_DATA
                            break;
                        fi
    esac
    shift
done

echo "${@}"
