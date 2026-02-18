#!/bin/sh
##
## Copyright (c) Winford (UncleGrumpy) <winford@object.stream>
## All rights reserved.
##
## SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

set -e

Args="${@}"

while [ "${1}" != "" ]; do
    case ${1} in
        "reboot" )
            shift
            if ( [ ${1} = "-f" ] && [ ${2} = "-u" ] ) then
                if [ -L $TEST_MYAPP_LOCK ] then
                    ## Since we are locked make sure mount doesn't exist or contain test artifacts
                    [-d $TEST_MYAPP_MOUNT] && rm -r $TEST_MYAPP_MOUNT
                    rm $TEST_MYAPP_LOCK
                    mkdir $TEST_MYAPP_MOUNT
                elif ([ -L $TEST_REBAR_OVERRIDES_LOCK ]) then
                    [-d $TEST_REBAR_OVERRIDES_MOUNT] && rm -r $TEST_REBAR_OVERRIDES_MOUNT
                    rm $TEST_REBAR_OVERRIDES_LOCK
                    mkdir $TEST_REBAR_OVERRIDES_MOUNT
                else
                    echo "No accessible RP-series devices in BOOTSEL mode were found."
                    exit 1
                fi
                echo "The device was asked to reboot into BOOTSEL mode."
            fi
            break
    esac
    shift
done

echo "${Args}"

exit 0
