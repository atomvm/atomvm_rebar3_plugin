#!/bin/sh
##
## Copyright (c) dushin.net
## All rights reserved.
##
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

root_dir="$(cd $(dirname $0)/.. && pwd)"
apps_dir="${root_dir}/apps"

rm -rf ${root_dir}/_build

for app in $(/bin/ls ${apps_dir}); do
    cd ${apps_dir}/${app}
    rm -rf _build
    rm -rf _checkouts
done

echo "done"
