#!/bin/sh

root_dir="$(cd $(dirname $0)/.. && pwd)"
apps_dir="${root_dir}/apps"

for app in $(/bin/ls ${apps_dir}); do
    cd ${apps_dir}/${app}
    rm -rf _build
    rm -rf _checkouts
    mkdir _checkouts
    cd _checkouts
    ln -s ../../../../.. atomvm_rebar3_plugin
done

echo "done"
