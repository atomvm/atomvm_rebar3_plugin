##
## Copyright (c) dushin.net
## All rights reserved.
##
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later


all: compile etest doc

compile:
	rebar3 compile

doc:
	rebar3 as doc ex_doc

etest:
	cd test && ./run.sh

clean:
	rm -rf _build
	rm -rf docs

publish: doc
	rebar3 as publish hex publish --doc-dir docs
