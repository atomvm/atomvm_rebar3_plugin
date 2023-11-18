##
## Copyright (c) dushin.net
## All rights reserved.
##

all: compile etest doc

compile:
	rebar3 compile

doc:
	rebar3 as doc ex_doc

etest:
	cd test && ./run.sh

clean:
	rm -rf _build

publish: doc
	rebar3 as publish hex publish --doc-dir docs
