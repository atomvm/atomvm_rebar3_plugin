##
## Copyright (c) dushin.net
## All rights reserved.
##

all: compile doc etest

compile:
	rebar3 compile

doc:
	rebar3 ex_doc

etest:
	cd test && ./run.sh

clean:
	rm -rf _build

publish:
	rebar3 hex publish --doc-dir docs
