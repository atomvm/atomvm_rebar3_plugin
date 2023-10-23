##
## Copyright (c) dushin.net
## All rights reserved.
##

all: compile edoc etest

compile:
	rebar3 compile

edoc:
	rebar3 edoc

etest:
	cd test && ./run.sh

clean:
	rm -rf _build

publish:
	rebar3 hex publish
