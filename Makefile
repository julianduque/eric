test: 
	rebar eunit skip_deps=true

build:
	rebar compile

.PHONY: test
