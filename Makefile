build:
	rebar compile

test: 
	rebar eunit skip_deps=true

run:
	erl -pa deps/*/ebin/ -pa ebin/ -eval "application:start(eric)"

.PHONY: test
