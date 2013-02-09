REBAR = `which rebar`

all: deps compile

deps:
	@( $(REBAR) get-deps )

compile:
	@( $(REBAR) compile )

run:
	@( erl -pa ebin deps/*/ebin -s emailchecker )

test:
	@( $(REBAR) eunit )

.PHONY: all deps compile run test
