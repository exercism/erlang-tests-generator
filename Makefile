run: testgen
	./$< generate

testgen: _build/default/bin/testgen
	cp $< $@

_build/default/bin/testgen:
	rebar3 escriptize
.PHONY: _build/default/bin/testgen

typecheck:
	rebar3 dialyzer
