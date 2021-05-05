all:

_out/%.html: src/%.elm _out
	elm make $< --output=$@

_out:
	mkdir -p $@

reactor:
	elm reactor

clean:
	rm -rf _out

test:
	npx elm-test

.PHONY: all reactor clean test
