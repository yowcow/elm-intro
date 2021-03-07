all:

_out/%.html: src/%.elm _out
	elm make $< --output=$@

_out:
	mkdir -p $@

reactor:
	elm reactor

clean:
	rm -rf _out

.PHONY: all reactor clean
