DIST := public/dist
ARTIFACT := $(DIST)/main.min.js

all: setup build

setup:
	npm install

build: $(ARTIFACT)

%.min.js: %.js
	npx uglifyjs $< --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
		| npx uglifyjs --mangle --output $@

$(DIST)/main.js: src/Main.elm
	elm make $< --optimize --output=$@

clean:
	rm -rf $(DIST)

start:
	ezserve -addr :8000 -root public

.PHONY: all setup clean start
