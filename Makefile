all:
	#elm make src/Main.elm
	#elm make src/Main.elm --optimize --output=main.js

reactor:
	elm reactor

.PHONY: all reactor
