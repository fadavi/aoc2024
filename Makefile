.PHONY: format day

format:
	@black src/

day05:
	./src/day05.lua

day08:
	./src/day08.js

day%:
	./src/$@.py
