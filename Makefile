.PHONY: format day

format:
	@black src/

day05:
	./src/day05.lua

day08:
	./src/day08.js

day10:
	./src/day10.lisp

day%:
	./src/$@.py
