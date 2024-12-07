.PHONY: format day

format:
	@black src/

day05:
	./src/day05.lua

day%:
	./src/$@.py
