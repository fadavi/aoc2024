.PHONY: format day

format:
	@black src/

day%:
	./src/$@.py
