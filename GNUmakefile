
TARGETS = Examples/fgrep

all: $(TARGETS)
.PHONY: all always clean doc

WALL = -Wall -Werror

always:
	@:

Examples/%: always
	ghc --make -Wall -Werror $@.hs

Setup: Setup.hs
	ghc --make $(WALL) Setup.hs

doc: Setup
	./Setup configure --user
	./Setup haddock --hyperlink-source

clean:
	rm -rf $(TARGETS) Setup dist
	find . \( -name '*~' -o -name '*.hi' -o -name '*.o' \) -print0 \
		| xargs -0 rm -f --
