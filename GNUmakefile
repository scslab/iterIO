
TARGETS = Examples/fgrep Examples/reliable/reference	\
Examples/reliable/tester

all: $(TARGETS) Setup
	./Setup configure --user
	./Setup build

.PHONY: all always clean doc

GHC = ghc -XForeignFunctionInterface -XFlexibleInstances $(WALL)
WALL = -Wall -Werror

always:
	@:

Examples/reliable/%: always
	$(GHC) --make -iExamples/reliable -Wall -Werror $@.hs

Examples/%: always
	$(GHC) --make $@.hs

Setup: Setup.hs
	$(GHC) --make Setup.hs

doc: Setup
	./Setup configure --user
	./Setup haddock --hyperlink-source

clean:
	rm -rf $(TARGETS) Setup dist
	find . \( -name '*~' -o -name '*.hi' -o -name '*.o' \) -print0 \
		| xargs -0 rm -f --
