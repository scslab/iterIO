
TARGETS = Examples/fgrep

all: $(TARGETS)
.PHONY: all always clean

always:
	@:

Examples/%: always
	ghc --make -Wall -Werror $@.hs

clean:
	rm -f $(TARGETS)
	find . \( -name '*~' -o -name '*.hi' -o -name '*.o' \) -print0 \
		| xargs -0 rm -f --
