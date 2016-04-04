.PHONY: all
all: comparison-vanilla.png comparison-base3.png

%.dot: %.cry
	cryfsm $< -o $@

%.png: %.dot
	dot -Tpng $< >$@
