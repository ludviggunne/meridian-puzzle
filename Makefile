SOURCES	:=	$(wildcard */Main.hs)
TARGETS	:=	$(SOURCES:%/Main.hs=%/Main)

all: $(TARGETS)

%/Main: %/Main.hs Game.hs
	ghc --make -o $(@) $(<)

clean:
	rm -f */*.o */*.hi */Main

.PHONY: clean
