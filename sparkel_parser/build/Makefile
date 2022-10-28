# Makefile used at AdaCore to build Sparkel lexer/parser
# You must run this Makefile from parser/build

PROCS=0
RM=rm -rf
GNATCHOP=gnatchop
GNATMAKE=gnatmake

.PHONY: all clean sparkel-parser

all: build

build: sparkel-parser

sparkel-parser: ../gen/sparkel_tokens.ads

../gen/sparkel_tokens.ads: ../sparkel_lex.l ../sparkel.y
	@rm -f bin
	@ln -s ../../parser/build/bin bin
	@mkdir -p ../gen
	$(RM) sparkel.y sparkel_lex.l sparkel.verbose
	cp ../sparkel.y ../sparkel_lex.l .
	bin/ayacc sparkel.y verbose "=>" on debug "=>" off
	bin/aflex -s sparkel_lex.l
	cd ../gen; $(GNATCHOP) -w ../build/sparkel*.ad?
	$(RM) sparkel*.ad? sparkel.y sparkel_lex.l

clean:
	$(RM) ../gen/* sparkel*.ad? *.verbose bin
