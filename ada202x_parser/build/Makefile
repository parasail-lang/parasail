# Makefile used at AdaCore to build ada202x lexer/parser
# You must run this Makefile from parser/build

PROCS=0
RM=rm -rf
GNATCHOP=gnatchop
GNATMAKE=gnatmake

.PHONY: all clean ada202x-parser

all: build

build: ada202x-parser

ada202x-parser: ../gen/ada202x_tokens.ads

../gen/ada202x_tokens.ads: ../ada202x_lex.l ../ada202x.y
	@rm -f bin
	@ln -s ../../parser/build/bin bin
	@mkdir -p ../gen
	$(RM) ada202x.y ada202x_lex.l ada202x.verbose
	cp ../ada202x.y ../ada202x_lex.l .
	bin/ayacc ada202x.y verbose "=>" on debug "=>" off
	bin/aflex -s ada202x_lex.l
	cd ../gen; $(GNATCHOP) -w ../build/ada202x*.ad?
	$(RM) ada202x*.ad? ada202x.y ada202x_lex.l

clean:
	$(RM) ../gen/* ada202x*.ad? *.verbose bin
