# Makefile used at AdaCore to build parython lexer/parser
# You must run this Makefile from parser/build

PROCS=0
RM=rm -rf
GNATCHOP=gnatchop
GNATMAKE=gnatmake

.PHONY: all clean parython-parser

all: build

build: parython-parser

parython-parser: ../gen/parython_tokens.ads

../gen/parython_tokens.ads: ../parython_lex.l ../parython.y
	@rm -f bin
	@ln -s ../../parser/build/bin bin
	@mkdir -p ../gen
	$(RM) parython.y parython_lex.l parython.verbose
	cp ../parython.y ../parython_lex.l .
	bin/ayacc parython.y verbose "=>" on debug "=>" off
	bin/aflex -s parython_lex.l
	cd ../gen; $(GNATCHOP) -w ../build/parython*.ad?
	$(RM) parython*.ad? parython.y parython_lex.l

clean:
	$(RM) ../gen/* parython*.ad? *.verbose bin
