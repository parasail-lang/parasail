# Makefile used at AdaCore to build Javallel lexer/parser
# You must run this Makefile from parser/build

PROCS=0
RM=rm -rf
GNATCHOP=gnatchop
GNATMAKE=gnatmake

.PHONY: all clean javallel-parser

all: build

build: javallel-parser

javallel-parser: ../gen/javallel_tokens.ads

../gen/javallel_tokens.ads: ../javallel_lex.l ../javallel.y
	@rm -f bin
	@ln -s ../../parser/build/bin bin
	@mkdir -p ../gen
	$(RM) javallel.y javallel_lex.l javallel.verbose
	cp ../javallel.y ../javallel_lex.l .
	bin/ayacc javallel.y verbose "=>" on debug "=>" off
	bin/aflex -s javallel_lex.l
	cd ../gen; $(GNATCHOP) -w ../build/javallel*.ad?
	$(RM) javallel*.ad? javallel.y javallel_lex.l

clean:
	$(RM) ../gen/* javallel*.ad? *.verbose bin
