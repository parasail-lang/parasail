# Makefile used at AdaCore to build ParaSail lexer/parser
# You must run this Makefile from parser/build

PROCS=0
RM=rm -rf
GNATCHOP=gnatchop
GNATMAKE=gnatmake

.PHONY: all clean clean-all ayacc aflex parasail-parser

all: build

build: ayacc aflex parasail-parser

ayacc:
	@mkdir -p bin
	@mkdir -p ayacc
	cd ayacc; $(GNATMAKE) -I../../../aflex_ayacc/ayacc ../../../aflex_ayacc/ayacc/ayacc.adb -o ../bin/ayacc

aflex:
	@mkdir -p bin
	@mkdir -p aflex
	cd aflex; $(GNATMAKE) -I../../../aflex_ayacc/aflex -I../../../aflex_ayacc/aflex/GEN ../../../aflex_ayacc/aflex/aflex.adb -o ../bin/aflex
	
parasail-parser: ../gen/parasail_tokens.ads

../gen/parasail_tokens.ads: ../parasail_lex.l ../parasail.y
	@mkdir -p ../gen
	$(RM) parasail.y parasail_lex.l parasail.verbose
	cp ../parasail.y ../parasail_lex.l .
	bin/ayacc parasail.y verbose "=>" on
	bin/aflex -s parasail_lex.l
	cd ../gen; $(GNATCHOP) -w ../build/parasail*.ad?
	$(RM) parasail*.ad? parasail.y parasail_lex.l

clean:
	$(RM) ../gen/* parasail*.ad? *.verbose aflex ayacc

clean-all: clean
	$(RM) bin
