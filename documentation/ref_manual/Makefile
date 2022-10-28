# Makefile used at AdaCore to build ParaSail reference manual
# You must run this Makefile from the documentation/ref_manual subdirectory

PROCS=0
RM=rm -rf
PDFLATEX=pdflatex
LATEXHTML=latex2html -local_icons

.PHONY: clean ref-manual

all: ref-manual

ref-manual: parasail_ref_manual.pdf

parasail_ref_manual.pdf: parasail_ref_manual.ltx
	$(PDFLATEX) parasail_ref_manual.ltx
	$(PDFLATEX) parasail_ref_manual.ltx
	$(PDFLATEX) parasail_ref_manual.ltx
	-$(LATEXHTML) parasail_ref_manual.ltx
	-$(LATEXHTML) parasail_ref_manual.ltx

clean:
	$(RM) *.pdf *.aux *.toc *.log
	-$(RM) -rf parasail_ref_manual
