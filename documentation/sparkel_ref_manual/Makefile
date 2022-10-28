# Makefile used at AdaCore to build Sparkel reference manual
# You must run this Makefile from the documentation/ref_manual subdirectory

PROCS=0
RM=rm -rf
PDFLATEX=pdflatex
LATEXHTML=latex2html -local_icons

.PHONY: clean ref-manual

all: ref-manual

ref-manual: sparkel_ref_manual.pdf

sparkel_ref_manual.pdf: sparkel_ref_manual.ltx
	$(PDFLATEX) sparkel_ref_manual.ltx
	$(PDFLATEX) sparkel_ref_manual.ltx
	$(PDFLATEX) sparkel_ref_manual.ltx
	-$(LATEXHTML) sparkel_ref_manual.ltx
	-$(LATEXHTML) sparkel_ref_manual.ltx

clean:
	$(RM) *.pdf *.aux *.toc *.log
	-$(RM) -rf sparkel_ref_manual
