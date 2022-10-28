# Makefile used at AdaCore to build Ada202X reference manual
# You must run this Makefile from the documentation/ref_manual subdirectory

PROCS=0
RM=rm -rf
PDFLATEX=pdflatex
LATEXHTML=latex2html -local_icons

.PHONY: clean ref-manual

all: ref-manual

ref-manual: ada202x_ref_manual.pdf

ada202x_ref_manual.pdf: ada202x_ref_manual.ltx
	$(PDFLATEX) ada202x_ref_manual.ltx
	$(PDFLATEX) ada202x_ref_manual.ltx
	$(PDFLATEX) ada202x_ref_manual.ltx
	-$(LATEXHTML) ada202x_ref_manual.ltx
	-$(LATEXHTML) ada202x_ref_manual.ltx

clean:
	$(RM) *.pdf *.aux *.toc *.log
	-$(RM) -rf ada202x_ref_manual
