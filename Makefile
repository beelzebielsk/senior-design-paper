full-project-notes.pdf : full-project-notes.tex
	pdflatex $<

full-project-notes.tex : full-project-notes.tex.pm pollen.rkt template.tex.p
	raco pollen render $@
