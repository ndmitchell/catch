lhs2tex catch.tex -o final.tex
pdflatex final.tex
bibtex final
pdflatex final.tex
start final.pdf
