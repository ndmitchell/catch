lhs2tex catch.tex -o final.tex
bibtex final
texify final.tex
del catch.dvi
copy final.dvi catch.dvi
