default:one two three four

one:	eposter.tex
	latex -no-shell-escape eposter.tex
	sage eposter.sagetex.sage

two:
	latex -shell-escape eposter.tex

three:
	dvips eposter.dvi
four:
	ps2pdf eposter.ps

fourth:eposter.tex
	latex -no-shell-escape eposter.tex
	dvips eposter.dvi
	ps2pdf eposter.ps

clean: 
	rm -rf eposter.aux eposter.log eposter.dvi eposter.ps eposter.sagetex.py eposter.sagetex* temp.eps sage-plots-for* *~
