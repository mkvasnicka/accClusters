MAINNAME=dokumentace
SOURCE=$(MAINNAME).Rmd
TARGET=$(MAINNAME).pdf
OTHERFILES=dokumentace.theme references.bib title_fig.pdf

man:	$(TARGET)
$(TARGET):	$(SOURCE) $(OTHERFILES)
	Rscript -e "rmarkdown::render('dokumentace.Rmd', 'pdf_document')"

clean:
	rm -f *.log
