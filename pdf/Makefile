.PHONY: clean

clean: 
	rm -f \
	main.aux \
	main.bbl \
	main.bcf \
	main.blg \
	main.log \
	main.out \
	main.toc \
	main.dvi \
	main.fls \
	main.run.xml \
	main.fdb_latexmk \
	main.synctex.gz \
	2>/dev/null

publish: main.tex references.bib 
	latexmk && \
	make clean
