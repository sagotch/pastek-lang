SRC=index demo cheatsheet html_render
TMP=$(SRC:=.tmp)
HTML=$(SRC:=.html)

all: proper $(TMP) $(HTML)

%.html: %.tmp
	 pastek --full-document < $< > $@

%.tmp: %.pa
	@echo "%{ [css] urls = [\"style.css\"] %}" > $@
	@echo '{{{<div style="position:fixed;left:1rem;top:1rem;">' >> $@
	@echo '<a href="index.html">Return to index</a>' >> $@
	@echo '</div>}}}' >> $@
	@echo '{{{<div style="position:fixed;right:1rem;top:1rem;">' >> $@
	@echo '<a href="' $< '" target="_new">View Page Source</a>' >> $@
	@echo '</div>}}}' >> $@
	@cat $< >> $@

clean:
	rm -f *~ *.tmp

proper: clean
	rm -f *.html
