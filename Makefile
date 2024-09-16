

project: engine.cmo project.ml
	ocamlc -c project.ml
	ocamlc -o project unix.cma engine.cmo project.cmo

engine.cmo: engine.ml engine.cmi
	ocamlc -c engine.ml
	
engine.cmi: engine.ml
	ocamlc -c engine.mli
	
dist:
	mkdir projet
	cp engine.ml projet/engine.ml
	cp engine.mli projet/engine.mli
	cp project.ml projet/project.ml
	latexmk main.tex
	cp main.pdf projet/main.pdf
	cp Makefile projet/Makefile
	zip -r projet.zip projet

clean:
	rm -f *.cmo *.cmi project
	rm -rf projet
	rm -f projet.zip
