all: occur

occur: occur.ml
	ocamlfind ocamlopt -package earley,earley.str -linkpkg -pp pa_ocaml -o $@ $^

clean:
	rm -f *.cmi *.cmx *.o *~

distclean: clean
	rm -f occur
