test:
	ocamlc -c unif.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o pprolog unif.cmo lexer.cmo parser.cmo main.cmo 
	rm lexer.ml  
	rm parser.ml parser.mli 
	rm *.cmo *.cmi
clean:	
	rm pprolog 

