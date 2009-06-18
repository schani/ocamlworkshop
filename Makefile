SRC = pqueue.ml astar.ml bruecke.ml

astar : $(SRC)
	ocamlopt -o astar $(SRC)

clean :
	rm -f astar *.cmo *.cmx *.cmi *.o *~
