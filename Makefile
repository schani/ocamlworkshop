OC = ocamlopt
ASTAR_OBJS = pqueue.cmx astar.cmx bruecke.cmx
CHESS_OBJS = iterdeep.cmx chess.cmx

all : astar chess

astar : $(ASTAR_OBJS)
	$(OC) -o astar $(ASTAR_OBJS)

chess : $(CHESS_OBJS)
	$(OC) -o chess $(CHESS_OBJS)

%.cmi : %.mli
	$(OC) -c $<

%.cmo : %.ml
	$(OC) -c $<

%.cmx : %.ml
	$(OC) -c $<

%.mli : %.ml
	$(OC) -i $< >$@

clean :
	rm -f *~ core *.cmi *.cmo *.cmx *.o *.mli astar chess
