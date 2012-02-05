CC=ocamlc

FILENAME=main

all: $(FILENAME)

$(FILENAME): Board.cmo Backtracking.cmo
	$(CC) -o $@ $^

%.cmo: %.ml %.cmi
	$(CC) -c $<

%.cmi: %.mli
	$(CC) -c $<

clean:
	rm -f *.cmi *.cmo $(FILENAME)
