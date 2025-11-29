all:
	ghc Main.hs -o juego -threaded -package mtl -package gloss 

run: all
	./juego

clean:
	rm -f juego Main.o Main.hi
