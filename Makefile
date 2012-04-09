
all: minii

lex.hs: lex.x
	alex lex.x

parse.hs: parse.y
	happy parse.y

minii: lex.hs parse.hs main.hs
	ghc --make main.hs -o minii

clean:
	rm -f *.hi
	rm -f *.o
	rm -f minii
	rm -f lex.hs
	rm -f parse.hs

