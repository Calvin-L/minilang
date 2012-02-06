
all: minii

lex.hs: lex.x
	alex lex.x

parse.hs: parse.y
	happy parse.y

minii: lex.hs parse.hs main.hs
	ghc -optl "-Wl,-read_only_relocs,suppress" --make main.hs -o minii

clean:
	rm -f *.hi
	rm -f *.o
	rm -f minic
	rm -f lex.hs
	rm -f parse.hs

