all:
	alex Tokens.x
	happy Parser.y
	ghc --make Main.hs