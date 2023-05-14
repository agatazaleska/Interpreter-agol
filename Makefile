all:
	mkdir -p build
	ghc --make -outputdir build -o interpreter Main.hs

clean:
	rm -r build/ interpreter