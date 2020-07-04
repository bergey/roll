build:
	@mkdir -p output/objects
	ghc -o output/hello -outputdir output/objects -isrc app/hello.hs

clean:
	rm -rf output

run: build
	./output/hello
