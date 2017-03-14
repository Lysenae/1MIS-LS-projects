# Projekt: FLP #1 - plg-2-nka
# Autor:   Daniel Klimaj, xklima22@stud.fit.vutbr.cz
# Datum:   2017-02-27
# Soubor:  Makefile

FILE = plg-2-nka

all:
	ghc --make $(FILE)

clean:
	rm -f $(FILE)
	find . -name "*.o" -type f -delete
	find . -name "*.hi" -type f -delete
	find . -name ".fuse_*" -type f -delete

git: clean
	git add --all
	git commit
	git push -u origin master
