gitit: Gitit.hs Gitit/Git.hs Gitit/State.hs Gitit/HAppS.hs Gitit/HStringTemplate.hs
	ghc --make -Wall -o gitit Gitit.hs -threaded -idata

.PHONY: static clean
static:
	mkdir -p static/js
	mkdir -p static/css
	mkdir -p static/img
	cp js/*.js static/js/
	cp css/*.css static/css/
	cp -r img/* static/img/

clean: 
	rm Gitit.hi Gitit.o Gitit/*.hi Gitit/*.o gitit

