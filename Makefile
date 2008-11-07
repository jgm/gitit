gitit: Gitit.hs Gitit/Git.hs Gitit/State.hs
	ghc --make -Wall -o gitit Gitit.hs -threaded

.PHONY: static clean
static:
	mkdir -p static/javascripts
	mkdir -p static/stylesheets
	cp javascripts/*.js static/javascripts/
	cp stylesheets/*.css static/stylesheets/

clean: 
	rm Gitit.hi Gitit.o Gitit/*.hi Gitit/*.o gitit

