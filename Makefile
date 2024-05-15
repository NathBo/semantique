# Cours "Semantics and applications to verification"
#
# Ecole normale supérieure, Paris, France / CNRS / INRIA

.PHONY: all install clean cleantest doc compress

all:
	@dune build analyzer @install

install:
	@dune install --prefix=.

clean: cleantest
	@dune clean
	@rm -rf _build/ bin/ lib/ *~ */*~
	@rm -rf *.dot *.pdf *.svg */*.dot */*.pdf */*.svg *.tar.gz

cleantest:
	@rm -rf results

test: cleantest all
	@scripts/test.sh

doc: all
	@dune build @doc-private

compress: clean
	@tar -czvf ../project-semantics.tar.gz --exclude=".git*" ../project-semantics
	@mv ../project-semantics.tar.gz .

run: all
	@./_build/default/analyzer/analyzer.exe $(file)
#utiliser sous forme "make file=examples/bool/assert_false_false.c run"

check: all
	@./_build/default/analyzer/analyzer.exe test.c
check-domain: all
	@./_build/default/analyzer/analyzer.exe test.c --domain $(domain)
# comme check mais on peut specifier le domaine
#utiliser sous forme "make domain=constant check-domain"

