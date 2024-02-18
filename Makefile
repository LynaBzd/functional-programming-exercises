opam_exec := opam exec --switch $$PWD --

.PHONY : all
all : build

.PHONY : phony
phony :

.PHONY : build
build :
	@ $(opam_exec) dune build

.PHONY : test
test :
	@ $(opam_exec) dune exec test/test.exe -- test
test-% : phony
	@ $(opam_exec) dune exec test/test.exe -- test $*

.PHONY : top
top :
	@ $(opam_exec) dune utop . -- -init top.ml

.PHONY : clean
clean :
	@ $(opam_exec) dune clean
