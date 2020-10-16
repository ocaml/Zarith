WORDSIZE:=$(shell echo 'print_int Sys.word_size' | ocaml -stdin)
STDLIBDIR:=$(shell ocamlc -where)
ifeq ($(wildcard $(STDLIBDIR)/big_int.cmi),)
HAS_NUM=false
NUMS_CMA=
NUMS_CMXA=
else
HAS_NUM=true
NUMS_CMA=nums.cma
NUMS_CMXA=nums.cmxa
endif

test:: zq.exe
	@echo "Testing zq (native)..."
	@if ./zq.exe | cmp -s zq.output$(WORDSIZE) - ; then echo "zq: passed"; else echo "zq: FAILED"; exit 2; fi

test:: zq.byt
	@echo "Testing zq (bytecode)..."
	@if ocamlrun -I .. ./zq.byt | cmp -s zq.output$(WORDSIZE) - ; then echo "zq: passed"; else echo "zq: FAILED"; exit 2; fi

ifeq ($(HAS_NUM),true)
test:: bi.exe
	@echo "Testing bi..."
	@if ./bi.exe; then echo "bi: passed"; else echo "bi: FAILED"; exit 2; fi
endif

test:: pi.exe
	@echo "Testing pi..."
	@if ./pi.exe 500 | cmp -s pi.output - ; then echo "pi: passed"; else echo "pi: FAILED"; exit 2; fi

test:: tofloat.exe
	@echo "Testing tofloat..."
	@./tofloat.exe

test:: ofstring.exe
	@echo "Testing ofstring..."
	@./ofstring.exe

bench:: timings.exe
	./timings.exe

bench:: pi.exe
	@echo "Benchmarking pi"; time ./pi.exe 10000 > /dev/null

tofloat.exe: tofloat.ml setround.o ../zarith.cmxa
	ocamlopt -I .. -ccopt "-L.." zarith.cmxa -o tofloat.exe \
           setround.o tofloat.ml

%.exe: %.ml ../zarith.cmxa
	ocamlopt -I .. -ccopt "-L.." zarith.cmxa $(NUMS_CMXA) -o $*.exe $*.ml
%.byt: %.ml ../zarith.cma
	ocamlc -I .. -ccopt "-L.." zarith.cma $(NUMS_CMA) -o $*.byt $*.ml
%.o: %.c
	ocamlc -c $*.c

clean:
	rm -f *.cm[iox] *.o *.exe *.byt
