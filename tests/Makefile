ifeq ($(wildcard $(STDLIBDIR)/big_int.cmi),)
HAS_NUM=false
NUMS_CMA=
NUMS_CMXA=
else
HAS_NUM=true
NUMS_CMA=nums.cma
NUMS_CMXA=nums.cmxa
endif

include ../Makefile.config

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

test:: chi2.exe
	@echo "Testing random number generation..."
	@if ./chi2.exe; then echo "chi2: passed"; else echo "chi2: FAILED"; exit 2; fi

bench:: timings.exe
	./timings.exe

bench:: pi.exe
	@echo "Benchmarking pi"; time ./pi.exe 10000 > /dev/null

test:: tst_extract.exe
	@echo "Testing extract..."
	@if ./tst_extract.exe; then echo "tst_extract: passed"; else echo "tst_extract: FAILED"; exit 2; fi

test:: intern.exe
	@echo "Testing unmarshaling..."
	@if ./intern.exe extern.data32 | cmp -s intern.output32$(WORDSIZE) -; then echo "intern 32: passed"; else echo "intern 32: failed"; exit 2; fi
	@if ./intern.exe extern.data64 | cmp -s intern.output64$(WORDSIZE) -; then echo "intern 64: passed"; else echo "intern 64: failed"; exit 2; fi

extern.data$(WORDSIZE): extern.exe
	./extern.exe extern.data$(WORDSIZE)

tofloat.exe: tofloat.ml setround.$(OBJSUFFIX) ../zarith.cmxa
	ocamlopt -I .. -ccopt "-L.." zarith.cmxa -o tofloat.exe \
           setround.$(OBJSUFFIX) tofloat.ml

%.exe: %.ml ../zarith.cmxa
	ocamlopt -I .. -ccopt "-L.." zarith.cmxa $(NUMS_CMXA) -o $*.exe $*.ml
%.byt: %.ml ../zarith.cma
	ocamlc -I .. -ccopt "-L.." zarith.cma $(NUMS_CMA) -o $*.byt $*.ml
%.$(OBJSUFFIX): %.c
	ocamlc -c $*.c

clean:
	rm -f *.cm[iox] *.o *.obj *.exe *.byt
