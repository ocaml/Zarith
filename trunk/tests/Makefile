test:: zq.exe
	@echo "Testing zq (native)..."
	@if ./zq.exe | cmp -s zq.output - ; then echo "zq: passed"; else echo "zq: FAILED"; exit 2; fi

test:: zq.byt
	@echo "Testing zq (bytecode)..."
	@if ocamlrun -I .. ./zq.byt | cmp -s zq.output - ; then echo "zq: passed"; else echo "zq: FAILED"; exit 2; fi

test:: bi.exe
	@echo "Testing bi..."
	@if ./bi.exe; then echo "bi: passed"; else echo "bi: FAILED"; exit 2; fi

test:: pi.exe
	@echo "Testing pi..."
	@if ./pi.exe 500 | cmp -s pi.output - ; then echo "pi: passed"; else echo "pi: FAILED"; exit 2; fi

bench:: fib.exe
	@for k in int natint Z bigint num; do echo "Benchmarking fib $$k"; time ./fib.exe $$k 45 > /dev/null; done

bench:: fact.exe
	@echo "Benchmarking fact 10"; time ./fact.exe 10 100000000 > /dev/null
	@echo "Benchmarking fact 40"; time ./fact.exe 40 10000000 > /dev/null
	@echo "Benchmarking fact 200"; time ./fact.exe 200 1000000 > /dev/null

bench:: tak.exe
	@for k in int natint Z bigint num; do echo "Benchmarking tak $$k"; time ./tak.exe $$k 5000 > /dev/null; done

bench:: pi.exe
	@echo "Benchmarking pi"; time ./pi.exe 10000 > /dev/null

%.exe: %.ml ../zarith.cmxa
	ocamlopt -I .. -ccopt "-L.." zarith.cmxa nums.cmxa -o $*.exe $*.ml
%.byt: %.ml ../zarith.cma
	ocamlc -I .. -ccopt "-L.." zarith.cma nums.cma -o $*.byt $*.ml

clean:
	rm -f *.cm[iox] *.o *.exe *.byt
