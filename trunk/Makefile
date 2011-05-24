# This file is part of the Zarith library 
# http://forge.ocamlcore.org/projects/zarith .
# It is distributed under LGPL 2 licensing, with static linking exception.
# See the LICENSE file included in the distribution.
#   
# Copyright (c) 2010-2011 Antoine Miné, Abstraction project.
# Abstraction is part of the LIENS (Laboratoire d'Informatique de l'ENS),
# a joint laboratory by:
# CNRS (Centre national de la recherche scientifique, France),
# ENS (École normale supérieure, Paris, France),
# INRIA Rocquencourt (Institut national de recherche en informatique, France).


ARCH = $(shell uname -m)

OCAMLC = ocamlc.opt
OCAMLOPT = ocamlopt.opt
OCAMLDEP = ocamldep
OCAMLFLAGS =
OCAMLOPTFLAGS =
CFLAGS = -O3 -Wall -Wextra

LIBS = -lgmp
#LIBS = -lmpir

CSRC = caml_z.c
SSRC = $(wildcard caml_z_$(ARCH).S)
MLSRC = z.ml
MLISRC = z.mli

AUTOGEN = z.ml z.mli

all: test test.b bitest

test: $(SSRC:%.S=%.o) $(CSRC:%.c=%.o) $(MLSRC:%.ml=%.cmx) test.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLINC) $+ -cclib "$(LIBS)" -o $@

test.b: $(CSRC:%.c=%.o) $(MLSRC:%.ml=%.cmo) test.cmo
	$(OCAMLC) -custom $(OCAMLFLAGS) $(OCAMLINC) $+ -cclib "$(LIBS)" -o $@

rtest: $(SSRC:%.S=%.o) $(CSRC:%.c=%.o) $(MLSRC:%.ml=%.cmx) rtest.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLINC) gmp.cmxa bigarray.cmxa $+ -cclib "$(LIBS)" -o $@

bitest: $(SSRC:%.S=%.o) $(CSRC:%.c=%.o) $(MLSRC:%.ml=%.cmx) big_int_Z.cmx bitest.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLINC) nums.cmxa  $+ -cclib "$(LIBS)" -o $@


$(AUTOGEN): z.mlp z.mlip $(SSRC) z_pp.pl
	./z_pp.pl $(ARCH)

%.o: %.c
	$(OCAMLC) -ccopt "$(CFLAGS)" -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $<

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -c $<

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -c $<


clean:
	/bin/rm -rf *.o *.a *.so *.cmi *.cmo *.cmx *.cmxa *.cma *~ \#* depend test $(AUTOGEN)
	/bin/rm -rf test test.b rtest bitest

depend: $(AUTOGEN)
	$(OCAMLDEP) -native $(OCAMLINC) *.ml *.mli > depend

include depend

.PHONY: clean
