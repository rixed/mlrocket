OCAMLC   = ocamlfind ocamlc -thread
OCAMLOPT = ocamlfind ocamlopt -thread
OCAMLDEP = ocamlfind ocamldep
OCAMLOPTFLAGS = -w Ael -g -annot
OCAMLFLAGS    = -w Ael -g -annot

REQUIRES = geom glop bricabrac

.PHONY: all clean install

PROGRAMS = mlrocket.byte
SOURCES = mlrocket.ml pic.ml rocket.ml sparkle.ml world.ml game.ml main.ml

all: byte opt
byte: $(PROGRAMS)
opt: $(PROGRAMS:.byte=.opt)

NAME = mlrocket

install: all
	if test -f mlrocket.opt ; then extra=mlrocket.opt ; fi ; \
	ocamlfind install $(NAME) META mlrocket.byte $$extra

uninstall:
	ocamlfind remove $(NAME)

reinstall: uninstall install

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .opt .byte

mlrocket.byte: $(SOURCES:.ml=.cmo)
	$(OCAMLC) -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) $^

mlrocket.opt: $(SOURCES:.ml=.cmx)
	$(OCAMLOPT) -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLOPTFLAGS) $^

.ml.cmo:
	$(OCAMLC) -package "$(REQUIRES)" $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) -package "$(REQUIRES)" $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) -package "$(REQUIRES)" $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f *.cm[ioxa] *.cmxa *.a *.s *.o *.byte *.opt .depend

# Dependencies
.depend: *.ml
	$(OCAMLDEP) -package "$(REQUIRES)" -I .. $^ > $@

-include .depend
