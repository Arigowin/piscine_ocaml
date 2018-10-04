NAME = ft_graphics

SOURCES = ft_graphics.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

LIBS = $(WITHGRAPHICS)
WITHGRAPHICS = graphics.cma -cclib -lgraphics

all: depend $(NAME)

$(NAME): opt byt
	@ln -s $(NAME).byt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

$(NAME).byt:		$(OBJS)
	@$(CAMLC) -o $(NAME).byt $(LIBS) $(OBJS)

$(NAME).opt:		$(OPTOBJS)
	@$(CAMLOPT) -o $(NAME).opt $(LIBS:.cma=.cmxa) $(OPTOBJS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	@$(CAMLC) -c $<

.mli.cmi:
	@$(CAMLC) -c $<

.ml.cmx:
	@$(CAMLOPT) -c $<

clean:
	@rm -f *.cm[iox] *~ .*~ #*#
	@rm -f $(NAME).o

fclean: clean
	@rm -f $(NAME)
	@rm -f $(NAME).opt
	@rm -f $(NAME).byt

depend: .depend
	@$(CAMLDEP) $(SOURCES) > .depend

re: fclean all

include .depend