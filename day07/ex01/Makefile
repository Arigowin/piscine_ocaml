NAME = ex01

SOURCES =	people.ml \
			doctor.ml \
			main.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

LIBS = 

all: $(NAME)

$(NAME): opt byt
	@rm -f $(NAME)
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
	@rm -f *.o

fclean: clean
	@rm -f $(NAME)
	@rm -f $(NAME).opt
	@rm -f $(NAME).byt

re: fclean all