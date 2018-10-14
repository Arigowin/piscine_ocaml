RESULT = tama
SOURCES =	pet.ml \
			graph.ml \
			main.ml
LIBS = bigarray sdl sdlttf sdlloader
INCDIRS = +sdl ~/.brew/lib
THREADS = true
OCAMLLDFLAGS = -cclib "-framework Cocoa"
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
install:
	brew install sdl ocamlsdl sdl_ttf sdl_mixer sdl_gfx sdl_image
uninstall:
	brew uninstall sdl_gfx sdl_ttf sdl_mixer sdl_image sdl ocamlsdl

# brew install sdl ; opam install ocamlsdl ; brew install sdl_ttf sdl_mixer sdl_gfx sdl_image ; opam install conf-sdl-ttf conf-sdl-image conf-sdl-mixer conf-sdl-gfx ocamlsdl ; brew install ocamlsdl
# opam uninstall conf-sdl-ttf conf-sdl-image conf-sdl-mixer conf-sdl-gfx ocamlsdl ; brew uninstall sdl_gfx sdl_ttf sdl_mixer sdl_image sdl ocamlsdl