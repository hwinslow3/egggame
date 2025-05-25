all : egggame/egggame.sdl.import.so

egggame/egggame.sdl.import.so : egggame/egggame.sdl.import.scm
	chicken-csc -shared egggame/egggame.sdl.import.scm -L -lSDL3

clean :
	rm -f egggame/egggame.sdl.import.so
