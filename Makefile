all : egggame/egggame.sdl.import.so egggame/egggame.glew.import.so

egggame/egggame.sdl.import.so : egggame/egggame.sdl.import.scm
	chicken-csc -shared egggame/egggame.sdl.import.scm -L -lSDL3

egggame/egggame.glew.import.so : egggame/egggame.glew.import.scm
	chicken-csc -shared egggame/egggame.glew.import.scm \
    -L -lGLEW -L -lEGL -L -lGL -L -lGLU -L -lOpenGL

clean :
	rm -f egggame/egggame.sdl.import.so egggame/egggame.glew.import.so
