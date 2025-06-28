all : egggame/egggame.sdl.import.so egggame/egggame.glew.import.so egggame/egggame.devil.import.so egggame/egggame.glutil.import.so

egggame/egggame.sdl.import.so : egggame/egggame.sdl.import.scm
	chicken-csc -shared egggame/egggame.sdl.import.scm -L -lSDL3

egggame/egggame.glew.import.so : egggame/egggame.glew.import.scm
	chicken-csc -shared egggame/egggame.glew.import.scm \
    -L -lGLEW -L -lEGL -L -lGL -L -lGLU -L -lOpenGL

egggame/egggame.devil.import.so : egggame/egggame.devil.import.scm
	chicken-csc -shared egggame/egggame.devil.import.scm -L -lIL -L -lILU

egggame/egggame.glutil.import.so : egggame/egggame.glutil.import.scm
	chicken-csc -shared egggame/egggame.glutil.import.scm -I ./egggame

clean :
	rm -f egggame/egggame.sdl.import.so egggame/egggame.glew.import.so egggame/egggame.devil.import.so egggame/egggame.glutil.import.so
