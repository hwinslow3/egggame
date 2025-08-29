all : egggame/egggame.sdl.import.so egggame/egggame.glew.import.so egggame/egggame.devil.import.so egggame/egggame.glutil.import.so egggame/egggame.matrix.import.so egggame/egggame.tile-collection.import.so egggame/egggame.sdlutil.import.so egggame/egggame.tiled-file.import.so

egggame/egggame.sdl.import.so : egggame/egggame.sdl.import.scm
	chicken-csc -shared egggame/egggame.sdl.import.scm -L -lSDL3

egggame/egggame.sdlutil.import.so : egggame/egggame.sdlutil.import.scm egggame/egggame.sdl.import.so
	chicken-csc -shared egggame/egggame.sdlutil.import.scm -I ./egggame

egggame/egggame.tiled-file.import.so : egggame/egggame.tiled-file.import.scm
	chicken-csc -shared egggame/egggame.tiled-file.import.scm -I ./egggame

egggame/egggame.glew.import.so : egggame/egggame.glew.import.scm
	chicken-csc -shared egggame/egggame.glew.import.scm \
    -L -lGLEW -L -lEGL -L -lGL -L -lGLU -L -lOpenGL

egggame/egggame.devil.import.so : egggame/egggame.devil.import.scm
	chicken-csc -shared egggame/egggame.devil.import.scm -L -lIL -L -lILU

egggame/egggame.glutil.import.so : egggame/egggame.glutil.import.scm
	chicken-csc -shared egggame/egggame.glutil.import.scm -I ./egggame

egggame/egggame.matrix.import.so : egggame/egggame.matrix.import.scm
	chicken-csc -shared egggame/egggame.matrix.import.scm -I ./egggame

egggame/egggame.tile-collection.import.so : egggame/egggame.tile-collection.import.scm egggame/egggame.matrix.import.so egggame/egggame.glutil.import.so
	chicken-csc -shared egggame/egggame.tile-collection.import.scm -I ./egggame

clean :
	rm -f egggame/egggame.sdl.import.so egggame/egggame.glew.import.so egggame/egggame.devil.import.so egggame/egggame.glutil.import.so egggame/egggame.matrix.import.so egggame/egggame.tile-collection.import.so egggame/egggame.sdlutil.import.so egggame/egggame.tiled-file.import.so
