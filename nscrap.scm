(import (srfi-151))
(import (egggame sdl)
        (egggame glew)
        (egggame devil)
        (srfi-4))
(import (chicken format) (chicken blob) (chicken port))

(load "nscrap-code.scm")
(load "scrap-matrix.scm")

(or (SDL_InitSubSystem SDL_INIT_VIDEO)
    (error (format "failed to init video: ~a" (SDL_GetError))))

(ilInit)

(define +window-dimensions+ '(400 300))

(define *window*
  (or (SDL_CreateWindow "fucking around"
                        (car +window-dimensions+) (cadr +window-dimensions+)
                        (bitwise-ior SDL_WINDOW_OPENGL))
      (error (format "failed to create window: ~a" (SDL_GetError)))))

(define *gl-context* (SDL_GL_CreateContext *window*))

(unless (>= (SDL_GL_GetAttribute SDL_GL_CONTEXT_MAJOR_VERSION) 2)
  (error "gl version too low"))

(unless (= (glewInit) GLEW_OK)
  (error "failed to initialize glew"))

(or (SDL_GL_SetSwapInterval 1)
    (error "failed to set swap interval" (SDL_GetError)))

(define *ev* (make-SDL_Event))

(define running? #t)

;; main regulator
(define regulate! (make-fps-regulator 29))

(define +cycle+ 650.0)

(glEnable GL_BLEND)
(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

;; tile lib
(define *tile-coll* (make-tile-collection screen-dimensions: +window-dimensions+))
(check-gl-error "post-create-tile-coll")

(define *tilesets* (tile-collection-add-tilesets!
                    *tile-coll*
                    '((tileset-1 . "scrap-images/gl2-hello-1.png")
                      (tileset-2 . "scrap-images/gl2-hello-2.png")
                      (tileset-3 . "scrap-images/gl2-hello-3.png")
                      (tileset-4 . "scrap-images/gl2-hello-4.png")
                      )))
(check-gl-error "post-add-tilesets")

(define *tileset-1* (alist-ref 'tileset-1 *tilesets*))
(define *tileset-2* (alist-ref 'tileset-2 *tilesets*))
(define *tileset-3* (alist-ref 'tileset-3 *tilesets*))
(define *tileset-4* (alist-ref 'tileset-4 *tilesets*))

(define *tile-spec-1* (tileset-add-tile-spec!
                       *tileset-1*
                       start: '(0 0)
                       dimensions: +window-dimensions+))
(define *tile-spec-2* (tileset-add-tile-spec!
                       *tileset-2*
                       start: '(0 0)
                       dimensions: +window-dimensions+))
(define *tile-spec-3* (tileset-add-tile-spec!
                       *tileset-3*
                       start: '(0 0)
                       dimensions: +window-dimensions+))
(define *tile-spec-4* (tileset-add-tile-spec!
                       *tileset-4*
                       start: '(0 0)
                       dimensions: +window-dimensions+))

(define *tile-1* (create-tile! *tile-spec-1*))
(check-gl-error "post-create-tile")
(define *tile-2* (create-tile! *tile-spec-2*))
(define *tile-3* (create-tile! *tile-spec-3*))
(define *tile-4* (create-tile! *tile-spec-4*))


(set! (tile-position *tile-1*) '(-200 -150))
(set! (tile-position *tile-2*) '(200 150))

(define *tiles* (list *tile-1* *tile-2* *tile-3* *tile-4*))

;end
