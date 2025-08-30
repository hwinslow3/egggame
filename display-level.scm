#!/bin/sh
#|
exec chicken-csi -I ./egggame/ -s "$0" "$@"
|#

(import (format)
        (scheme)
        (chicken process-context)
        (chicken port)
        (srfi-1)
        (srfi-4)
        (srfi-13)
        (srfi-69)
        (srfi-151)
        (alist-lib)
        (egggame sdl)
        (egggame sdlutil)
        (egggame glew)
        (egggame devil)
        (egggame tile-collection)
        (egggame tiled-file)
        (egggame glutil)
        (egggame matrix)
        (defstruct)
        (ssax)
        (filepath))

(define sdl-window (make-parameter #f))
(define gl-context (make-parameter #f))
(define fps-regulator (make-parameter #f))
(define tile-collection (make-parameter #f))

(defstruct config
  path-to-file
  window-dimensions)

(define (parse-cli-args args)
  (let iter ((args args)
             (res  (make-config)))
    (cond
     ((null? args)
      res)

     ((and (equal? (car args) "--window-dimensions")
           (>= (length args) 2))
      (let ((window-dimensions (with-input-from-string (cadr args) read)))
        (or (and (list? window-dimensions)
                 (= (length window-dimensions) 2)
                 (every number? window-dimensions)
                 (every positive? window-dimensions)
                 (iter (cddr args)
                       (update-config res window-dimensions: window-dimensions)))
            (error "bad window dimensions" (cadr args)))))
     ((equal? (car args) "--window-dimensions")
      (error "window-dimensions missing argument" args))

     (else
      (let* ((path-to-file (car args))
             (args         (cdr args))
             (res          (update-config res path-to-file: path-to-file)))
        (iter args res))))))

(define (load-tiled-file path coll)
  (let* ((sxml (with-input-from-file
                   path
                 (lambda () (ssax:xml->sxml (current-input-port) '()))))
         (tilesets
          (tile-collection-add-tilesets!
           (tile-collection)
           (map (lambda (ts-sxml)
                  `(,(tileset-name ts-sxml) .
                    ,(filepath:join-path
                      (list (filepath:take-directory path)
                            (tileset-source ts-sxml)))))
                (map-tilesets (sxml-toplevel-tag sxml)))))
         (tile-specs (alist->hash-table '())))

    (for-each
     (lambda (ts-sxml)
       (let ((tileset    (alist-ref tilesets (tileset-name ts-sxml)))
             (firstgid   (tileset-firstgid ts-sxml))
             (columns    (tileset-columns ts-sxml))
             (tilecount  (tileset-tilecount ts-sxml))
             (dimensions (tileset-tiledimensions ts-sxml))
             )
         (for-each
          (lambda (tile-idx)
            (let* ((gid      (+ firstgid tile-idx))
                   (row      (floor (/ tile-idx columns)))
                   (column   (modulo tile-idx columns)))
              (set! (hash-table-ref tile-specs (number->string gid))
                    (tileset-add-tile-spec! tileset
                     start:      (map * (list column row) dimensions)
                     dimensions: dimensions))))
          (iota tilecount))))
     (map-tilesets (sxml-toplevel-tag sxml)))

    (for-each
     (lambda (layer-sxml depth)
       (let ((data (layer-data layer-sxml)))
         (for-each
          (lambda (row row-idx)
            (for-each
             (lambda (gid column-idx)
               (unless (equal? gid "0")
                 (let* ((spec (hash-table-ref tile-specs gid))
                        (tile (create-tile! spec))

                        (xy-pos (map * (tile-dimensions tile)
                                       (list column-idx row-idx))))
                   (set! (tile-position tile)
                         (append xy-pos (list (* -1 depth)))))))
             row
             (iota (length row))))
          data
          (iota (length data)))))
     (map-layers/sorted (sxml-toplevel-tag sxml))
     (map-layers-depths (sxml-toplevel-tag sxml)))))

(define (with-game-params config func)
  (or (SDL_InitSubSystem SDL_INIT_VIDEO)
      (error (format "failed to init video: ~a" (SDL_GetError))))
  (ilInit)

  (parameterize ((sdl-window
                  (let ((dims (config-window-dimensions config)))
                    (or (SDL_CreateWindow "fucking around"
                                          (car dims) (cadr dims)
                                          (bitwise-ior SDL_WINDOW_OPENGL))
                        (error (format "failed to create window: ~a" (SDL_GetError)))))))
    (parameterize ((gl-context    (SDL_GL_CreateContext (sdl-window)))
                   (fps-regulator (make-fps-regulator 29)))
      (unless (>= (SDL_GL_GetAttribute SDL_GL_CONTEXT_MAJOR_VERSION) 2)
        (error "gl version too low"))

      (unless (= (glewInit) GLEW_OK)
        (error "failed to initialize glew"))

      (or (SDL_GL_SetSwapInterval 1)
          (error "failed to set swap interval" (SDL_GetError)))

      (glEnable GL_BLEND)
      (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

      (parameterize ((tile-collection
                      (make-tile-collection
                       screen-dimensions: (config-window-dimensions config))))
        (load-tiled-file (config-path-to-file config) (tile-collection))
        (func)))))

(define-syntax while
  (syntax-rules ()
    ((while test body . rest)
     (let iter ()
       (if test (begin (begin body . rest) (iter)))))))

(define main-loop
  (let ((dragging?  #f)
        (running?   #t)
        (camera-pos  '(0 0))
        (camera-dims #f)
        (ev         (make-SDL_Event)))

    (define (pump-events!)
      (and
       (SDL_PollEvent ev)
       (begin
         (cond
          ((= (SDL_Event-type ev) SDL_EVENT_QUIT)
           (set! running? #f)
           #t)
          ((and (= (SDL_Event-type ev) SDL_EVENT_MOUSE_BUTTON_DOWN)
                (= (SDL_Event-button-button ev) 1))
           (set! dragging? #t)
           #t)
          ((= (SDL_Event-type ev) SDL_EVENT_MOUSE_BUTTON_UP)
           (set! dragging? #f)
           #t)
          ((= (SDL_Event-type ev) SDL_EVENT_MOUSE_MOTION)
           (when dragging?
             (set! camera-pos
               (map + camera-pos
                    (map - (list (SDL_Event-motion-xrel ev)
                                 (SDL_Event-motion-yrel ev)))))
             (shift-2d-ortho-matrix!
              (tile-collection-camera-matrix (tile-collection))
              dimensions: camera-dims start: camera-pos)))
          (else #f))
         (pump-events!))))

    (lambda ()
      (set! camera-dims
        (s32vector->list (SDL_GetWindowSizeInPixels-s32vector (sdl-window))))
      (while running?
        (pump-events!)
        ((fps-regulator))
        (render-tile-collection! (tile-collection))
        (SDL_GL_SwapWindow (sdl-window))))))

(define (main args)
  (with-game-params
   (parse-cli-args args)
   main-loop))

(cond-expand
  (chicken-script
    (main (command-line-arguments)))
  (else))
