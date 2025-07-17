(import (srfi-151))
(import (egggame sdl)
        (egggame glew)
        (egggame devil)
        (egggame glutil)
        (srfi-4)
        (defstruct))
(import (chicken format) (chicken blob) (chicken port) (chicken memory))

(load "scrap-matrix.scm")

(define-syntax while
  (syntax-rules ()
    ((while test body . rest)
     (let iter ()
       (if test (begin (begin body . rest) (iter)))))))

(define (make-fps-regulator fps)
  (let ((last-time (SDL_GetTicks))
        (mspf (* 1000.0 (/ fps))))
    (lambda ()
      (let ((old-time last-time))
        (set! last-time (SDL_GetTicks))
        (let ((diff (- last-time old-time)))
          ;; ok, so, if diff is greater than a frame, print msg and skip
          ;; if diff is less than a frame, sleep the difference
          (if (< diff mspf)
              (SDL_Delay (inexact->exact (floor (- mspf diff))))
              (with-output-to-port
                  (current-error-port)
                (lambda ()
                  (printf "warning: overshot frame by ~s ms\n" (- diff mspf))))))))))

(define event-cycle!
  (let ((mouse-button-down? #f))
    (lambda ()
      (let ((res (SDL_PollEvent *ev*)))
        (and
         res
         (cond
          ((= (SDL_Event-type *ev*) SDL_EVENT_QUIT)
           (set! running? #f))
          ((= (SDL_Event-type *ev*) SDL_EVENT_MOUSE_BUTTON_DOWN)
           (set! mouse-button-down? #t))
          ((= (SDL_Event-type *ev*) SDL_EVENT_MOUSE_BUTTON_UP)
           (set! mouse-button-down? #f))
          ((= (SDL_Event-type *ev*) SDL_EVENT_MOUSE_MOTION)
           (when mouse-button-down?
             (let ((offset
                    (list (SDL_Event-motion-xrel *ev*)
                          (SDL_Event-motion-xrel *ev*)
                          0.5)))
               (set! (tile-position *tile-1*) (map + (tile-position *tile-1*) offset))
;               (set! (tile-position *tile-2*) (map + (tile-position *tile-2*) offset))
)))
          (else
           (printf "unrecognized event type: ~a\n" (SDL_Event-type *ev*)))))))))

;; main regulator
(define regulate! (make-fps-regulator 30))

(define (draw!)
  (render-tile-collection! *tile-coll*)
)

(define (main-loop)
  (while running?
    (while (event-cycle!)
      #t)
    (regulate!)
    (draw!)
    (SDL_GL_SwapWindow *window*)))

;; tile library stuff

(defstruct tile-collection
  gl-program
  gl-texture-array
  gl-texture-dimensions
  (tilesets '())
  vert-buffer-data
  vert-buffer
  texcoord-buffer-data
  texcoord-buffer
  texlayer-buffer-data
  texlayer-buffer
  element-buffer-data
  element-buffer
  camera-matrix
)

(set! tile-collection-gl-texture-dimensions
  (getter-with-setter
   tile-collection-gl-texture-dimensions
   tile-collection-gl-texture-dimensions-set!))

(set! tile-collection-gl-texture-array
  (getter-with-setter
   tile-collection-gl-texture-array
   tile-collection-gl-texture-array-set!))

(set! tile-collection-tilesets
  (getter-with-setter
   tile-collection-tilesets
   tile-collection-tilesets-set!))

(defstruct tileset
  tile-collection
  name
  dimensions
  layer
  (tile-specs '())
)

(set! tileset-tile-specs
  (getter-with-setter tileset-tile-specs tileset-tile-specs-set!))

(defstruct tile-spec
  tileset
  start
  dimensions
  (tiles '())
)

(set! tile-spec-tiles (getter-with-setter tile-spec-tiles tile-spec-tiles-set!))

(defstruct tile
  element-buffer-index
  array-buffer-start-index
  tile-spec
)

(define orig-make-tile-collection make-tile-collection)

(set! make-tile-collection
  (lambda (#!key screen-dimensions)
    (let* (
           ;; buffers
           (vert-buffer-data (f32vector 0 0 0
                                        0 0 0
                                        0 0 0
                                        0 0 0))
           (vert-buffer (make-buffer GL_ARRAY_BUFFER
                                     (f32vector->blob/shared vert-buffer-data)))
           (texcoord-buffer-data (f32vector 0 0
                                            0 0
                                            0 0
                                            0 0))
           (texcoord-buffer (make-buffer
                             GL_ARRAY_BUFFER
                             (f32vector->blob/shared texcoord-buffer-data)))
           (texlayer-buffer-data (s32vector 0 0 0 0))
           (texlayer-buffer (make-buffer
                             GL_ARRAY_BUFFER
                             (s32vector->blob/shared texlayer-buffer-data)))
           (element-buffer-data (u16vector 0 0 0
                                           0 0 0))
           (element-buffer (make-buffer
                            GL_ELEMENT_ARRAY_BUFFER
                            (u16vector->blob/shared element-buffer-data)))

           ;; shader
           (vert-shader (make-shader GL_VERTEX_SHADER #<<END
#version 130

in vec3 position;
in vec2 in_texcoord;
in int in_texlayer;

uniform mat4 camera;

out vec2 texcoord;
flat out int texlayer;

void main() {
  gl_Position = camera * vec4(position, 1.0);
  texcoord = in_texcoord;
  texlayer = in_texlayer;
}
END
))
           (frag-shader (make-shader GL_FRAGMENT_SHADER #<<END
#version 130

uniform sampler2DArray textures;

in vec2 texcoord;
flat in int texlayer;

void main()
{
    gl_FragColor = texture(textures, vec3(texcoord, texlayer));
}
END
))
           (gl-program (make-program vert-shader frag-shader))

           (camera-matrix (ortho-matrix left: 0
                                        top: 0
                                        bottom: (cadr screen-dimensions)
                                        right: (car screen-dimensions)
                                        near: 0
                                        far: 1))
           )
      (orig-make-tile-collection
       gl-program: gl-program
       gl-texture-array: #f
       tilesets: '()
       vert-buffer-data: vert-buffer-data
       vert-buffer: vert-buffer
       texcoord-buffer-data: texcoord-buffer-data
       texcoord-buffer: texcoord-buffer
       texlayer-buffer-data: texlayer-buffer-data
       texlayer-buffer: texlayer-buffer
       element-buffer-data: element-buffer-data
       element-buffer: element-buffer
       camera-matrix: camera-matrix))))

(define (check-gl-error msg)
  (let ((err (glGetError)))
    (unless (= err GL_NO_ERROR)
      (error msg (gluErrorString err)))))

(define (check-il-error msg)
  (let ((err (ilGetError)))
    (unless (= err IL_NO_ERROR)
      (error msg (iluErrorString err)))))

(define (dimensions-from-stored-image filename)
  (let ((img (let ((res (u32vector 3)))
               (ilGenImages 1 res)
               (check-il-error "failed to create devil image")
               (u32vector-ref res 0))))
    (ilBindImage img)
    (check-il-error "failed to bind devil image")

    (or (ilLoadImage filename)
        (check-il-error "failed to load devil image"))

    (or (ilConvertImage IL_RGBA IL_UNSIGNED_BYTE)
        (check-il-error "failed to convert devil image"))

    (let ((res (list (ilGetInteger IL_IMAGE_WIDTH) (ilGetInteger IL_IMAGE_HEIGHT))))
      (ilDeleteImages 1 (u32vector img))
      res)))

(define (tile-collection-add-tilesets! coll alist)
  ;; ok, so
  ;; * make sure texture array hasn't been created
  ;; * do a first pass over the alist and figure out the biggest dimension
  ;; * create a fully transparent base image
  ;; * do a second pass over the alist and load the layers
  ;;   - first load transparent
  ;;   - then load image
  (when (tile-collection-gl-texture-array coll)
    (error "tile collection tilesets already initialized"))
  (let* ((max-dimensions (append
                          (apply map max
                                 (map dimensions-from-stored-image
                                      (map cdr alist)))
                          (list (length alist))))
         (gl-tex
          (let ((names (u32vector 4)))
            (glGenTextures 1 names)
            (u32vector-ref names 0)))
         (initialized-storage? #f)
         (empty-image (make-u32vector (* (car max-dimensions) (cadr max-dimensions)) 0)))
    (glBindTexture GL_TEXTURE_2D_ARRAY gl-tex)
    (set! (tile-collection-gl-texture-dimensions coll) max-dimensions)
    (for-each
     (lambda (pair layer)
       (let ((name     (car pair))
             (filename (cdr pair))
             (img (let ((res (u32vector 3)))
                    (ilGenImages 1 res)
                    (check-il-error "failed to create devil image")
                    (u32vector-ref res 0))))
         (ilBindImage img)
         (check-il-error "failed to bind devil image")

         (or (ilLoadImage filename)
             (check-il-error "failed to load devil image"))

         (or (ilConvertImage IL_RGBA IL_UNSIGNED_BYTE)
             (check-il-error "failed to convert devil image"))

         (let ((dimensions (list (ilGetInteger IL_IMAGE_WIDTH) (ilGetInteger IL_IMAGE_HEIGHT))))

           (unless initialized-storage?
             (glTexStorage3D
              GL_TEXTURE_2D_ARRAY 1 GL_RGBA8
              (car max-dimensions) (cadr max-dimensions)
              (caddr max-dimensions))
             (check-gl-error "failed to initialize 3d gl texture storage")
             (set! initialized-storage? #t))

           (glTexSubImage3D/blob
            GL_TEXTURE_2D_ARRAY 0 0 0 layer
            (car max-dimensions) (cadr max-dimensions)
            1
            GL_RGBA GL_UNSIGNED_BYTE (u32vector->blob/shared empty-image))
           (check-gl-error "failed to assign 3d  gl sub texture")

           (glTexSubImage3D
            GL_TEXTURE_2D_ARRAY 0 0 0 layer
            (car dimensions) (cadr dimensions)
            1
            GL_RGBA GL_UNSIGNED_BYTE (ilGetData))
           (check-gl-error "failed to assign 3d  gl sub texture")

           (set! (tile-collection-tilesets coll)
             (append (tile-collection-tilesets coll)
                     (list (make-tileset
                            tile-collection: coll
                            name: name
                            dimensions: dimensions
                            layer: layer))))

           (ilDeleteImages 1 (u32vector img)))))
     alist
     (iota (length alist)))
    (set! (tile-collection-gl-texture-array coll) gl-tex)

    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)

    (map
     (lambda (ts) (cons (tileset-name ts) ts))
     (tile-collection-tilesets coll))))

(define (tileset-add-tile-spec! ts #!key start dimensions)
  (let ((res (make-tile-spec
              tileset: ts
              start: start
              dimensions: dimensions)))
    (set! (tileset-tile-specs ts) (append (tileset-tile-specs ts) (list res)))
    res))

(define (tile-collection-next-array-buffer-start-index coll)
  ;; a la next-element-buffer-index, we're looking for a zero sized set of verts
  ;; we'll consider it zero sized if the two first verts are both 0,0,0
  ;; although because this is array buffer data, and we're going through the
  ;; vert buffer data (which is three floats per entry), we'll divide by 3
  (let* ((buffer-data  (tile-collection-vert-buffer-data coll))
         (max-rect-idx (/ (f32vector-length buffer-data) (* 3 4))))
    (let iter ((rect-idx 0))
      (if (>= rect-idx max-rect-idx)
          #f
          (let ((idx (* rect-idx (* 3 4))))
            (if (and (zero? (f32vector-ref buffer-data (+ idx 0)))
                     (zero? (f32vector-ref buffer-data (+ idx 1)))
                     (zero? (f32vector-ref buffer-data (+ idx 2)))

                     (zero? (f32vector-ref buffer-data (+ idx 3)))
                     (zero? (f32vector-ref buffer-data (+ idx 4)))
                     (zero? (f32vector-ref buffer-data (+ idx 5))))
                (/ idx 3)
                (iter (add1 rect-idx))))))))

(define (tile-collection-next-element-buffer-index coll)
  ;; rect-idx is 1/6th of the element buffer idx
  ;; each rect is two triangles, so (0 1 2 1 3 2)
  ;; this function will return the actual index
  ;; we'll consider the rect-idx to be free if it's a zero sized set of triangles
  ;; so 0 0 should be a decent test
  (let* ((buffer-data  (tile-collection-element-buffer-data coll))
         (max-rect-idx (/ (u16vector-length buffer-data) 6)))
    (let iter ((rect-idx 0))
      (if (>= rect-idx max-rect-idx)
          #f
          (let ((idx (* rect-idx 6)))
            (if (and (zero? (u16vector-ref buffer-data idx))
                     (zero? (u16vector-ref buffer-data (add1 idx))))
                idx
                (iter (add1 rect-idx))))))))

(define (%tile-update-gl-array-buffer! tile #!key
                                       get-buffer-data
                                       get-buffer
                                       target
                                       buffer-data-to-blob
                                       sub-buffer-data
                                       ;; stupidly verbose, I know
                                       ;; but I get confused easily
                                       buffer-vect-element-count-per-vert
                                       vect-element-size-bytes
                                       )
  (let* ((spec (tile-tile-spec tile))
         (set  (tile-spec-tileset spec))
         (coll (tileset-tile-collection set)))

    (let ((buffer-data       (get-buffer-data coll))
          (array-buffer-idx  (tile-array-buffer-start-index tile))
          (verts-per-tile    4)
          )
      (glBindBuffer target (get-buffer coll))
      (glBufferSubData target
                       (* array-buffer-idx
                          buffer-vect-element-count-per-vert
                          vect-element-size-bytes)
                       (* buffer-vect-element-count-per-vert
                          vect-element-size-bytes
                          verts-per-tile)
                       (buffer-data-to-blob
                        (sub-buffer-data
                         buffer-data
                         (* array-buffer-idx
                            buffer-vect-element-count-per-vert)
                         (* (+ verts-per-tile array-buffer-idx)
                            buffer-vect-element-count-per-vert)))))))

(define (tile-update-gl-buffers!/position tile)
  (%tile-update-gl-array-buffer! tile
   get-buffer-data:     tile-collection-vert-buffer-data
   get-buffer:          tile-collection-vert-buffer
   target:              GL_ARRAY_BUFFER
   buffer-data-to-blob: f32vector->blob/shared
   sub-buffer-data:     subf32vector

   buffer-vect-element-count-per-vert: 3
   vect-element-size-bytes:            4))

(define (%tile-position tile)
  (let* ((spec (tile-tile-spec tile))
         (set  (tile-spec-tileset spec))
         (coll (tileset-tile-collection set)))
    (let ((buffer-data      (tile-collection-vert-buffer-data coll))
          (array-buffer-idx (tile-array-buffer-start-index tile)))
      (list (f32vector-ref buffer-data (+ (* array-buffer-idx 3) 0))
            (f32vector-ref buffer-data (+ (* array-buffer-idx 3) 1))
            (f32vector-ref buffer-data (+ (* array-buffer-idx 3) 2))))))

(define (tile-position-set! tile pos)
  (let* ((pos (if (= 2 (length pos))
                  (append pos (list (caddr (tile-position tile))))
                  pos))
         (spec (tile-tile-spec tile))
         (set  (tile-spec-tileset spec))
         (coll (tileset-tile-collection set)))

    (let ((buffer-data      (tile-collection-vert-buffer-data coll))
          (array-buffer-idx (tile-array-buffer-start-index tile)))

      (define (assign-vert! vert-idx-offset value)
        (for-each
         (lambda (value value-offset)
           (set! (f32vector-ref buffer-data
                                (+ (* (+ array-buffer-idx vert-idx-offset) 3)
                                   value-offset))
             value))
         value
         '(0 1 2)))

      (let* ((left   (car pos))
             (right  (+ (car (tile-spec-dimensions spec)) (car pos)))
             (top    (cadr pos))
             (bottom (+ (cadr (tile-spec-dimensions spec)) (cadr pos)))
             (depth  (caddr pos))
             )
        (assign-vert! 0 (list left top depth))
        (assign-vert! 1 (list right top depth))
        (assign-vert! 2 (list right bottom depth))
        (assign-vert! 3 (list left bottom depth))))

    (tile-update-gl-buffers!/position tile)))

(define tile-position (getter-with-setter %tile-position tile-position-set!))

(define tile-position/xy
  (getter-with-setter
   (lambda (tile) (take (tile-position tile) 2))
   tile-position-set!))

(define (tile-update-gl-buffers! tile)
  (let* ((spec (tile-tile-spec tile))
         (set  (tile-spec-tileset spec))
         (coll (tileset-tile-collection set)))

    ;; element buffer
    (let ((buffer-data (tile-collection-element-buffer-data coll))
          (elem-idx    (tile-element-buffer-index tile)))
      (glBindBuffer GL_ELEMENT_ARRAY_BUFFER (tile-collection-element-buffer coll))
      (glBufferSubData GL_ELEMENT_ARRAY_BUFFER (* 2 elem-idx) (* 6 2)
                       (u16vector->blob/shared
                        (subu16vector buffer-data elem-idx (+ elem-idx 6))))))

  ;; position buffer
  (tile-update-gl-buffers!/position tile)

  ;; texcoord buffer
  (%tile-update-gl-array-buffer! tile
   get-buffer-data:     tile-collection-texcoord-buffer-data
   get-buffer:          tile-collection-texcoord-buffer
   target:              GL_ARRAY_BUFFER
   buffer-data-to-blob: f32vector->blob/shared
   sub-buffer-data:     subf32vector

   buffer-vect-element-count-per-vert: 2
   vect-element-size-bytes:            4)


  ;; texlayer buffer
  (%tile-update-gl-array-buffer! tile
   get-buffer-data:     tile-collection-texlayer-buffer-data
   get-buffer:          tile-collection-texlayer-buffer
   target:              GL_ARRAY_BUFFER
   buffer-data-to-blob: s32vector->blob/shared
   sub-buffer-data:     subs32vector

   buffer-vect-element-count-per-vert: 1
   vect-element-size-bytes:            4))

(define (tile-assign-indices! tile)
  (let* ((spec (tile-tile-spec tile))
         (set  (tile-spec-tileset spec))
         (coll (tileset-tile-collection set)))

    ;; element buffer
    (let ((buffer-data      (tile-collection-element-buffer-data coll))
          (elem-idx         (tile-element-buffer-index tile))
          (array-buffer-idx (tile-array-buffer-start-index tile))
          )
      (set! (u16vector-ref buffer-data (+ elem-idx 0)) (+ array-buffer-idx 0))
      (set! (u16vector-ref buffer-data (+ elem-idx 1)) (+ array-buffer-idx 1))
      (set! (u16vector-ref buffer-data (+ elem-idx 2)) (+ array-buffer-idx 2))

      (set! (u16vector-ref buffer-data (+ elem-idx 3)) (+ array-buffer-idx 3))
      (set! (u16vector-ref buffer-data (+ elem-idx 4)) (+ array-buffer-idx 0))
      (set! (u16vector-ref buffer-data (+ elem-idx 5)) (+ array-buffer-idx 2))
      )

    ;; position vertex buffer
    (let ((buffer-data      (tile-collection-vert-buffer-data coll))
          (array-buffer-idx (tile-array-buffer-start-index tile)))

      (define (assign-vert! vert-idx-offset value)
        (for-each
         (lambda (value value-offset)
           (set! (f32vector-ref buffer-data
                                (+ (* (+ array-buffer-idx vert-idx-offset) 3)
                                   value-offset))
             value))
         value
         '(0 1 2)))

      (let* ((left   0)
             (right  (car (tile-spec-dimensions spec)))
             (top    0)
             (bottom (cadr (tile-spec-dimensions spec)))
             (depth  -0.5)
             )
        (assign-vert! 0 (list left top depth))
        (assign-vert! 1 (list right top depth))
        (assign-vert! 2 (list right bottom depth))
        (assign-vert! 3 (list left bottom depth))))

    ;; texcoord buffer
    (let ((buffer-data      (tile-collection-texcoord-buffer-data coll))
          (array-buffer-idx (tile-array-buffer-start-index tile))
          (max-dims         (take (tile-collection-gl-texture-dimensions coll) 2)))

      (define (assign-texcoord! texcoord-idx-offset value)
        (for-each
         (lambda (value value-offset)
           (set! (f32vector-ref buffer-data
                                (+ (* (+ array-buffer-idx texcoord-idx-offset) 2)
                                   value-offset))
             value))
         (map / value max-dims)
         '(0 1)))

      (let* ((left   (car (tile-spec-start spec)))
             (right  (+ (car (tile-spec-start spec))
                        (car (tile-spec-dimensions spec))))
             (top    (cadr (tile-spec-start spec)))
             (bottom (+ (cadr (tile-spec-start spec))
                        (cadr (tile-spec-dimensions spec)))))
        (assign-texcoord! 0 (list left top))
        (assign-texcoord! 1 (list right top))
        (assign-texcoord! 2 (list right bottom))
        (assign-texcoord! 3 (list left bottom))))


    ;; texlayer
    (let ((buffer-data      (tile-collection-texlayer-buffer-data coll))
          (array-buffer-idx (tile-array-buffer-start-index tile))
          (layer            (tileset-layer set)))

      (for-each
       (lambda (idx-offset)
         (set! (s32vector-ref buffer-data (+ array-buffer-idx idx-offset)) layer))
       '(0 1 2 3)))

    (tile-update-gl-buffers! tile)))

(define (delete-tile! tile)
  (let* ((spec (tile-tile-spec tile))
         (set  (tile-spec-tileset spec))
         (coll (tileset-tile-collection set)))
    ;; blank element buffer section
    (let ((buffer-data      (tile-collection-element-buffer-data coll))
          (elem-idx         (tile-element-buffer-index tile))
          )
      (set! (u16vector-ref buffer-data (+ elem-idx 0)) 0)
      (set! (u16vector-ref buffer-data (+ elem-idx 1)) 0)
      (set! (u16vector-ref buffer-data (+ elem-idx 2)) 0)

      (set! (u16vector-ref buffer-data (+ elem-idx 3)) 0)
      (set! (u16vector-ref buffer-data (+ elem-idx 4)) 0)
      (set! (u16vector-ref buffer-data (+ elem-idx 5)) 0))

    ;; blank first two verts
    (let* ((buffer-data      (tile-collection-vert-buffer-data coll))
           (array-buffer-idx (tile-array-buffer-start-index tile))
           (idx              (* array-buffer-idx 3)))
      (set! (f32vector-ref buffer-data (+ idx 0)) 0)
      (set! (f32vector-ref buffer-data (+ idx 1)) 0)
      (set! (f32vector-ref buffer-data (+ idx 2)) 0)

      (set! (f32vector-ref buffer-data (+ idx 3)) 0)
      (set! (f32vector-ref buffer-data (+ idx 4)) 0)
      (set! (f32vector-ref buffer-data (+ idx 5)) 0)))

  (tile-update-gl-buffers! tile))

(define (tile-collection-double! coll)

  (define (double-buffer! #!key
           get-buffer-data
           set-buffer-data
           get-buffer
           make-new-buffer-data
           buffer-data-length
           buffer-data-to-blob
           target
           (usage GL_DYNAMIC_DRAW))
    (let* ((old (get-buffer-data coll))
           (new (make-new-buffer-data (* (buffer-data-length old) 2) 0)))
      (move-memory! old new)
      (set-buffer-data coll new)
      (glBindBuffer target (get-buffer coll))
      (let ((blob (buffer-data-to-blob new)))
        (glBufferData target (blob-size blob) blob usage))))

  (double-buffer!
   get-buffer-data:      tile-collection-vert-buffer-data
   set-buffer-data:      tile-collection-vert-buffer-data-set!
   get-buffer:           tile-collection-vert-buffer
   make-new-buffer-data: make-f32vector
   buffer-data-length:   f32vector-length
   buffer-data-to-blob:  f32vector->blob/shared
   target:               GL_ARRAY_BUFFER)
  (double-buffer!
   get-buffer-data:      tile-collection-texcoord-buffer-data
   set-buffer-data:      tile-collection-texcoord-buffer-data-set!
   get-buffer:           tile-collection-texcoord-buffer
   make-new-buffer-data: make-f32vector
   buffer-data-length:   f32vector-length
   buffer-data-to-blob:  f32vector->blob/shared
   target:               GL_ARRAY_BUFFER)
  (double-buffer!
   get-buffer-data:      tile-collection-texlayer-buffer-data
   set-buffer-data:      tile-collection-texlayer-buffer-data-set!
   get-buffer:           tile-collection-texlayer-buffer
   make-new-buffer-data: make-s32vector
   buffer-data-length:   s32vector-length
   buffer-data-to-blob:  s32vector->blob/shared
   target:               GL_ARRAY_BUFFER)
  (double-buffer!
   get-buffer-data:      tile-collection-element-buffer-data
   set-buffer-data:      tile-collection-element-buffer-data-set!
   get-buffer:           tile-collection-element-buffer
   make-new-buffer-data: make-u16vector
   buffer-data-length:   u16vector-length
   buffer-data-to-blob:  u16vector->blob/shared
   target:               GL_ELEMENT_ARRAY_BUFFER)
)

(define (create-tile! tile-spec)
  (let* ((set      (tile-spec-tileset tile-spec))
         (coll     (tileset-tile-collection set))
         (next-idx (tile-collection-next-element-buffer-index coll)))
    (if (not next-idx)
        (begin
          (tile-collection-double! coll)
          (create-tile! tile-spec))
        (let* ((array-idx (tile-collection-next-array-buffer-start-index coll))
               (res       (make-tile
                           element-buffer-index: next-idx
                           array-buffer-start-index: array-idx
                           tile-spec: tile-spec)))
          (set! (tile-spec-tiles tile-spec)
                (append (tile-spec-tiles tile-spec) (list res)))
          (tile-assign-indices! res)
          res))))

(define (render-tile-collection! coll)
  (let* ((program (tile-collection-gl-program coll))

         (a-position (glGetAttribLocation program "position"))
         (a-texcoord (glGetAttribLocation program "in_texcoord"))
         (a-texlayer (glGetAttribLocation program "in_texlayer")))

    (glUseProgram program)

    (glUniformMatrix4fv
     (glGetUniformLocation program "camera")
     1 #t (matrix-data (tile-collection-camera-matrix coll)))

    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D_ARRAY (tile-collection-gl-texture-array coll))
    (glUniform1i (glGetUniformLocation program "textures") 0)

    (glBindBuffer GL_ARRAY_BUFFER (tile-collection-vert-buffer coll))
    (glVertexAttribPointer a-position 3 GL_FLOAT GL_FALSE (* 3 sizeof-GLfloat) #f)
    (glEnableVertexAttribArray a-position)

    (glBindBuffer GL_ARRAY_BUFFER (tile-collection-texcoord-buffer coll))
    (glVertexAttribPointer a-texcoord 2 GL_FLOAT GL_FALSE (* 2 sizeof-GLfloat) #f)
    (glEnableVertexAttribArray a-texcoord)

    (glBindBuffer GL_ARRAY_BUFFER (tile-collection-texlayer-buffer coll))
    (glVertexAttribIPointer a-texlayer 1 GL_INT (* 1 sizeof-GLint) #f)
    (glEnableVertexAttribArray a-texlayer)

    (glBindBuffer GL_ELEMENT_ARRAY_BUFFER (tile-collection-element-buffer coll))
    (glDrawElements
     GL_TRIANGLES
     (u16vector-length (tile-collection-element-buffer-data coll))
     GL_UNSIGNED_SHORT
     #f)

    (for-each
     glDisableVertexAttribArray
     (list a-position a-texcoord a-texlayer))))
