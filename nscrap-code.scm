(import (srfi-151))
(import (egggame sdl)
        (egggame glew)
        (egggame devil)
        (egggame glutil)
        (srfi-4)
        (defstruct))
(import (chicken format) (chicken blob) (chicken port) (chicken memory) (chicken condition))

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
  ;; ok dragging tiles
  ;; so if the mouse button goes down, test to see if it's on a tile
  ;; if it's on a tile, set that to the dragging tile
  ;; if the mouse moves, move the dragging tile, if it exists
  (let ((dragging-tile #f))
    (lambda ()
      (and-let* ((_ (SDL_PollEvent *ev*)))
        (cond
         ((= (SDL_Event-type *ev*) SDL_EVENT_QUIT)
          (set! running? #f))
         ((= (SDL_Event-type *ev*) SDL_EVENT_MOUSE_BUTTON_DOWN)
          (when (= (SDL_Event-button-button *ev*) 1)
            (let ((click-pos
                   (list (SDL_Event-button-x *ev*)
                         (SDL_Event-button-y *ev*))))
              (for-each
               (lambda (tile)
                 (when (and (every >= click-pos (tile-position/xy tile))
                            (every < click-pos (tile-bottom-right/xy tile)))
                   (set! dragging-tile tile)))
               *tiles*))))
          ((= (SDL_Event-type *ev*) SDL_EVENT_MOUSE_BUTTON_UP)
           (set! dragging-tile #f))
          ((= (SDL_Event-type *ev*) SDL_EVENT_MOUSE_MOTION)
           (when dragging-tile
             (set! (tile-position/xy dragging-tile)
                   (map + (tile-position/xy dragging-tile)
                          (list (SDL_Event-motion-xrel *ev*)
                                (SDL_Event-motion-yrel *ev*))))))
          ((= (SDL_Event-type *ev*) SDL_EVENT_KEY_UP)
           (when (and (member (SDL_Event-key-key *ev*) (list SDLK_UP SDLK_DOWN))
                      dragging-tile)
             (set! (tile-depth dragging-tile)
                   (+ (tile-depth dragging-tile)
                      (if (= (SDL_Event-key-key *ev*) SDLK_UP)
                          0.1
                          -0.1)))
             (format #t "debugk01 ~s\n" (tile-depth dragging-tile))))
          (else
;           (format #t "debugk03 ~s\n" (SDL_Event-type *ev*))
           #f
)
)
        #t
)
)))

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

  tile-vector
  ;; allocated tiles stored by element-buffer-index/6

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

(set! tile-collection-tile-vector
  (getter-with-setter
   tile-collection-tile-vector
   tile-collection-tile-vector-set!))

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
  ;; first index of the first item in the element buffer array
  ;; each tile consists of two triangles
  ;; so the items in the element-buffer consist of six indices into
  ;; vert-buffer+texcoord-buffer+texlayer-buffer
  ;; so element-buffer-index will increment by 6 each time
  ;; so they'll be something like 0, 6, 12, 18, 24, etc
  ;; to calculate byte offsets, multiply by 2 (each index is a u16)
  ;; also the tiles are stored in tile-vector
  ;; to calculate the tile-vector index for the tile, divide
  ;; element-buffer-index by six

  array-buffer-start-index
  ;; the vert-buffer+texcoord-buffer+texlayer-buffer correspond
  ;; because each tile is two triangles, but the two triangles share two verts
  ;; so there's four elements
  ;; we use a single index to cover vert+texcoord+texlayer, so one needs to
  ;; multiply by the number of elements in each vert for each buffer to figure
  ;; out the actual numeric vector index
  ;; (so array-buffer-start-index*3 works for vert, xyz per each vert; or *2 for
  ;;  texcoord, uv for each vert; and *1 for texlayer)
  ;; the indices increment by four (two triangles share two verts, and have two
  ;;  spare verts), so this increments something like 0, 4, 8, 12, 16, etc

  tile-spec
)

(set! tile-element-buffer-index
  (getter-with-setter tile-element-buffer-index tile-element-buffer-index-set!))

(define (tile-coll-tile-vector-index tile)
  (/ (tile-element-buffer-index tile) 6))

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
           (tile-vector (vector #f))

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
                                        ;; flipping far because of:
                                        ;; https://community.khronos.org/t/z-axis-mirror-reflection-effect-of-orthographic-projection-matrix-and-handedness-switch/105160
                                        far: -1))
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
       tile-vector: tile-vector
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

(define (vector-shift-sort-item! vect idx #!key
                                 length
                                 is-empty?
                                 less-than-or-equal?
                                 equal?
                                 swap!
                                 )
  (define (seek-next-non-empty advance-idx idx)
    (if (or (negative? idx) (>= idx length))
        #f
        (if (is-empty? vect idx)
            (let ((next (advance-idx idx)))
              (cond
               ((or (negative? next) (>= next length)) #f)
               ((not (is-empty? vect next))            next)
               (else
                (seek-next-non-empty advance-idx next))))
            idx)))
  (define (seek-non-empty-left idx)
    (seek-next-non-empty
     (lambda (idx) (sub1 idx))
     (sub1 idx)))
  (define (seek-non-empty-right idx)
    (seek-next-non-empty
     (lambda (idx) (add1 idx))
     (add1 idx)))
  (define (equal-seeker seek-non-empty)
    (lambda (idx)
      (let ((next (seek-non-empty idx)))
        (if (and next (equal? vect idx next))
            ((equal-seeker seek-non-empty) next)
            idx))))
  (define seek-equal-left (equal-seeker seek-non-empty-left))
  (define seek-equal-right (equal-seeker seek-non-empty-right))

  (let ((left-idx  (seek-non-empty-left idx))
        (right-idx (seek-non-empty-right idx)))
    (cond
     ((and (not left-idx) (not right-idx))
      #f)

     ;; no item to the left, return if we're less than or equal to our right
     ((and (not left-idx) right-idx (less-than-or-equal? vect idx right-idx))
      #f)
     ;; no item to the right, return if we're greater than or equal to our left
     ((and (not right-idx) left-idx (less-than-or-equal? vect left-idx idx))
      #f)

     ;; items to both sides and everything in order
     ((and left-idx right-idx
           (less-than-or-equal? vect left-idx idx)
           (less-than-or-equal? vect idx right-idx))
      #f)

     ;; an item to the left, we're less than it, so look for a string of equals and swap with the leftmost equal
     ((and left-idx (less-than-or-equal? vect idx left-idx))
      (let ((left-idx (seek-equal-left left-idx)))
        (swap! vect left-idx idx)
        (vector-shift-sort-item!
         vect left-idx
         length:              length
         is-empty?:           is-empty?
         less-than-or-equal?: less-than-or-equal?
         equal?:              equal?
         swap!:               swap!)))
     ;; an item to the left, we're less than it, so look for a string of equals and swap with the leftmost equal
     ((and right-idx (less-than-or-equal? vect right-idx idx))
      (let ((right-idx (seek-equal-right right-idx)))
        (swap! vect right-idx idx)
        (vector-shift-sort-item!
         vect right-idx
         length:              length
         is-empty?:           is-empty?
         less-than-or-equal?: less-than-or-equal?
         equal?:              equal?
         swap!:               swap!)))

     (else
      #f))))

(define (%tile-sort-depths! tile)
  (let* ((spec (tile-tile-spec tile))
         (set  (tile-spec-tileset spec))
         (coll (tileset-tile-collection set))

         (tile-vector (tile-collection-tile-vector coll)))
    (define (is-empty? _coll idx)
      (not (vector-ref tile-vector idx)))
    (define (less-than-or-equal? _coll a-idx b-idx)
      (<= (tile-depth (vector-ref tile-vector a-idx))
          (tile-depth (vector-ref tile-vector b-idx))))
    (define (equal? _coll a-idx b-idx)
      (= (tile-depth (vector-ref tile-vector a-idx))
         (tile-depth (vector-ref tile-vector b-idx))))
    (define swap!
      (let ((temp-elem-buffer (u16vector 0 0 0 0 0 0))
            (temp-buffer-size (* 2 6))

            (elem-buffer (tile-collection-element-buffer-data coll))
            )
        (lambda (_coll a-idx b-idx)
          (let* ((a             (vector-ref tile-vector a-idx))
                 (a-element-idx (tile-element-buffer-index a))

                 (b             (vector-ref tile-vector b-idx))
                 (b-element-idx (tile-element-buffer-index b)))
            ;; ok so, swap the element-buffer-data for the two items
            ;; move a    -> temp
            ;; move b    -> a
            ;; move temp -> b
            (move-memory! elem-buffer temp-elem-buffer
                          temp-buffer-size
                          (* 2 a-element-idx))
            (move-memory! elem-buffer elem-buffer
                          temp-buffer-size
                          (* 2 b-element-idx)
                          (* 2 a-element-idx))
            (move-memory! temp-elem-buffer elem-buffer
                          temp-buffer-size
                          0
                          (* 2 b-element-idx))

            ;; then swap their element buffer index
            (set! (tile-element-buffer-index a) b-element-idx)
            (set! (tile-element-buffer-index b) a-element-idx)

            ;; then swap their places in the tile vector
            (set! (vector-ref tile-vector a-idx) b)
            (set! (vector-ref tile-vector b-idx) a)

            ;; and then finally, write out the gl buffer changes
            (tile-update-gl-buffers!/element a)
            (tile-update-gl-buffers!/element b)))))
    (vector-shift-sort-item! coll (tile-coll-tile-vector-index tile)
     length:              (vector-length tile-vector)
     is-empty?:           is-empty?
     less-than-or-equal?: less-than-or-equal?
     equal?:              equal?
     swap!:               swap!)))

(define (tile-position-set! tile pos)
  (let* ((old-depth (tile-depth tile))
         (pos (if (= 2 (length pos))
                  (append pos (list old-depth))
                  pos))
         (depth-changed?
          (and (= 3 (length pos))
               (not (= (caddr pos) old-depth))))
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

    (tile-update-gl-buffers!/position tile)

    (%tile-sort-depths! tile)))

(define tile-position (getter-with-setter %tile-position tile-position-set!))

(define tile-position/xy
  (getter-with-setter
   (lambda (tile) (take (tile-position tile) 2))
   tile-position-set!))

(define (%tile-depth tile)
  (list-ref (tile-position tile) 2))

(define (tile-depth-set! tile depth)
  (set! (tile-position tile) (append (tile-position/xy tile) (list depth))))

(define tile-depth (getter-with-setter %tile-depth tile-depth-set!))

(define (tile-bottom-right tile)
  (let* ((spec (tile-tile-spec tile))
         (set  (tile-spec-tileset spec))
         (coll (tileset-tile-collection set)))
    (let* ((buffer-data      (tile-collection-vert-buffer-data coll))
           (array-buffer-idx (+ (tile-array-buffer-start-index tile) 2)))
      (list (f32vector-ref buffer-data (+ (* array-buffer-idx 3) 0))
            (f32vector-ref buffer-data (+ (* array-buffer-idx 3) 1))
            (f32vector-ref buffer-data (+ (* array-buffer-idx 3) 2))))))

(define (tile-bottom-right/xy tile)
  (take (tile-bottom-right tile) 2))

(define (tile-dimensions tile)
  (map - (tile-bottom-right tile) (tile-position tile)))

(define (tile-update-gl-buffers!/element tile)
  (let* ((spec (tile-tile-spec tile))
         (set  (tile-spec-tileset spec))
         (coll (tileset-tile-collection set)))

    ;; element buffer
    (let ((buffer-data (tile-collection-element-buffer-data coll))
          (elem-idx    (tile-element-buffer-index tile)))
      (glBindBuffer GL_ELEMENT_ARRAY_BUFFER (tile-collection-element-buffer coll))
      (glBufferSubData GL_ELEMENT_ARRAY_BUFFER (* 2 elem-idx) (* 6 2)
                       (u16vector->blob/shared
                        (subu16vector buffer-data elem-idx (+ elem-idx 6)))))))

(define (tile-update-gl-buffers! tile)
  ;; element buffer
  (tile-update-gl-buffers!/element tile)

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

(define (tile-write-initial-buffers! tile)
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
                 (->inexact value)))
         value
         '(0 1 2)))

      (let* ((left   0)
             (right  (car (tile-spec-dimensions spec)))
             (top    0)
             (bottom (cadr (tile-spec-dimensions spec)))
             (depth  0.5)
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
                 (->inexact value)))
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
      (set! (f32vector-ref buffer-data (+ idx 5)) 0))

    (tile-update-gl-buffers! tile)

    (set! (vector-ref (tile-collection-tile-vector coll)
                      (tile-coll-tile-vector-index tile))
          #f)))

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


  (let ((vect (tile-collection-tile-vector coll)))
    (set! (tile-collection-tile-vector coll)
          (list->vector
           (append (vector->list vect)
                   (make-list (vector-length vect) #f)))))
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
          ;; debug do we still need tile-spec to have the tiles list?
          (set! (tile-spec-tiles tile-spec)
                (append (tile-spec-tiles tile-spec) (list res)))
          (tile-write-initial-buffers! res)
          (set! (vector-ref (tile-collection-tile-vector coll)
                            (tile-coll-tile-vector-index res))
                res)
          (%tile-sort-depths! res)
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

;; testing

;; syntax
;; (with-wiped-funcs (func ...) body . rest)
;; =>
;; (let ((orig-func func) ...)
;;   (dynamic-wind
;;    (lambda ()
;;      (set! func (lambda args #f))
;;      ...
;;    )
;;    (lambda () body . rest)
;;    (lambda ()
;;      (set! func orig-func)
;;      ...
;;    )
;;   )
;; )

(define-syntax with-wiped-funcs
  (er-macro-transformer
   (lambda (exp rename compare)
     (import (srfi-1)
             (srfi-2)
             (srfi-13)
             (chicken string)
             (chicken foreign)
             (chicken format))
     (or (and-let* ((_ (and (list? exp)
                            (>= (length exp) 3)))

                    (symbol-append
                     (lambda items
                       (string->symbol
                        (string-concatenate (map symbol->string items)))))

                    (_ (car exp))

                    (funcs-to-wipe (and (list? (cadr exp))
                                        (every symbol? (cadr exp))
                                        (cadr exp)))

                    (body-list (cddr exp))

                    (&let          (rename 'let))
                    (&dynamic-wind (rename 'dynamic-wind))
                    (&lambda       (rename 'lambda))
                    (&set!         (rename 'set!))
                    )
           `(,&let ,(map (lambda (func)
                           `(,(symbol-append 'orig- func) ,func))
                         funcs-to-wipe)
              (,&dynamic-wind
                (,&lambda ()
                  #f
                  ,@(map (lambda (func)
                           `(,&set! ,func (,&lambda _ #f)))
                         funcs-to-wipe))
                (,&lambda () #f . ,body-list)
                (,&lambda ()
                  #f
                  ,@(map (lambda (func)
                           `(,&set! ,func ,(symbol-append 'orig- func)))
                         funcs-to-wipe)))))
         (error "bad with-wiped-funcs syntax" exp)))))

(define (run-tile-collection-tests)
  (let ((tests '()))

    (define (add-test! #!key name func)
      (set! tests (append tests (list (cons name func)))))

    ;; tests
    (add-test!
     name: 'tile-collection-double!
     func:
       (lambda ()
         ;; ok so
         ;; let's have a tile-collection with 2 rects worth
         ;; only one allocated though
         (let* ((tile-vector
                 (vector
                  (make-tile
                   element-buffer-index:     0
                   array-buffer-start-index: 4)))
                (coll
                 (orig-make-tile-collection
                  vert-buffer-data:
                    (f32vector 0 0 0
                               0 0 0
                               1 2 3
                               4 5 6

                               7 8 9
                               1 2 3
                               4 5 6
                               7 8 9)
                  texcoord-buffer-data:
                    (f32vector 1 2
                               3 4
                               5 6
                               7 8

                               9 1
                               2 3
                               4 5
                               6 7)
                  texlayer-buffer-data:
                    (s32vector 1 2 3 4

                               5 6 7 8)
                  element-buffer-data:
                    (u16vector 4 5 6
                               6 7 4

                               0 0 0
                               0 0 0)
                  tile-vector: tile-vector
                  )))
           (with-wiped-funcs (glBindBuffer glBufferData)
            (tile-collection-double! coll)
            (or (equal? (f32vector
                         0 0 0
                         0 0 0
                         1 2 3
                         4 5 6

                         7 8 9
                         1 2 3
                         4 5 6
                         7 8 9

                         0 0 0
                         0 0 0
                         0 0 0
                         0 0 0

                         0 0 0
                         0 0 0
                         0 0 0
                         0 0 0)
                        (tile-collection-vert-buffer-data coll))
                (error "tile-collection-double! failed to double vert buffer data"))
            (or (equal? (f32vector
                         1 2
                         3 4
                         5 6
                         7 8

                         9 1
                         2 3
                         4 5
                         6 7

                         0 0
                         0 0
                         0 0
                         0 0

                         0 0
                         0 0
                         0 0
                         0 0)
                        (tile-collection-texcoord-buffer-data coll))
                (error "tile-collection-double! failed to double texcoord buffer data"))
            (or (equal? (s32vector
                         1 2 3 4
                         5 6 7 8
                         0 0 0 0
                         0 0 0 0)
                        (tile-collection-texlayer-buffer-data coll))
                (error "tile-collection-double! failed to double texlayer buffer data"))
            (or (equal? (u16vector
                         4 5 6
                         6 7 4

                         0 0 0
                         0 0 0

                         0 0 0
                         0 0 0

                         0 0 0
                         0 0 0)
                        (tile-collection-element-buffer-data coll))
                (error "tile-collection-double! failed to double element buffer data"))
            (let ((vect (tile-collection-tile-vector coll)))
              (for-each
               (lambda (original stored idx)
                 (or (eq? original stored)
                     (error "tile-collection-double! failed to properly duplicate tile vector"))
                 (when (>= idx (vector-length vect))
                   (or (not stored)
                       (error "tile-collection-double! somehow added a non-empty cell after doubling collection" idx stored))))
               (append (vector->list tile-vector)
                       (make-list
                        (vector-length tile-vector)
                        #f))
               (append (vector->list vect)
                       (make-list
                        (vector-length vect)
                        #f))
               (iota (* 2 (vector-length vect)))))))))

    (add-test!
     name: 'create-tile!/maintain-tile-vect
     func:
       (lambda ()
         (let* ((tile-vector
                 (vector
                  (make-tile
                   element-buffer-index:     0
                   array-buffer-start-index: 4)
                  #f))
                (coll
                 (orig-make-tile-collection
                  vert-buffer-data:
                    (f32vector 0 0 0
                               0 0 0
                               1 2 0
                               4 5 0

                               7 8 0
                               1 2 0
                               4 5 0
                               7 8 0)
                  texcoord-buffer-data:
                    (f32vector 1 2
                               3 4
                               5 6
                               7 8

                               9 1
                               2 3
                               4 5
                               6 7)
                  texlayer-buffer-data:
                    (s32vector 1 2 3 4

                               3 3 3 3)
                  element-buffer-data:
                    (u16vector 4 5 6
                               6 7 4

                               0 0 0
                               0 0 0)
                  tile-vector: tile-vector
                  gl-texture-dimensions: '(100 100 5)))
                (set  (make-tileset tile-collection: coll
                                    layer: 3))
                (spec (make-tile-spec tileset: set
                                      start: '(0 0)
                                      dimensions: '(2 3)
                                      layer: 3)))
           (tile-tile-spec-set! (vector-ref tile-vector 0) spec)
           (with-wiped-funcs (glBindBuffer glBufferData
                                           glBufferSubData)
            (let ((next (create-tile! spec)))
              (or (= (tile-element-buffer-index next) 6)
                  (error "create-tile! got unexpected element buffer index"
                         (tile->alist next)))
              (or (= (tile-array-buffer-start-index next) 0)
                  (error "create-tile! got unexpected array buffer start index"
                         (tile->alist next)))
              (or (eq? (vector-ref (tile-collection-tile-vector coll) 1) next)
                  (error "create-tile! didn't update the tile vector")))))))

    (let ()
      ;; for our vector tests, each item is (num key)
      ;; and we're sorting lowest to highest, left to right
      (define (make-item #!key value key) (list value key))
      (define (item-value item) (car item))

      (define (add-vect-shift-test! #!key name-suffix in-vect out-vect idx)
        (add-test!
         name: (symbol-append 'vector-shift-sort-item!/ name-suffix)
         func:
           (lambda ()
             (vector-shift-sort-item!
              in-vect idx
              length: (vector-length in-vect)
              is-empty?: (lambda (vect idx)
                           (not (and (vector-ref vect idx) #t)))
              less-than-or-equal?:
                (lambda (vect a b)
                  (<= (item-value (vector-ref vect a))
                      (item-value (vector-ref vect b))))
              equal?:
                (lambda (vect a b)
                  (= (item-value (vector-ref vect a))
                     (item-value (vector-ref vect b))))
              swap!:
                (lambda (vect a b)
                  (let ((a-val (vector-ref vect a)))
                    (set! (vector-ref vect a) (vector-ref vect b))
                    (set! (vector-ref vect b) a-val))))
             (or (equal? in-vect out-vect)
                 (error "vector-shift-sort-item! didn't work"
                        name-suffix)))))

      (add-vect-shift-test!
       name-suffix: 'left-min
       idx: 0
       in-vect:
         (vector
          (make-item value: 0.5 key: 'a)
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.7 key: 'c)
          (make-item value: 0.8 key: 'd))
       out-vect:
         (vector
          (make-item value: 0.5 key: 'a)
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.7 key: 'c)
          (make-item value: 0.8 key: 'd)))

      (add-vect-shift-test!
       name-suffix: 'right-max
       idx: 3
       in-vect:
         (vector
          (make-item value: 0.5 key: 'a)
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.7 key: 'c)
          (make-item value: 0.8 key: 'd))
       out-vect:
         (vector
          (make-item value: 0.5 key: 'a)
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.7 key: 'c)
          (make-item value: 0.8 key: 'd)))

      (add-vect-shift-test!
       name-suffix: 'left-min-equal-neighbor
       idx: 0
       in-vect:
         (vector
          (make-item value: 0.6 key: 'a)
          #f
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.7 key: 'c)
          (make-item value: 0.8 key: 'd))
       out-vect:
         (vector
          (make-item value: 0.6 key: 'a)
          #f
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.7 key: 'c)
          (make-item value: 0.8 key: 'd)))

      (add-vect-shift-test!
       name-suffix: 'right-max-equal-neighbor
       idx: 4
       in-vect:
         (vector
          (make-item value: 0.5 key: 'a)
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.7 key: 'c)
          #f
          (make-item value: 0.7 key: 'd))
       out-vect:
         (vector
          (make-item value: 0.5 key: 'a)
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.7 key: 'c)
          #f
          (make-item value: 0.7 key: 'd)))

      (add-vect-shift-test!
       name-suffix: 'middle-shifting-left-gap
       idx: 3
       in-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.6 key: 'b)
          #f
          (make-item value: 0.5 key: 'c)
          (make-item value: 0.8 key: 'd))
       out-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.5 key: 'c)
          #f
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.8 key: 'd)))

      (add-vect-shift-test!
       name-suffix: 'middle-shifting-left-no-gap
       idx: 2
       in-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.5 key: 'c)
          (make-item value: 0.8 key: 'd))
       out-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.5 key: 'c)
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.8 key: 'd)))

      (add-vect-shift-test!
       name-suffix: 'middle-shifting-right-gap
       idx: 1
       in-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.6 key: 'b)
          #f
          (make-item value: 0.5 key: 'c)
          (make-item value: 0.8 key: 'd))
       out-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.5 key: 'c)
          #f
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.8 key: 'd)))

      (add-vect-shift-test!
       name-suffix: 'middle-shifting-right-no-gap
       idx: 1
       in-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.5 key: 'c)
          (make-item value: 0.8 key: 'd))
       out-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.5 key: 'c)
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.8 key: 'd)))

      (add-vect-shift-test!
       name-suffix: 'middle-alright
       idx: 2
       in-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.5 key: 'b)
          (make-item value: 0.6 key: 'c)
          (make-item value: 0.7 key: 'd))
       out-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.5 key: 'b)
          (make-item value: 0.6 key: 'c)
          (make-item value: 0.7 key: 'd)))

      (add-vect-shift-test!
       name-suffix: 'middle-left-equals
       idx: 5
       in-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.6 key: 'c)
          (make-item value: 0.6 key: 'd)
          (make-item value: 0.6 key: 'e)
          (make-item value: 0.5 key: 'f)
          (make-item value: 0.7 key: 'g))
       out-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.5 key: 'f)
          (make-item value: 0.6 key: 'c)
          (make-item value: 0.6 key: 'd)
          (make-item value: 0.6 key: 'e)
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.7 key: 'g)))

      (add-vect-shift-test!
       name-suffix: 'middle-static-item
       idx: 2
       in-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.6 key: 'c)
          (make-item value: 0.6 key: 'd)
          (make-item value: 0.6 key: 'e)
          (make-item value: 0.5 key: 'f)
          (make-item value: 0.7 key: 'g))
       out-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.6 key: 'c)
          (make-item value: 0.6 key: 'd)
          (make-item value: 0.6 key: 'e)
          (make-item value: 0.5 key: 'f)
          (make-item value: 0.7 key: 'g)))

      (add-vect-shift-test!
       name-suffix: 'middle-right-equals
       idx: 1
       in-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.7 key: 'b)
          (make-item value: 0.6 key: 'c)
          (make-item value: 0.6 key: 'd)
          (make-item value: 0.6 key: 'e)
          (make-item value: 0.6 key: 'f)
          (make-item value: 0.8 key: 'g))
       out-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          (make-item value: 0.6 key: 'f)
          (make-item value: 0.6 key: 'c)
          (make-item value: 0.6 key: 'd)
          (make-item value: 0.6 key: 'e)
          (make-item value: 0.7 key: 'b)
          (make-item value: 0.8 key: 'g)))

      (add-vect-shift-test!
       name-suffix: 'middle-left-multiple-hops
       idx: 16
       in-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          #f
          (make-item value: 0.6 key: 'b)
          #f
          #f
          (make-item value: 0.6 key: 'c)
          (make-item value: 0.6 key: 'd)
          #f
          #f
          (make-item value: 0.6 key: 'e)
          (make-item value: 0.7 key: 'f)
          (make-item value: 0.7 key: 'g)
          (make-item value: 0.8 key: 'h)
          (make-item value: 0.9 key: 'i)
          (make-item value: 1.0 key: 'j)
          (make-item value: 1.0 key: 'k)
          (make-item value: 0.5 key: 'l)
          (make-item value: 1.1 key: 'm))
       out-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          #f
          (make-item value: 0.5 key: 'l)
          #f
          #f
          (make-item value: 0.6 key: 'c)
          (make-item value: 0.6 key: 'd)
          #f
          #f
          (make-item value: 0.6 key: 'e)
          (make-item value: 0.6 key: 'b)
          (make-item value: 0.7 key: 'g)
          (make-item value: 0.7 key: 'f)
          (make-item value: 0.8 key: 'h)
          (make-item value: 0.9 key: 'i)
          (make-item value: 1.0 key: 'k)
          (make-item value: 1.0 key: 'j)
          (make-item value: 1.1 key: 'm)))

      (add-vect-shift-test!
       name-suffix: 'middle-right-multiple-hops
       idx: 2
       in-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          #f
          (make-item value: 0.9 key: 'b)
          (make-item value: 0.5 key: 'c)
          #f
          #f
          (make-item value: 0.5 key: 'd)
          (make-item value: 0.5 key: 'e)
          #f
          (make-item value: 0.6 key: 'f)
          (make-item value: 0.6 key: 'g)
          (make-item value: 0.7 key: 'h)
          (make-item value: 0.8 key: 'i)
          #f
          (make-item value: 1.0 key: 'j))
       out-vect:
         (vector
          (make-item value: 0.4 key: 'a)
          #f
          (make-item value: 0.5 key: 'e)
          (make-item value: 0.5 key: 'c)
          #f
          #f
          (make-item value: 0.5 key: 'd)
          (make-item value: 0.6 key: 'g)
          #f
          (make-item value: 0.6 key: 'f)
          (make-item value: 0.7 key: 'h)
          (make-item value: 0.8 key: 'i)
          (make-item value: 0.9 key: 'b)
          #f
          (make-item value: 1.0 key: 'j)))

      )

    (add-test!
     name: '%tile-sort-depths!
     func:
     (lambda ()
       ;; ok, so, simple simple
       ;; we want two tiles
       ;; we want the tiles to be out of order, depth-wise
       ;; so when sorted, they get swapped
       (let* ((tile-a
               (make-tile
                element-buffer-index:     0
                array-buffer-start-index: 0))
              (tile-b
               (make-tile
                element-buffer-index:     6
                array-buffer-start-index: 4))
              (tile-vector (vector tile-a tile-b))
              (coll
               (orig-make-tile-collection
                vert-buffer-data:
                  (f32vector 0 0 -0.1
                             1 0 -0.1
                             1 1 -0.1
                             0 1 -0.1

                             0 0 -0.9
                             1 1 -0.9
                             1 1 -0.9
                             0 0 -0.9)
                texcoord-buffer-data:
                  (f32vector 0 0
                             1 0
                             1 1
                             0 1

                             0 0
                             1 0
                             1 1
                             0 1)
                texlayer-buffer-data:
                  (s32vector 0 0 0 0

                             0 0 0 0)
                element-buffer-data:
                  (u16vector 0 1 2
                             2 3 0

                             4 5 6
                             6 7 4)
                tile-vector: tile-vector
                gl-texture-dimensions: '(1 1 1)))
              (set  (make-tileset tile-collection: coll
                                  layer: 0))
              (spec (make-tile-spec tileset: set
                                    start: '(0 0)
                                    dimensions: '(1 1)
                                    layer: 0)))
         (tile-tile-spec-set! tile-a spec)
         (tile-tile-spec-set! tile-b spec)
         (with-wiped-funcs (glBindBuffer glBufferData
                                         glBufferSubData)
          (%tile-sort-depths! tile-a)
          ;; ok, so, check to make sure the vector got swapped
          ;; and that the element buffer is updated
          (or (eq? (vector-ref tile-vector 0) tile-b)
              (error "%tile-sort-depths! failed to move tile-b ahead"))
          (or (eq? (vector-ref tile-vector 1) tile-a)
              (error "%tile-sort-depths! failed to move tile-a down"))
          (or (equal? (tile-collection-element-buffer-data coll)
                      (u16vector 4 5 6
                                 6 7 4
                                 0 1 2
                                 2 3 0))
              (error "%tile-sort-depths! failed to update element buffer"))))))

    (for-each
     (lambda (test-pair)
       (let ((name (car test-pair))
             (func (cdr test-pair)))
         (handle-exceptions
             exn
             (format #t "error with ~s test: ~s\n" name (condition->list exn))
           (func))))
     tests)))
