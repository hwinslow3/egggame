(import (srfi-151))
(import (egggame sdl)
        (egggame glew)
        (egggame devil)
        (srfi-4)
        (srfi-1)
        (defstruct))
(import (chicken format) (chicken blob) (chicken port))

;; making buffers
(define (make-buffer target data)
  (let ((res (u32vector 5))
        (count (blob-size data)))
    (glGenBuffers 1 res)
    (glBindBuffer target (u32vector-ref res 0))
    (glBufferData target count data GL_DYNAMIC_DRAW)
    (u32vector-ref res 0)))

(define (update-buffer! buf target data)
  (let ((count (blob-size data)))
    (glBindBuffer target buf)
    (glBufferData target count data GL_DYNAMIC_DRAW)))

;; texture allocation
(define (check-il-error msg)
  (let ((err (ilGetError)))
    (unless (= err IL_NO_ERROR)
      (error msg (iluErrorString err)))))

(define (check-gl-error msg)
  (let ((err (glGetError)))
    (unless (= err GL_NO_ERROR)
      (error msg (gluErrorString err)))))

(define (allocate-gl-texture filename)
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

    (let ((gl-tex
           (let ((names (u32vector 4)))
             (glGenTextures 1 names)
             (u32vector-ref names 0))))
      (glBindTexture GL_TEXTURE_2D gl-tex)

      (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA
                    (ilGetInteger IL_IMAGE_WIDTH) (ilGetInteger IL_IMAGE_HEIGHT)
                    0 GL_RGBA GL_UNSIGNED_BYTE
                    (ilGetData))
      (check-il-error "failed to get devil image width, height or data")

      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)

      (ilDeleteImages 1 (u32vector img))

      (check-il-error "failed to delete devil image")

      (check-gl-error "failed to create gl texture")

      gl-tex)))

(define (allocate-gl-texture-array filenames)
  (let ((gl-tex
         (let ((names (u32vector 4)))
           (glGenTextures 1 names)
           (u32vector-ref names 0)))
        (initialized-storage? #f))
    (glBindTexture GL_TEXTURE_2D_ARRAY gl-tex)

    (check-gl-error "failed to bind 3d gl texture")

    (for-each
     (lambda (filename layer)
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

         (unless initialized-storage?
           (glTexStorage3D
            GL_TEXTURE_2D_ARRAY 1 GL_RGBA8
            (ilGetInteger IL_IMAGE_WIDTH) (ilGetInteger IL_IMAGE_HEIGHT)
            (length filenames))
           (check-gl-error "failed to initialize 3d gl texture storage")
           (set! initialized-storage? #t))

         (glTexSubImage3D
          GL_TEXTURE_2D_ARRAY 0 0 0 layer
          (ilGetInteger IL_IMAGE_WIDTH) (ilGetInteger IL_IMAGE_HEIGHT)
          1
          GL_RGBA GL_UNSIGNED_BYTE (ilGetData))

         (check-gl-error "failed to assign 3d  gl sub texture")))
     filenames
     (iota (length filenames)))

    (glTexParameteri GL_TEXTURE_2D_ARRAY GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D_ARRAY GL_TEXTURE_MIN_FILTER GL_LINEAR)

    gl-tex))

;; shader stuff
(define (get-info-log obj get get-info-log)
  (let* ((info-log-len
          (let ((output (s32vector 45)))
            (get obj GL_INFO_LOG_LENGTH output)
            (s32vector-ref output 0)))
         (info-log (make-blob (+ 1 info-log-len))))
    (get-info-log obj info-log-len #f info-log)
    (blob->string info-log)))

(define (make-shader type text)
  (let ((shader (glCreateShader type)))
    (check-gl-error "failed to create gl shader")

    (glShaderSource/single shader text)
    (check-gl-error "failed to load shader source")

    (glCompileShader shader)
    (let ((compiled?
           (let ((info (s32vector 34)))
             (glGetShaderiv shader GL_COMPILE_STATUS info)
             (not (zero? (s32vector-ref info 0))))))
      (unless compiled?
        (let ((info-log (get-info-log shader glGetShaderiv glGetShaderInfoLog)))
          (glDeleteShader shader)
          (error "failed to make shader" info-log)))
      shader)))

(define (make-program vert frag)
  (let ((prog (glCreateProgram)))
    (glAttachShader prog vert)
    (glAttachShader prog frag)
    (glLinkProgram prog)
    (let ((linked?
           (let ((info (s32vector 67)))
             (glGetProgramiv prog GL_LINK_STATUS info)
             (not (zero? (s32vector-ref info 0))))))
      (unless linked?
        (let ((info-log (get-info-log prog glGetProgramiv glGetProgramInfoLog)))
          (glDeleteProgram prog)
          (error "failed to link program" info-log)))
      prog)))

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

;; main regulator
(define regulate! (make-fps-regulator 30))

(define (check-gl-error msg)
  (let ((err (glGetError)))
    (unless (zero? err)
      (error "got gl error" msg
             err
             (gluErrorString err)))))

(define (draw!)
  (check-gl-error "pre-use-program")
  (glUseProgram *program*)
  (check-gl-error "post-use-program")

  ;; setting up uniforms
  ;; ok so
  (glUniform1f
   *u-fade-factor*
   (let* ((double-cycle-progress (modulo (SDL_GetTicks) (* 2 +cycle+))))
     (if (< double-cycle-progress +cycle+)
         (/ double-cycle-progress +cycle+)
         (/ (- (* 2 +cycle+) double-cycle-progress) +cycle+))))

  (glUniformMatrix4fv *u-camera* 1 #t (matrix-data (camera-matrix)))

  (glActiveTexture GL_TEXTURE0)
  (check-gl-error "post active texture 0")
  (glBindTexture GL_TEXTURE_2D_ARRAY *tex*)
  (check-gl-error "post bind texture 0")
  (glUniform1i *u-textures* 0)
  (check-gl-error "post uniform texture 0")

;  (glActiveTexture GL_TEXTURE0)
;  (check-gl-error "post active texture 0")
;  (glBindTexture GL_TEXTURE_2D *tex-1*)
;  (check-gl-error "post bind texture 0")
;  (glUniform1i *u-textures-0* 0)
;  (check-gl-error "post uniform texture 0")

;  (glActiveTexture GL_TEXTURE1)
;  (check-gl-error "post active texture 1")
;  (glBindTexture GL_TEXTURE_2D *tex-2*)
;  (check-gl-error "post bind texture 1")
;  (glUniform1i *u-textures-1* 1)
;  (check-gl-error "post uniform texture 1")

  (glBindBuffer GL_ARRAY_BUFFER *vert-buffer-data*)
  (glVertexAttribPointer *a-position* 2 GL_FLOAT GL_FALSE (* 2 sizeof-GLfloat) #f)
  (glEnableVertexAttribArray *a-position*)

  (glBindBuffer GL_ARRAY_BUFFER *text-coord-buffer-data*)
  (glVertexAttribPointer *a-in-texcoord* 2 GL_FLOAT GL_FALSE (* 2 sizeof-GLfloat) #f)
  (glEnableVertexAttribArray *a-in-texcoord*)

  (check-gl-error "post-enable-text-coords-buffer")

  (glBindBuffer GL_ARRAY_BUFFER *text-idx-buffer-data*)
  (check-gl-error "post-vertex-idx-attrib-bind-buffer")
  (glVertexAttribPointer *a-in-texidx* 1 GL_FLOAT GL_FALSE (* 1 sizeof-GLfloat) #f)
  (check-gl-error "post-vertex-idx-attrib-buffer")
  (glEnableVertexAttribArray *a-in-texidx*)

  (check-gl-error "post-enable-text-idx-buffer")

  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER *element-buffer-data*)
;  (glDrawElements GL_TRIANGLE_STRIP 4 GL_UNSIGNED_SHORT #f)
  (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT #f)

  (glDisableVertexAttribArray *a-position*)
  (glDisableVertexAttribArray *a-in-texcoord*)
  (glDisableVertexAttribArray *a-in-texidx*)

  (check-gl-error "post-disable-verts")
)

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
           (set-texture-idx! 1)
           (set! mouse-button-down? #t))
          ((= (SDL_Event-type *ev*) SDL_EVENT_MOUSE_BUTTON_UP)
           (set-texture-idx! 0)
           (set! mouse-button-down? #f))
          ((= (SDL_Event-type *ev*) SDL_EVENT_MOUSE_MOTION)
           (when mouse-button-down?
             (adjust-vert-buffer!
              (list (SDL_Event-motion-xrel *ev*)
                    (SDL_Event-motion-yrel *ev*)))))
          (else
           (printf "unrecognized event type: ~a\n" (SDL_Event-type *ev*)))))))))

(define (main-loop)
  (while running?
    (while (event-cycle!)
      #t)
    (regulate!)
    (draw!)
    (SDL_GL_SwapWindow *window*)))

;; tile layout
'(
(defstruct tile-collection
  gl-program
)

(defstruct tileset)
(defstruct tile-listing)
(defstruct tile)

(define orig-make-tile-collection make-tile-collection)

(set! make-tile-collection
  (lambda ()
    (let* ((vert-shader-text #<<END
#version 110

attribute vec3 position;
attribute vec2 tex_coord;
uniform mat4 camera;
varying vec2 texcoord;

void main() {
  gl_Position = vec4(position, 1.0) * camera;
  texcoord = tex_coord;
}
END
)
           (vert-shader X)
           (frag-shader-text #<<END
END
)
           (gl-program X))
      (orig-make-tile-collection
       gl-program: gl-program
       tilesets: '()))))

(define (tile-collection-add-tileset! tc filename)
  X
)

(define (tileset-add-tile-listing! ts #!key start dimensions)
  X
)

(define (tile-collection-alloc-tile! tc tile)
  X
)

(define (draw-tile-collection! tc)
  X)
)

;end
