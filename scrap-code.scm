(import (srfi-151))
(import (egggame sdl)
        (egggame glew)
        (egggame devil)
        (srfi-4))
(import (chicken format) (chicken blob) (chicken port))

;; making buffers
(define (make-buffer target data)
  (let ((res (u32vector 5))
        (count (blob-size data)))
    (glGenBuffers 1 res)
    (glBindBuffer target (u32vector-ref res 0))
    (glBufferData target count data GL_STATIC_DRAW)
    (u32vector-ref res 0)))

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

  (glActiveTexture GL_TEXTURE0)
  (check-gl-error "post active texture 0")
  (glBindTexture GL_TEXTURE_2D *tex-1*)
  (check-gl-error "post bind texture 0")
  (glUniform1i *u-textures-0* 0)
  (check-gl-error "post uniform texture 0")

  (glActiveTexture GL_TEXTURE1)
  (check-gl-error "post active texture 1")
  (glBindTexture GL_TEXTURE_2D *tex-2*)
  (check-gl-error "post bind texture 1")
  (glUniform1i *u-textures-1* 1)
  (check-gl-error "post uniform texture 1")

  (glBindBuffer GL_ARRAY_BUFFER *vert-buffer-data*)
  (glVertexAttribPointer *a-position* 2 GL_FLOAT GL_FALSE (* 2 sizeof-GLfloat) #f)
  (glEnableVertexAttribArray *a-position*)

  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER *element-buffer-data*)
  (glDrawElements GL_TRIANGLE_STRIP 4 GL_UNSIGNED_SHORT #f)

  (glDisableVertexAttribArray *a-position*)
)

(define (main-loop)
  (while running?
    (while (SDL_PollEvent *ev*)
      (cond
       ((= (SDL_Event-type *ev*) SDL_EVENT_QUIT)
        (set! running? #f))
       (else
        (printf "unrecognized event type: ~a\n" (SDL_Event-type *ev*)))))
    (regulate!)
    (draw!)
    (SDL_GL_SwapWindow *window*)))

;end
