(import (srfi-151))
(import (egggame sdl)
        (egggame glew)
        (egggame devil)
        (srfi-4))
(import (chicken format) (chicken blob) (chicken port))

(or (SDL_InitSubSystem SDL_INIT_VIDEO)
    (error (format "failed to init video: ~a" (SDL_GetError))))

(ilInit)


(define *window*
  (or (SDL_CreateWindow "fucking around" 400 300 (bitwise-ior SDL_WINDOW_OPENGL))
      (error (format "failed to create window: ~a" (SDL_GetError)))))

(define *gl-context* (SDL_GL_CreateContext *window*))

(unless (>= (SDL_GL_GetAttribute SDL_GL_CONTEXT_MAJOR_VERSION) 2)
  (error "gl version too low"))

(unless (= (glewInit) GLEW_OK)
  (error "failed to initialize glew"))

(or (SDL_GL_SetSwapInterval 1)
    (error "failed to set swap interval" (SDL_GetError)))

(define *ev* (make-SDL_Event))

;; making buffers
(define (make-buffer target data)
  (let ((res (u32vector 5))
        (count (blob-size data)))
    (glGenBuffers 1 res)
    (glBindBuffer target (u32vector-ref res 0))
    (glBufferData target count data GL_STATIC_DRAW)
    (u32vector-ref res 0)))

(define *vert-buffer-data*
  (make-buffer GL_ARRAY_BUFFER
               (f32vector->blob/shared
;                (f32vector -1 -1
;                            0 -1
;                           -1  1
;                            0  1
;                            )
                (f32vector -1 -1
                            1 -1
                           -1  1
                            1  1
                            0 -1
                            0  1
                            )
)))
(define *element-buffer-data*
  (make-buffer GL_ELEMENT_ARRAY_BUFFER
               (u16vector->blob/shared
                (u16vector 0 1 2 3
))))

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


;; textures
(define *tex-1* (allocate-gl-texture "scrap-images/gl2-hello-1.png"))
(define *tex-2* (allocate-gl-texture "scrap-images/gl2-hello-2.png"))

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

;; shaders
(define vert-shader-text #<<END
attribute vec2 position;

varying vec2 texcoord;

void main()
{
    gl_Position = vec4(position, 0.0, 1.0);
    texcoord = position * vec2(0.5) + vec2(0.5);
}
END
)

(define fragment-shader-text #<<END

uniform float fade_factor;
uniform sampler2D textures[2];

varying vec2 texcoord;

void main()
{
    gl_FragColor = mix(
        texture2D(textures[0], texcoord),
        texture2D(textures[1], texcoord),
        fade_factor
    );
}
END
)

(define *vert-shader* (make-shader GL_VERTEX_SHADER vert-shader-text))
(define *fragment-shader* (make-shader GL_FRAGMENT_SHADER fragment-shader-text))

(define *program* (make-program *vert-shader* *fragment-shader*))

;; uniforms
(define *u-fade-factor* (glGetUniformLocation *program* "fade_factor"))
(define *u-textures-0* (glGetUniformLocation *program* "textures[0]"))
(define *u-textures-1* (glGetUniformLocation *program* "textures[1]"))

;; attributes
(define *a-position* (glGetAttribLocation *program* "position"))

(define-syntax while
  (syntax-rules ()
    ((while test body . rest)
     (let iter ()
       (if test (begin (begin body . rest) (iter)))))))

(define running? #t)

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

(define +cycle+ 650.0)

(define (draw!)
  (glUseProgram *program*)


  (glUniform1f
   *u-fade-factor*
   1.0)

  ;; setting up uniforms
  ;; ok so
#;  (glUniform1f
   *u-fade-factor*
   (let* ((double-cycle-progress (modulo (SDL_GetTicks) (* 2 +cycle+))))
     (printf "debugk01 ~s (if ~s ~s ~s)\n"
             double-cycle-progress
             (< double-cycle-progress +cycle+)
             (/ double-cycle-progress +cycle+)
             (/ (- (* 2 +cycle+) double-cycle-progress) +cycle+))
     (if (< double-cycle-progress +cycle+)
         (/ double-cycle-progress +cycle+)
         (/ (- (* 2 +cycle+) double-cycle-progress) +cycle+))))

  (glActiveTexture GL_TEXTURE0)
  (glBindTexture GL_TEXTURE_2D *tex-1*)
  (glUniform1i *u-textures-0* 0)

  (glActiveTexture GL_TEXTURE1)
  (glBindTexture GL_TEXTURE_2D *tex-2*)
  (glUniform1i *u-textures-1* 1)

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
