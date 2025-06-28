(import (srfi-151))
(import (egggame sdl)
        (egggame glew)
        (egggame devil)
        (srfi-4))
(import (chicken format) (chicken blob) (chicken port))

(load "scrap-code.scm")
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

(define *vert-buffer-vect*
  (f32vector
                           0 (cadr +window-dimensions+)
   (car +window-dimensions+) (cadr +window-dimensions+)
                           0                          0
   (car +window-dimensions+)                          0
   ))

(define (adjust-vert-buffer! offset)
  (for-each
   (lambda (idx)
     (set! (f32vector-ref *vert-buffer-vect* idx)
           (+ (f32vector-ref *vert-buffer-vect* idx) (car offset))))
   '(0 2 4 6))
  (for-each
   (lambda (idx)
     (set! (f32vector-ref *vert-buffer-vect* idx)
           (+ (f32vector-ref *vert-buffer-vect* idx) (cadr offset))))
   '(1 3 5 7))
  (update-buffer! *vert-buffer-data* GL_ARRAY_BUFFER
                  (f32vector->blob/shared *vert-buffer-vect*)))

(define *vert-buffer-data*
  (make-buffer GL_ARRAY_BUFFER
               (f32vector->blob/shared
;                (f32vector -1 -1
;                            0 -1
;                           -1  1
;                            0  1
;                            )
;                (f32vector -1 -1 ; bottom left
;                            1 -1 ; bottom right
;                           -1  1 ; top left
;                            1  1 ; top right
;                            )
                *vert-buffer-vect*
)))

(define cycle-text-coords!
  (let ((items
         (list
          (map / +window-dimensions+ +window-dimensions+)
          (map / (list 0                          0) +window-dimensions+)
          (map / (list (car +window-dimensions+) 0) +window-dimensions+)
          (map / (list 0 (cadr +window-dimensions+)) +window-dimensions+)
          )))
    (lambda ()
      (set! items (append (cdr items) (list (car items))))
      (update-buffer! *text-coord-buffer-data*
                      GL_ARRAY_BUFFER
                      (f32vector->blob/shared
                       (list->f32vector
                        (flatten items)))))))

(define *tex-idx-buffer-vect* (f32vector 0 0 0 0))
(define (set-texture-idx! value)
  (format #t "debugk01 switching ~s\n" value)
;  (cycle-text-coords!)

  (for-each
   (lambda (idx) (set! (f32vector-ref *tex-idx-buffer-vect* idx) value))
   '(0 1 2 3))
  (update-buffer! *text-idx-buffer-data*
                  GL_ARRAY_BUFFER
                  (f32vector->blob/shared *tex-idx-buffer-vect*)))

(define *text-coord-buffer-data*
  (make-buffer
   GL_ARRAY_BUFFER
   (f32vector->blob/shared
    (list->f32vector
     (flatten
      (list
       (map / (list 0 (cadr +window-dimensions+)) +window-dimensions+)
       (map / +window-dimensions+ +window-dimensions+)
       (map / (list 0                          0) +window-dimensions+)
       (map / (list (car +window-dimensions+) 0) +window-dimensions+)
       )
    )))))
(define *text-idx-buffer-data*
  (make-buffer
   GL_ARRAY_BUFFER
   (f32vector->blob/shared *tex-idx-buffer-vect*)))
(define *element-buffer-data*
  (make-buffer GL_ELEMENT_ARRAY_BUFFER
               (u16vector->blob/shared
                (u16vector 0 1 2
                           1 3 2
))))

;; textures
;(define *tex-1* (allocate-gl-texture "scrap-images/gl2-hello-1.png"))
;(define *tex-2* (allocate-gl-texture "scrap-images/gl2-hello-2.png"))

(define *tex* (allocate-gl-texture-array
               '("scrap-images/gl2-hello-1.png"
                 "scrap-images/gl2-hello-2.png")))

;; shaders
(define vert-shader-text #<<END
#version 130

in vec2 position;
in vec2 in_texcoord;
in int in_texidx;
uniform mat4 camera;

out vec2 texcoord;
flat out int texidx;

void main()
{
    gl_Position = camera * vec4(position, 0.0, 1.0);
    texcoord = in_texcoord;
    texidx = in_texidx;
}
END
)

(define fragment-shader-text #<<END
#version 130

uniform float fade_factor;
uniform sampler2DArray textures;

in vec2 texcoord;
flat in int texidx;

void main()
{
  gl_FragColor = texture(textures, vec3(texcoord, texidx));
//    gl_FragColor = mix(
//        texture2D(textures[0], texcoord),
//        texture2D(textures[1], texcoord),
//        fade_factor
//    );
}
END
)

(define *vert-shader* (make-shader GL_VERTEX_SHADER vert-shader-text))
(define *fragment-shader* (make-shader GL_FRAGMENT_SHADER fragment-shader-text))

(define *program* (make-program *vert-shader* *fragment-shader*))

;; uniforms
(define *u-fade-factor* (glGetUniformLocation *program* "fade_factor"))
(define *u-textures* (glGetUniformLocation *program* "textures"))
;(define *u-textures-0* (glGetUniformLocation *program* "textures[0]"))
;(define *u-textures-1* (glGetUniformLocation *program* "textures[1]"))
(define *u-camera* (glGetUniformLocation *program* "camera"))

;; attributes
(define *a-position* (glGetAttribLocation *program* "position"))
(define *a-in-texcoord* (glGetAttribLocation *program* "in_texcoord"))
(define *a-in-texidx* (glGetAttribLocation *program* "in_texidx"))

(define running? #t)

;; main regulator
(define regulate! (make-fps-regulator 28))

(define +cycle+ 650.0)

(define *camera-offset* '(0 0))

(define (camera-matrix)
  (let ((x-offset (car *camera-offset*))
        (y-offset (cadr *camera-offset*)))
    (ortho-matrix
     left: x-offset
     right: (+ x-offset (car +window-dimensions+))
     top: y-offset
     bottom: (+ y-offset (cadr +window-dimensions+))
     near: 0 far: 1)))

;end
