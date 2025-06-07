(import (srfi-151))
(import (egggame sdl)
        (egggame glew)
        (egggame devil)
        (srfi-4))
(import (chicken format) (chicken blob) (chicken port))

(load "scrap-code.scm")

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

;; textures
(define *tex-1* (allocate-gl-texture "scrap-images/gl2-hello-1.png"))
(define *tex-2* (allocate-gl-texture "scrap-images/gl2-hello-2.png"))

;; shaders
(define vert-shader-text #<<END
#version 110

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
#version 110

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

(define running? #t)

;; main regulator
(define regulate! (make-fps-regulator 50))

(define +cycle+ 650.0)

;end
