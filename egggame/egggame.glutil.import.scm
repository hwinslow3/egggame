(module (egggame glutil) *

(import (scheme)
        (chicken base)
        (chicken syntax)
        (chicken module)
        (egggame glew)
        (srfi-4)
        (chicken blob))

(define (check-gl-error msg)
  (let ((err (glGetError)))
    (unless (= err GL_NO_ERROR)
      (error msg (gluErrorString err)))))

(define (make-buffer target data)
  (let ((res (u32vector 5))
        (count (blob-size data)))
    (glGenBuffers 1 res)
    (glBindBuffer target (u32vector-ref res 0))
    (glBufferData target count data GL_DYNAMIC_DRAW)
    (u32vector-ref res 0)))

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

)
