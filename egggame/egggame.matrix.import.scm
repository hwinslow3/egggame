(module (egggame matrix) *

(import (scheme)
        (chicken base)
        (srfi-4)
        (srfi-1)
        (defstruct))

(defstruct matrix
  dimensions
  data)

(define (matrix-flat-index mat idx)
  ;; ok, so
  ;; if we've got
  ;; X Y Z
  ;; D E F

  ;; so column major ref (a b)
  ;; (+ b (* a height))

  (+ (cadr idx) (* (car idx) (cadr (matrix-dimensions mat)))))

(define (->inexact x)
  (if (exact? x)
      (exact->inexact x)
      x))

(define (matrix-set! mat idx val)
  (set! (f32vector-ref (matrix-data mat) (matrix-flat-index mat idx)) (->inexact val)))

(define (%matrix-ref mat idx)
  (f32vector-ref (matrix-data mat) (matrix-flat-index mat idx)))

(define matrix-ref (getter-with-setter %matrix-ref matrix-set!))

(define (identity-matrix dim)
  (let* ((data (make-f32vector (apply * dim) 0.0))
         (res (make-matrix data: data dimensions: dim)))
    (for-each
     (lambda (idx)
       (set! (matrix-ref res (list idx idx)) 1.0))
     (iota (apply min dim)))
    res))

(define (zero-matrix dim)
  (make-matrix data: (make-f32vector (apply * dim) 0.0) dimensions: dim))

(define (matrix-multiply a b)
  (let* ((dim (list (car (matrix-dimensions b)) (cadr (matrix-dimensions a))))
         (res (zero-matrix dim)))
    (for-each
     (lambda (x)
       (for-each
        (lambda (y)
          (for-each
           (lambda (z)
             (set! (matrix-ref res (list x y))
               (+ (matrix-ref res (list x y))
                  (* (matrix-ref b (list x z))
                     (matrix-ref a (list z y))))))
           (iota (cadr (matrix-dimensions b)))))
        (iota (cadr dim))))
     (iota (car dim)))
    res))

(define (ortho-matrix #!key left right far near top bottom)
  (let ((res (zero-matrix '(4 4))))

    (set! (matrix-ref res '(0 0)) (/ 2 (- right left)))
    (set! (matrix-ref res '(1 1)) (/ 2 (- top bottom)))
    (set! (matrix-ref res '(2 2)) (/ -2 (- far near)))
    (set! (matrix-ref res '(3 3)) 1)

    (set! (matrix-ref res '(0 3)) (- (/ (+ right left) (- right left))))
    (set! (matrix-ref res '(1 3)) (- (/ (+ top bottom) (- top bottom))))
    (set! (matrix-ref res '(2 3)) (- (/ (+ far near) (- far near))))

    res))

(define (shift-2d-ortho-matrix! mat #!key dimensions start)
  (let* ((left   (car start))
         (right  (+ left (car dimensions)))
         (top    (cadr start))
         (bottom (+ top (cadr dimensions)))
         )
    (set! (matrix-ref mat '(0 0)) (/ 2 (- right left)))
    (set! (matrix-ref mat '(1 1)) (/ 2 (- top bottom)))
    (set! (matrix-ref mat '(0 3)) (- (/ (+ right left) (- right left))))
    (set! (matrix-ref mat '(1 3)) (- (/ (+ top bottom) (- top bottom))))))

)
