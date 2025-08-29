(module (egggame sdlutil) *

(import (scheme)
        (chicken format)
        (chicken port)
        (chicken base)
        (egggame sdl))

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

)
