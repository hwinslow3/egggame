(module (egggame sdl) *

(import (scheme) (chicken base) (chicken syntax) (chicken module))
(import (chicken foreign))

(foreign-declare "#include <SDL3/SDL.h>")

;; =============================================================================
;; init functions
;; =============================================================================
(define SDL_INIT_AUDIO (foreign-value "SDL_INIT_AUDIO" unsigned-integer32))
(define SDL_INIT_VIDEO (foreign-value "SDL_INIT_VIDEO" unsigned-integer32))
(define SDL_INIT_JOYSTICK (foreign-value "SDL_INIT_JOYSTICK" unsigned-integer32))
(define SDL_INIT_HAPTIC (foreign-value "SDL_INIT_HAPTIC" unsigned-integer32))
(define SDL_INIT_GAMEPAD (foreign-value "SDL_INIT_GAMEPAD" unsigned-integer32))
(define SDL_INIT_EVENTS (foreign-value "SDL_INIT_EVENTS" unsigned-integer32))
(define SDL_INIT_SENSOR (foreign-value "SDL_INIT_SENSOR" unsigned-integer32))
(define SDL_INIT_CAMERA (foreign-value "SDL_INIT_CAMERA" unsigned-integer32))

(define SDL_Init (foreign-lambda bool SDL_Init unsigned-integer32))
(define SDL_InitSubSystem (foreign-lambda bool SDL_InitSubSystem unsigned-integer32))
(define SDL_Quit (foreign-lambda void SDL_Quit))
(define SDL_QuitSubSystem (foreign-lambda void SDL_QuitSubSystem unsigned-integer32))

(define SDL_GetError (foreign-lambda c-string SDL_GetError))

;; =============================================================================
;; video functions
;; =============================================================================

(define SDL_WINDOW_FULLSCREEN (foreign-value "SDL_WINDOW_FULLSCREEN" unsigned-integer64))
(define SDL_WINDOW_OPENGL (foreign-value "SDL_WINDOW_OPENGL" unsigned-integer64))
(define SDL_WINDOW_OCCLUDED (foreign-value "SDL_WINDOW_OCCLUDED" unsigned-integer64))
(define SDL_WINDOW_HIDDEN (foreign-value "SDL_WINDOW_HIDDEN" unsigned-integer64))
(define SDL_WINDOW_BORDERLESS (foreign-value "SDL_WINDOW_BORDERLESS" unsigned-integer64))
(define SDL_WINDOW_RESIZABLE (foreign-value "SDL_WINDOW_RESIZABLE" unsigned-integer64))
(define SDL_WINDOW_MINIMIZED (foreign-value "SDL_WINDOW_MINIMIZED" unsigned-integer64))
(define SDL_WINDOW_MAXIMIZED (foreign-value "SDL_WINDOW_MAXIMIZED" unsigned-integer64))
(define SDL_WINDOW_MOUSE_GRABBED (foreign-value "SDL_WINDOW_MOUSE_GRABBED" unsigned-integer64))
(define SDL_WINDOW_INPUT_FOCUS (foreign-value "SDL_WINDOW_INPUT_FOCUS" unsigned-integer64))
(define SDL_WINDOW_MOUSE_FOCUS (foreign-value "SDL_WINDOW_MOUSE_FOCUS" unsigned-integer64))
(define SDL_WINDOW_EXTERNAL (foreign-value "SDL_WINDOW_EXTERNAL" unsigned-integer64))
(define SDL_WINDOW_MODAL (foreign-value "SDL_WINDOW_MODAL" unsigned-integer64))
(define SDL_WINDOW_HIGH_PIXEL_DENSITY (foreign-value "SDL_WINDOW_HIGH_PIXEL_DENSITY" unsigned-integer64))
(define SDL_WINDOW_MOUSE_CAPTURE (foreign-value "SDL_WINDOW_MOUSE_CAPTURE" unsigned-integer64))
(define SDL_WINDOW_MOUSE_RELATIVE_MODE (foreign-value "SDL_WINDOW_MOUSE_RELATIVE_MODE" unsigned-integer64))
(define SDL_WINDOW_ALWAYS_ON_TOP (foreign-value "SDL_WINDOW_ALWAYS_ON_TOP" unsigned-integer64))
(define SDL_WINDOW_UTILITY (foreign-value "SDL_WINDOW_UTILITY" unsigned-integer64))
(define SDL_WINDOW_TOOLTIP (foreign-value "SDL_WINDOW_TOOLTIP" unsigned-integer64))
(define SDL_WINDOW_POPUP_MENU (foreign-value "SDL_WINDOW_POPUP_MENU" unsigned-integer64))
(define SDL_WINDOW_KEYBOARD_GRABBED (foreign-value "SDL_WINDOW_KEYBOARD_GRABBED" unsigned-integer64))
(define SDL_WINDOW_VULKAN (foreign-value "SDL_WINDOW_VULKAN" unsigned-integer64))
(define SDL_WINDOW_METAL (foreign-value "SDL_WINDOW_METAL" unsigned-integer64))
(define SDL_WINDOW_TRANSPARENT (foreign-value "SDL_WINDOW_TRANSPARENT" unsigned-integer64))
(define SDL_WINDOW_NOT_FOCUSABLE (foreign-value "SDL_WINDOW_NOT_FOCUSABLE" unsigned-integer64))

(define SDL_CreateWindow
  (foreign-lambda c-pointer SDL_CreateWindow
    c-string int int unsigned-integer64))

;; =============================================================================
;; event stuff
;; =============================================================================

(define sizeof-SDL_Event (foreign-value "sizeof(SDL_Event)" int))

(define SDL_PollEvent (foreign-lambda bool SDL_PollEvent c-pointer))

)
