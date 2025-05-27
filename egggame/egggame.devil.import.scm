(module (egggame devil) *

(import (scheme)
        (chicken base)
        (chicken syntax)
        (chicken module)
        (chicken foreign)
        (srfi-4))

(foreign-declare "#include <IL/il.h>")
(foreign-declare "#include <IL/ilu.h>")

(define-foreign-type ILenum unsigned-int)

;; errors
(define IL_NO_ERROR (foreign-value "IL_NO_ERROR" ILenum))

;; formats
(define IL_BGRA (foreign-value "IL_BGRA" ILenum))
(define IL_COLOUR_INDEX (foreign-value "IL_COLOUR_INDEX" ILenum))
(define IL_LUMINANCE (foreign-value "IL_LUMINANCE" ILenum))
(define IL_LUMINANCE_ALPHA (foreign-value "IL_LUMINANCE_ALPHA" ILenum))
(define IL_RGB (foreign-value "IL_RGB" ILenum))
(define IL_RGBA (foreign-value "IL_RGBA" ILenum))

;; types
(define IL_BYTE (foreign-value "IL_BYTE" ILenum))
(define IL_DOUBLE (foreign-value "IL_DOUBLE" ILenum))
(define IL_FLOAT (foreign-value "IL_FLOAT" ILenum))
(define IL_INT (foreign-value "IL_INT" ILenum))
(define IL_SHORT (foreign-value "IL_SHORT" ILenum))
(define IL_UNSIGNED_BYTE (foreign-value "IL_UNSIGNED_BYTE" ILenum))
(define IL_UNSIGNED_INT (foreign-value "IL_UNSIGNED_INT" ILenum))
(define IL_UNSIGNED_SHORT (foreign-value "IL_UNSIGNED_SHORT" ILenum))

;; get integer modes
(define IL_ACTIVE_IMAGE (foreign-value "IL_ACTIVE_IMAGE" ILenum))
(define IL_ACTIVE_LAYER (foreign-value "IL_ACTIVE_LAYER" ILenum))
(define IL_ACTIVE_MIPMAP (foreign-value "IL_ACTIVE_MIPMAP" ILenum))
(define IL_CONV_PAL (foreign-value "IL_CONV_PAL" ILenum))
(define IL_CUR_IMAGE (foreign-value "IL_CUR_IMAGE" ILenum))
(define IL_FILE_MODE (foreign-value "IL_FILE_MODE" ILenum))
(define IL_FORMAT_MODE (foreign-value "IL_FORMAT_MODE" ILenum))
(define IL_FORMAT_SET (foreign-value "IL_FORMAT_SET" ILenum))
(define IL_IMAGE_BITS_PER_PIXEL (foreign-value "IL_IMAGE_BITS_PER_PIXEL" ILenum))
(define IL_IMAGE_BYTES_PER_PIXEL (foreign-value "IL_IMAGE_BYTES_PER_PIXEL" ILenum))
(define IL_IMAGE_FORMAT (foreign-value "IL_IMAGE_FORMAT" ILenum))
(define IL_IMAGE_HEIGHT (foreign-value "IL_IMAGE_HEIGHT" ILenum))
(define IL_IMAGE_TYPE (foreign-value "IL_IMAGE_TYPE" ILenum))
(define IL_IMAGE_WIDTH (foreign-value "IL_IMAGE_WIDTH" ILenum))
(define IL_NUM_IMAGES (foreign-value "IL_NUM_IMAGES" ILenum))
(define IL_NUM_MIPMAPS (foreign-value "IL_NUM_MIPMAPS" ILenum))
(define IL_ORIGIN_MODE (foreign-value "IL_ORIGIN_MODE" ILenum))
(define IL_ORIGIN_SET (foreign-value "IL_ORIGIN_SET" ILenum))
(define IL_PALETTE_BPP (foreign-value "IL_PALETTE_BPP" ILenum))
(define IL_PALETTE_NUM_COLS (foreign-value "IL_PALETTE_NUM_COLS" ILenum))
(define IL_PALETTE_TYPE (foreign-value "IL_PALETTE_TYPE" ILenum))
(define IL_TYPE_MODE (foreign-value "IL_TYPE_MODE" ILenum))
(define IL_TYPE_SET (foreign-value "IL_TYPE_SET" ILenum))
(define IL_USE_KEY_COLOUR (foreign-value "IL_USE_KEY_COLOUR" ILenum))
(define IL_VERSION_NUM (foreign-value "IL_VERSION_NUM" ILenum))


(define ilInit (foreign-lambda void ilInit))
(define ilShutDown (foreign-lambda void ilShutDown))

(define ilGetError (foreign-lambda ILenum ilGetError))
(define iluErrorString (foreign-lambda c-string iluErrorString ILenum))

(define ilGenImages (foreign-lambda void ilGenImages size_t u32vector))
(define ilBindImage (foreign-lambda void ilBindImage unsigned-int))
(define ilDeleteImages (foreign-lambda void ilDeleteImages size_t u32vector))

(define ilLoadImage (foreign-lambda bool ilLoadImage c-string))

(define ilConvertImage (foreign-lambda bool ilConvertImage ILenum ILenum))
(define ilGetData (foreign-lambda c-pointer ilGetData))
(define ilGetInteger (foreign-lambda int ilGetInteger ILenum))

)
