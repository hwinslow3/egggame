;; this will be glew+opengl
(module (egggame glew) *

(import (scheme)
        (chicken base)
        (chicken syntax)
        (chicken module)
        (chicken foreign)
        (srfi-4))

(foreign-declare "#include <GL/glew.h>")

(define-foreign-type GLenum unsigned-int)

;; =============================================================================
;; buffers
;; =============================================================================

;; buffer types
(define GL_ARRAY_BUFFER (foreign-value "GL_ARRAY_BUFFER" GLenum))
(define GL_ATOMIC_COUNTER_BUFFER (foreign-value "GL_ATOMIC_COUNTER_BUFFER" GLenum))
(define GL_COPY_READ_BUFFER (foreign-value "GL_COPY_READ_BUFFER" GLenum))
(define GL_COPY_WRITE_BUFFER (foreign-value "GL_COPY_WRITE_BUFFER" GLenum))
(define GL_DISPATCH_INDIRECT_BUFFER (foreign-value "GL_DISPATCH_INDIRECT_BUFFER" GLenum))
(define GL_DRAW_INDIRECT_BUFFER (foreign-value "GL_DRAW_INDIRECT_BUFFER" GLenum))
(define GL_ELEMENT_ARRAY_BUFFER (foreign-value "GL_ELEMENT_ARRAY_BUFFER" GLenum))
(define GL_PIXEL_PACK_BUFFER (foreign-value "GL_PIXEL_PACK_BUFFER" GLenum))
(define GL_PIXEL_UNPACK_BUFFER (foreign-value "GL_PIXEL_UNPACK_BUFFER" GLenum))
(define GL_QUERY_BUFFER (foreign-value "GL_QUERY_BUFFER" GLenum))
(define GL_SHADER_STORAGE_BUFFER (foreign-value "GL_SHADER_STORAGE_BUFFER" GLenum))
(define GL_TEXTURE_BUFFER (foreign-value "GL_TEXTURE_BUFFER" GLenum))
(define GL_TRANSFORM_FEEDBACK_BUFFER (foreign-value "GL_TRANSFORM_FEEDBACK_BUFFER" GLenum))
(define GL_UNIFORM_BUFFER (foreign-value "GL_UNIFORM_BUFFER" GLenum))

;; buffer usages
(define GL_STREAM_DRAW (foreign-value "GL_STREAM_DRAW" GLenum))
(define GL_STREAM_READ (foreign-value "GL_STREAM_READ" GLenum))
(define GL_STREAM_COPY (foreign-value "GL_STREAM_COPY" GLenum))
(define GL_STATIC_DRAW (foreign-value "GL_STATIC_DRAW" GLenum))
(define GL_STATIC_READ (foreign-value "GL_STATIC_READ" GLenum))
(define GL_STATIC_COPY (foreign-value "GL_STATIC_COPY" GLenum))
(define GL_DYNAMIC_DRAW (foreign-value "GL_DYNAMIC_DRAW" GLenum))
(define GL_DYNAMIC_READ (foreign-value "GL_DYNAMIC_READ" GLenum))
(define GL_DYNAMIC_COPY (foreign-value "GL_DYNAMIC_COPY" GLenum))

;; funcs
(define glGenBuffers (foreign-lambda void glGenBuffers size_t u32vector))
(define glBindBuffer (foreign-lambda void glBindBuffer GLenum unsigned-int))
(define glBufferData (foreign-lambda void glBufferData GLenum unsigned-integer64 c-pointer GLenum))
(define glDeleteBuffers (foreign-lambda void glDeleteBuffers size_t u32vector))

;; =============================================================================
;; textures
;; =============================================================================

;; targets
(define GL_TEXTURE_1D (foreign-value "GL_TEXTURE_1D" GLenum))
(define GL_TEXTURE_1D_ARRAY (foreign-value "GL_TEXTURE_1D_ARRAY" GLenum))
(define GL_TEXTURE_2D (foreign-value "GL_TEXTURE_2D" GLenum))
(define GL_TEXTURE_2D_ARRAY (foreign-value "GL_TEXTURE_2D_ARRAY" GLenum))
(define GL_TEXTURE_2D_MULTISAMPLE (foreign-value "GL_TEXTURE_2D_MULTISAMPLE" GLenum))
(define GL_TEXTURE_2D_MULTISAMPLE_ARRAY (foreign-value "GL_TEXTURE_2D_MULTISAMPLE_ARRAY" GLenum))
(define GL_TEXTURE_3D (foreign-value "GL_TEXTURE_3D" GLenum))
(define GL_TEXTURE_CUBE_MAP (foreign-value "GL_TEXTURE_CUBE_MAP" GLenum))
(define GL_TEXTURE_CUBE_MAP_ARRAY (foreign-value "GL_TEXTURE_CUBE_MAP_ARRAY" GLenum))
(define GL_TEXTURE_RECTANGLE (foreign-value "GL_TEXTURE_RECTANGLE" GLenum))

;; pnames
(define GL_DEPTH_STENCIL_TEXTURE_MODE (foreign-value "GL_DEPTH_STENCIL_TEXTURE_MODE" GLenum))
(define GL_TEXTURE_BASE_LEVEL (foreign-value "GL_TEXTURE_BASE_LEVEL" GLenum))
(define GL_TEXTURE_COMPARE_FUNC (foreign-value "GL_TEXTURE_COMPARE_FUNC" GLenum))
(define GL_TEXTURE_COMPARE_MODE (foreign-value "GL_TEXTURE_COMPARE_MODE" GLenum))
(define GL_TEXTURE_LOD_BIAS (foreign-value "GL_TEXTURE_LOD_BIAS" GLenum))
(define GL_TEXTURE_MIN_FILTER (foreign-value "GL_TEXTURE_MIN_FILTER" GLenum))
(define GL_TEXTURE_MAG_FILTER (foreign-value "GL_TEXTURE_MAG_FILTER" GLenum))
(define GL_TEXTURE_MIN_LOD (foreign-value "GL_TEXTURE_MIN_LOD" GLenum))
(define GL_TEXTURE_MAX_LOD (foreign-value "GL_TEXTURE_MAX_LOD" GLenum))
(define GL_TEXTURE_MAX_LEVEL (foreign-value "GL_TEXTURE_MAX_LEVEL" GLenum))
(define GL_TEXTURE_SWIZZLE_R (foreign-value "GL_TEXTURE_SWIZZLE_R" GLenum))
(define GL_TEXTURE_SWIZZLE_G (foreign-value "GL_TEXTURE_SWIZZLE_G" GLenum))
(define GL_TEXTURE_SWIZZLE_B (foreign-value "GL_TEXTURE_SWIZZLE_B" GLenum))
(define GL_TEXTURE_SWIZZLE_A (foreign-value "GL_TEXTURE_SWIZZLE_A" GLenum))
(define GL_TEXTURE_WRAP_S (foreign-value "GL_TEXTURE_WRAP_S" GLenum))
(define GL_TEXTURE_WRAP_T (foreign-value "GL_TEXTURE_WRAP_T" GLenum))
(define GL_TEXTURE_WRAP_R (foreign-value "GL_TEXTURE_WRAP_R" GLenum))

;; funcs
(define glGenTextures (foreign-lambda void glGenTextures size_t u32vector))
(define glBindTexture (foreign-lambda void glBindTexture GLenum unsigned-int))
(define glTexParameteri (foreign-lambda void glTexParameteri GLenum GLenum int))
(define glTexImage2D
  (foreign-lambda void glTexImage2D
    GLenum int int size_t size_t int GLenum GLenum c-pointer))

)
