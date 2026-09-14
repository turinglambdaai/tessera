#lang racket/base

;; OpenGL entry-point loader for tessera.
;;
;; tessera needs OpenGL 3.3 core features only, and loads every entry point
;; at runtime after the window backend has created a context. The loader
;; itself is platform-dispatched (set once per process by
;; tessera/platform.rkt):
;;
;;   macOS            dlsym on OpenGL.framework (contexts come from CGL,
;;                    which GLFW's proc lookup cannot serve in NO_API mode)
;;   Linux / Windows  glfwGetProcAddress (context created by GLFW)
;;
;; `defgl` declares one lazily-bound function: resolution happens on first
;; call, after the context is current, and is cached.

(require ffi/unsafe
         (for-syntax racket/base
                     syntax/parse))

(provide gl-loader         ; parameter: (string?) -> (or/c cpointer procedure? #f)
         gl-set-loader!
         defgl
         gl-str
         (all-defined-out))

;; ---- loader -----------------------------------------------------------------

(define gl-loader (make-parameter (λ (name) #f)))

(define (gl-set-loader! lookup) (gl-loader lookup))

(define (gl-str p)
  (and p (cast p _pointer _string/utf-8)))

;; ---- declaration macro ------------------------------------------------------

;; (defgl glClear (_fun _uint -> _void))
(define-syntax (defgl stx)
  (syntax-parse stx
    [(_ name:id ty)
     (with-syntax ([name-str (symbol->string (syntax-e #'name))])
       #'(define name
           (let ([f #f])
             (λ args
               (unless f
                 (define p ((gl-loader) name-str))
                 (unless p
                   (error 'tessera/gl
                          "OpenGL entry point not available: ~a\n  (was a context created before rendering?)"
                          name-str))
                 (set! f (cast p _fpointer ty)))
               (apply f args)))))]))

;; ---- OpenGL 3.3 core surface used by tessera's renderer ---------------------

(defgl glGetString         (_fun _uint -> _pointer))
(defgl glGetError          (_fun -> _uint))
(defgl glViewport          (_fun _int _int _int _int -> _void))
(defgl glClearColor        (_fun _float _float _float _float -> _void))
(defgl glClear             (_fun _uint -> _void))
(defgl glReadPixels        (_fun _int _int _int _int _uint _uint _pointer -> _void))
(defgl glFinish            (_fun -> _void))
(defgl glOrtho             (_fun _double _double _double _double _double _double -> _void))
(defgl glMatrixMode        (_fun _uint -> _void))
(defgl glLoadIdentity      (_fun -> _void))
(defgl glTexEnvi           (_fun _uint _uint _int -> _void))

(defgl glEnable            (_fun _uint -> _void))
(defgl glDisable           (_fun _uint -> _void))
(defgl glBlendFunc         (_fun _uint _uint -> _void))
(defgl glScissor           (_fun _int _int _int _int -> _void))
(defgl glPixelStorei       (_fun _uint _int -> _void))

;; shaders / program
(defgl glCreateShader      (_fun _uint -> _uint))
(defgl glShaderSource      (_fun _uint _int _pointer _pointer -> _void))
(defgl glCompileShader     (_fun _uint -> _void))
(defgl glGetShaderiv       (_fun _uint _uint _pointer -> _void))
(defgl glGetShaderInfoLog  (_fun _uint _int _pointer _pointer -> _void))
(defgl glDeleteShader      (_fun _uint -> _void))
(defgl glCreateProgram     (_fun -> _uint))
(defgl glAttachShader      (_fun _uint _uint -> _void))
(defgl glLinkProgram       (_fun _uint -> _void))
(defgl glGetProgramiv      (_fun _uint _uint _pointer -> _void))
(defgl glGetProgramInfoLog (_fun _uint _int _pointer _pointer -> _void))
(defgl glDeleteProgram     (_fun _uint -> _void))
(defgl glUseProgram        (_fun _uint -> _void))
(defgl glGetUniformLocation (_fun _uint _string/utf-8 -> _int))
(defgl glUniform1f         (_fun _int _float -> _void))
(defgl glUniform2f         (_fun _int _float _float -> _void))
(defgl glUniform1i         (_fun _int _int -> _void))

;; client-side vertex arrays (legacy pipeline — works on every driver)
(defgl glVertexPointer     (_fun _int _uint _int _pointer -> _void))
(defgl glTexCoordPointer   (_fun _int _uint _int _pointer -> _void))
(defgl glColorPointer      (_fun _int _uint _int _pointer -> _void))
(defgl glEnableClientState (_fun _uint -> _void))
(defgl glDisableClientState (_fun _uint -> _void))
(defgl glBindAttribLocation (_fun _uint _uint _string/utf-8 -> _void))
(defgl glEnableVertexAttribArray (_fun _uint -> _void))
(defgl glDisableVertexAttribArray (_fun _uint -> _void))
(defgl glVertexAttribPointer (_fun _uint _int _uint _uint _int _pointer -> _void))

;; draw
(defgl glDrawArrays       (_fun _uint _int _int -> _void))
(defgl glDrawElements      (_fun _uint _int _uint _pointer -> _void))

;; textures
(defgl glGenTextures       (_fun _int _pointer -> _void))
(defgl glDeleteTextures    (_fun _int _pointer -> _void))
(defgl glBindTexture       (_fun _uint _uint -> _void))
(defgl glTexImage2D        (_fun _uint _int _int _int _int _int _uint _uint _pointer -> _void))
(defgl glTexSubImage2D     (_fun _uint _int _int _int _int _int _uint _uint _pointer -> _void))
(defgl glTexParameteri     (_fun _uint _uint _int -> _void))
(defgl glActiveTexture     (_fun _uint -> _void))
(defgl glClientActiveTexture (_fun _uint -> _void))

;; ---- constants (values from gl3.h) ------------------------------------------

(define GL_VERSION  #x1F02)
(define GL_VENDOR   #x1F00)
(define GL_RENDERER #x1F01)

(define GL_NO_ERROR 0)

(define GL_COLOR_BUFFER_BIT #x4000)
(define GL_BLEND #x0BE2)
(define GL_SCISSOR_TEST #x0C11)

(define GL_ZERO 0)
(define GL_ONE 1)
(define GL_SRC_ALPHA #x0302)
(define GL_ONE_MINUS_SRC_ALPHA #x0303)

(define GL_UNSIGNED_BYTE #x1401)
(define GL_FLOAT #x1406)
(define GL_INT #x1404)

(define GL_RGBA  #x1908)
(define GL_RGB   #x1907)
(define GL_RED   #x1903)
(define GL_LUMINANCE #x1909)
(define GL_RGBA8 #x8058)
(define GL_R8    #x8229)
(define GL_TEXTURE_2D #x0DE1)
(define GL_TEXTURE0 #x84C0)
(define GL_TEXTURE1 #x84C1)
(define GL_TEXTURE2 #x84C2)
(define GL_TEXTURE_MIN_FILTER #x2801)
(define GL_TEXTURE_MAG_FILTER #x2800)
(define GL_LINEAR #x2601)
(define GL_NEAREST #x2600)
(define GL_CLAMP_TO_EDGE #x812F)
(define GL_UNPACK_ALIGNMENT #x0CF5)
(define GL_TEXTURE_WRAP_S #x2802)
(define GL_TEXTURE_WRAP_T #x2803)

(define GL_ARRAY_BUFFER #x8892)
(define GL_ELEMENT_ARRAY_BUFFER #x8893)
(define GL_STATIC_DRAW #x88E4)
(define GL_DYNAMIC_DRAW #x88E8)
(define GL_STREAM_DRAW #x88E0)

(define GL_VERTEX_ARRAY #x8074)
(define GL_COLOR_ARRAY #x8076)
(define GL_TEXTURE_COORD_ARRAY #x8078)

(define GL_VERTEX_SHADER #x8B31)
(define GL_FRAGMENT_SHADER #x8B20)
(define GL_COMPILE_STATUS #x8B81)
(define GL_LINK_STATUS #x8B82)
(define GL_TRUE 1)
(define GL_FALSE 0)

(define GL_TRIANGLES #x0004)
(define GL_MULTISAMPLE #x809D)
(define GL_PROJECTION #x1701)
(define GL_MODELVIEW #x1700)
(define GL_TEXTURE_ENV #x2300)
(define GL_TEXTURE_ENV_MODE #x2200)
(define GL_MODULATE #x2100)
(define GL_ALPHA #x1906)
(define GL_UNSIGNED_SHORT #x1403)
(define GL_UNSIGNED_INT #x1405)

(define GL_BGRA #x80E1)
