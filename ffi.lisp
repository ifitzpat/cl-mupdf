;;;; ffi.lisp --- Low-level CFFI bindings to MuPDF
;;;;
;;;; This file defines the foreign types, structs, enums and functions
;;;; that cl-mupdf exposes from libmupdf.so.  All symbols here live in
;;;; the cl-mupdf package; the higher-level Lispy wrappers live in
;;;; cl-mupdf.lisp.
;;;;
;;;; Caveat about error handling: MuPDF reports errors through the
;;;; fz_try/fz_catch macros, which expand to setjmp/longjmp around the
;;;; caller's stack frame.  Those macros cannot be invoked from foreign
;;;; code, so we cannot catch a thrown MuPDF exception in Lisp.  Instead
;;;; we install an error callback (see cl-mupdf.lisp) that records the
;;;; most recent error message into a Lisp variable, and we treat NULL
;;;; return values as failure.  For belt-and-braces safety in long
;;;; running processes, link against a small C shim that wraps each call
;;;; in fz_try/fz_catch.

(in-package #:cl-mupdf)

;;; ----------------------------------------------------------------------------
;;; Conditions
;;; ----------------------------------------------------------------------------

(define-condition mupdf-error (error)
  ((message :initarg :message :reader mupdf-error-message))
  (:report (lambda (c stream)
             (format stream "MuPDF error: ~A"
                     (mupdf-error-message c)))))

(define-condition mupdf-library-not-found (mupdf-error)
  ()
  (:default-initargs
   :message "libmupdf shared library could not be loaded.  Set
cl-mupdf:*libmupdf-path* or install MuPDF system-wide."))

;;; ----------------------------------------------------------------------------
;;; Library loading
;;; ----------------------------------------------------------------------------

(defvar *libmupdf-path* nil
  "If non-NIL, an absolute path to libmupdf.so to load.  Otherwise the
default search path is used.")

(cffi:define-foreign-library libmupdf
  (:darwin (:or "libmupdf.dylib" "libmupdf.so"))
  (:unix   (:or "libmupdf.so"
                "libmupdf.so.26"
                "libmupdf.so.25"
                "libmupdf.so.24"
                "libmupdf.so.23"))
  (:windows "libmupdf.dll")
  (t (:default "libmupdf")))

(defun load-mupdf (&optional path)
  "Load libmupdf.  PATH, if supplied, is an absolute filename to load
in preference to the default search.  Signals MUPDF-LIBRARY-NOT-FOUND
on failure."
  (when path
    (setf *libmupdf-path* path))
  (handler-case
      (if *libmupdf-path*
          (cffi:load-foreign-library *libmupdf-path*)
          (cffi:load-foreign-library 'libmupdf))
    (cffi:load-foreign-library-error (e)
      (error 'mupdf-library-not-found
             :message (format nil "~A" e)))))

;;; ----------------------------------------------------------------------------
;;; Constants and version
;;; ----------------------------------------------------------------------------

(defvar *mupdf-version* "1.26.8"
  "MuPDF version string passed to fz_new_context_imp.  Must match the
version of the linked libmupdf.so.  Override before calling
MAKE-CONTEXT if you have a different MuPDF version installed.")

;; FZ_STORE_DEFAULT in source/fitz/store.c == 256 << 20
(defconstant +fz-store-default+ (ash 256 20)
  "Default size in bytes of MuPDF's resource cache (256 MiB).")

(defconstant +fz-store-unlimited+ 0)

;;; pdf_annot_type enum (include/mupdf/pdf/annot.h)
(defconstant +annot-text+           0)
(defconstant +annot-link+           1)
(defconstant +annot-free-text+      2)
(defconstant +annot-line+           3)
(defconstant +annot-square+         4)
(defconstant +annot-circle+         5)
(defconstant +annot-polygon+        6)
(defconstant +annot-poly-line+      7)
(defconstant +annot-highlight+      8)
(defconstant +annot-underline+      9)
(defconstant +annot-squiggly+      10)
(defconstant +annot-strike-out+    11)
(defconstant +annot-redact+        12)
(defconstant +annot-stamp+         13)

;;; pdf_redact_image_method enum
(defconstant +redact-image-none+               0)
(defconstant +redact-image-remove+             1)
(defconstant +redact-image-pixels+             2)
(defconstant +redact-image-unless-invisible+   3)

;;; pdf_redact_line_art enum
(defconstant +redact-line-art-none+              0)
(defconstant +redact-line-art-remove-if-covered+ 1)
(defconstant +redact-line-art-remove-if-touched+ 2)

;;; pdf_redact_text enum
(defconstant +redact-text-remove+ 0)
(defconstant +redact-text-none+   1)

;;; ----------------------------------------------------------------------------
;;; Foreign struct definitions
;;; ----------------------------------------------------------------------------

(cffi:defcstruct fz-rect-c
  (x0 :float)
  (y0 :float)
  (x1 :float)
  (y1 :float))

(cffi:defcstruct fz-matrix-c
  (a :float)
  (b :float)
  (c :float)
  (d :float)
  (e :float)
  (f :float))

(cffi:defcstruct pdf-redact-options-c
  (black-boxes  :int)
  (image-method :int)
  (line-art     :int)
  (text         :int))

;; fz_quad - 4 corner points (ul, ur, ll, lr); used by fz_search_page
;; and the structured-text API.
(cffi:defcstruct fz-quad-c
  (ul-x :float) (ul-y :float)
  (ur-x :float) (ur-y :float)
  (ll-x :float) (ll-y :float)
  (lr-x :float) (lr-y :float))

;; pdf_write_options has many fields; we expose only the no-options
;; case (NULL pointer) which causes MuPDF to use sane defaults.

;;; ----------------------------------------------------------------------------
;;; Foreign function bindings
;;; ----------------------------------------------------------------------------

;;; --- Context lifecycle ------------------------------------------------------

;; fz_context *fz_new_context_imp(const fz_alloc_context *alloc,
;;                                const fz_locks_context *locks,
;;                                size_t max_store,
;;                                const char *version);
(cffi:defcfun ("fz_new_context_imp" %fz-new-context-imp) :pointer
  (alloc      :pointer)
  (locks      :pointer)
  (max-store  :size)
  (version    :string))

;; void fz_drop_context(fz_context *ctx);
(cffi:defcfun ("fz_drop_context" %fz-drop-context) :void
  (ctx :pointer))

;; void fz_register_document_handlers(fz_context *ctx);
(cffi:defcfun ("fz_register_document_handlers" %fz-register-document-handlers)
    :void
  (ctx :pointer))

;; void fz_set_error_callback(fz_context *ctx,
;;                            void (*cb)(void *user, const char *msg),
;;                            void *user);
(cffi:defcfun ("fz_set_error_callback" %fz-set-error-callback) :void
  (ctx  :pointer)
  (cb   :pointer)
  (user :pointer))

;; void fz_set_warning_callback(...);
(cffi:defcfun ("fz_set_warning_callback" %fz-set-warning-callback) :void
  (ctx  :pointer)
  (cb   :pointer)
  (user :pointer))

;;; --- Document lifecycle -----------------------------------------------------

;; fz_document *fz_open_document(fz_context *ctx, const char *filename);
(cffi:defcfun ("fz_open_document" %fz-open-document) :pointer
  (ctx      :pointer)
  (filename :string))

;; void fz_drop_document(fz_context *ctx, fz_document *doc);
(cffi:defcfun ("fz_drop_document" %fz-drop-document) :void
  (ctx :pointer)
  (doc :pointer))

;; int fz_count_pages(fz_context *ctx, fz_document *doc);
(cffi:defcfun ("fz_count_pages" %fz-count-pages) :int
  (ctx :pointer)
  (doc :pointer))

;; int fz_needs_password(fz_context *ctx, fz_document *doc);
(cffi:defcfun ("fz_needs_password" %fz-needs-password) :int
  (ctx :pointer)
  (doc :pointer))

;; int fz_authenticate_password(fz_context *ctx, fz_document *doc,
;;                              const char *password);
(cffi:defcfun ("fz_authenticate_password" %fz-authenticate-password) :int
  (ctx      :pointer)
  (doc      :pointer)
  (password :string))

;;; --- Page lifecycle ---------------------------------------------------------

;; fz_page *fz_load_page(fz_context *ctx, fz_document *doc, int number);
(cffi:defcfun ("fz_load_page" %fz-load-page) :pointer
  (ctx    :pointer)
  (doc    :pointer)
  (number :int))

;; void fz_drop_page(fz_context *ctx, fz_page *page);
(cffi:defcfun ("fz_drop_page" %fz-drop-page) :void
  (ctx  :pointer)
  (page :pointer))

;; fz_rect fz_bound_page(fz_context *ctx, fz_page *page);
;; This returns a struct by value.  CFFI does support that on most
;; platforms via :struct.
(cffi:defcfun ("fz_bound_page" %fz-bound-page) (:struct fz-rect-c)
  (ctx  :pointer)
  (page :pointer))

;;; --- Rendering --------------------------------------------------------------

;; fz_colorspace *fz_device_rgb(fz_context *ctx);
(cffi:defcfun ("fz_device_rgb" %fz-device-rgb) :pointer
  (ctx :pointer))

;; fz_colorspace *fz_device_gray(fz_context *ctx);
(cffi:defcfun ("fz_device_gray" %fz-device-gray) :pointer
  (ctx :pointer))

;; fz_pixmap *fz_new_pixmap_from_page(fz_context *ctx, fz_page *page,
;;                                    fz_matrix ctm, fz_colorspace *cs,
;;                                    int alpha);
(cffi:defcfun ("fz_new_pixmap_from_page" %fz-new-pixmap-from-page) :pointer
  (ctx   :pointer)
  (page  :pointer)
  (ctm   (:struct fz-matrix-c))
  (cs    :pointer)
  (alpha :int))

;; void fz_drop_pixmap(fz_context *ctx, fz_pixmap *pix);
(cffi:defcfun ("fz_drop_pixmap" %fz-drop-pixmap) :void
  (ctx :pointer)
  (pix :pointer))

;; void fz_save_pixmap_as_png(fz_context *ctx, fz_pixmap *pix,
;;                            const char *filename);
(cffi:defcfun ("fz_save_pixmap_as_png" %fz-save-pixmap-as-png) :void
  (ctx      :pointer)
  (pix      :pointer)
  (filename :string))

;;; --- PDF specific -----------------------------------------------------------

;; pdf_document *pdf_document_from_fz_document(fz_context *ctx,
;;                                             fz_document *doc);
(cffi:defcfun ("pdf_document_from_fz_document" %pdf-document-from-fz-document)
    :pointer
  (ctx :pointer)
  (doc :pointer))

;; pdf_document *pdf_open_document(fz_context *ctx, const char *filename);
(cffi:defcfun ("pdf_open_document" %pdf-open-document) :pointer
  (ctx      :pointer)
  (filename :string))

;; pdf_page *pdf_page_from_fz_page(fz_context *ctx, fz_page *page);
(cffi:defcfun ("pdf_page_from_fz_page" %pdf-page-from-fz-page) :pointer
  (ctx  :pointer)
  (page :pointer))

;; pdf_page *pdf_load_page(fz_context *ctx, pdf_document *doc, int number);
(cffi:defcfun ("pdf_load_page" %pdf-load-page) :pointer
  (ctx    :pointer)
  (doc    :pointer)
  (number :int))

;; pdf_annot *pdf_create_annot(fz_context *ctx, pdf_page *page,
;;                             enum pdf_annot_type type);
(cffi:defcfun ("pdf_create_annot" %pdf-create-annot) :pointer
  (ctx  :pointer)
  (page :pointer)
  (type :int))

;; void pdf_set_annot_rect(fz_context *ctx, pdf_annot *annot, fz_rect rect);
(cffi:defcfun ("pdf_set_annot_rect" %pdf-set-annot-rect) :void
  (ctx   :pointer)
  (annot :pointer)
  (rect  (:struct fz-rect-c)))

;; int pdf_redact_page(fz_context *ctx, pdf_document *doc, pdf_page *page,
;;                     pdf_redact_options *opts);
(cffi:defcfun ("pdf_redact_page" %pdf-redact-page) :int
  (ctx  :pointer)
  (doc  :pointer)
  (page :pointer)
  (opts :pointer))

;; void pdf_save_document(fz_context *ctx, pdf_document *doc,
;;                        const char *filename, pdf_write_options *opts);
(cffi:defcfun ("pdf_save_document" %pdf-save-document) :void
  (ctx      :pointer)
  (doc      :pointer)
  (filename :string)
  (opts     :pointer))

;;; --- Structured text and text extraction ------------------------------------

;; fz_stext_page *fz_new_stext_page_from_page(fz_context *ctx,
;;                                            fz_page *page,
;;                                            const fz_stext_options *options);
;; Pass NULL for options to get the defaults.
(cffi:defcfun ("fz_new_stext_page_from_page" %fz-new-stext-page-from-page)
    :pointer
  (ctx     :pointer)
  (page    :pointer)
  (options :pointer))

;; void fz_drop_stext_page(fz_context *ctx, fz_stext_page *page);
(cffi:defcfun ("fz_drop_stext_page" %fz-drop-stext-page) :void
  (ctx   :pointer)
  (stext :pointer))

;; void fz_print_stext_page_as_text(fz_context *ctx, fz_output *out,
;;                                  fz_stext_page *page);
(cffi:defcfun ("fz_print_stext_page_as_text" %fz-print-stext-page-as-text) :void
  (ctx   :pointer)
  (out   :pointer)
  (stext :pointer))

;; void fz_print_stext_page_as_xhtml(fz_context *ctx, fz_output *out,
;;                                   fz_stext_page *page, int id);
(cffi:defcfun ("fz_print_stext_page_as_xhtml" %fz-print-stext-page-as-xhtml)
    :void
  (ctx   :pointer)
  (out   :pointer)
  (stext :pointer)
  (id    :int))

;;; --- Buffers and outputs (for capturing text into a Lisp string) ------------

;; fz_buffer *fz_new_buffer(fz_context *ctx, size_t capacity);
(cffi:defcfun ("fz_new_buffer" %fz-new-buffer) :pointer
  (ctx      :pointer)
  (capacity :size))

;; void fz_drop_buffer(fz_context *ctx, fz_buffer *buf);
(cffi:defcfun ("fz_drop_buffer" %fz-drop-buffer) :void
  (ctx :pointer)
  (buf :pointer))

;; const char *fz_string_from_buffer(fz_context *ctx, fz_buffer *buf);
;; Implicitly calls fz_terminate_buffer; the returned pointer is owned
;; by the buffer and remains valid until the buffer is dropped.
(cffi:defcfun ("fz_string_from_buffer" %fz-string-from-buffer) :string
  (ctx :pointer)
  (buf :pointer))

;; fz_output *fz_new_output_with_buffer(fz_context *ctx, fz_buffer *buf);
(cffi:defcfun ("fz_new_output_with_buffer" %fz-new-output-with-buffer) :pointer
  (ctx :pointer)
  (buf :pointer))

;; void fz_close_output(fz_context *ctx, fz_output *out);
(cffi:defcfun ("fz_close_output" %fz-close-output) :void
  (ctx :pointer)
  (out :pointer))

;; void fz_drop_output(fz_context *ctx, fz_output *out);
(cffi:defcfun ("fz_drop_output" %fz-drop-output) :void
  (ctx :pointer)
  (out :pointer))

;;; --- Searching --------------------------------------------------------------

;; int fz_search_page(fz_context *ctx, fz_page *page, const char *needle,
;;                    int *hit_mark, fz_quad *hit_bbox, int hit_max);
;; Returns the number of hits actually written to hit_bbox (up to hit_max).
(cffi:defcfun ("fz_search_page" %fz-search-page) :int
  (ctx      :pointer)
  (page     :pointer)
  (needle   :string)
  (hit-mark :pointer)
  (hit-bbox :pointer)
  (hit-max  :int))
