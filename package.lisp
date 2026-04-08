;;;; package.lisp --- Package definition for cl-mupdf

(defpackage #:cl-mupdf
  (:use #:cl)
  (:documentation
   "Common Lisp CFFI bindings for MuPDF, Artifex's PDF/XPS parsing and
rendering engine.  The primary use case of cl-mupdf is content-stream
level redaction of PDF documents via pdf_redact_page, but the package
also exposes enough of the MuPDF API to open documents, enumerate
pages, place annotations and render pages to PNG.")

  ;; -----------------------------------------------------------------
  ;; Conditions
  ;; -----------------------------------------------------------------
  (:export #:mupdf-error
           #:mupdf-error-message
           #:mupdf-library-not-found)

  ;; -----------------------------------------------------------------
  ;; Library loading
  ;; -----------------------------------------------------------------
  (:export #:load-mupdf
           #:*libmupdf-path*
           #:mupdf-version)

  ;; -----------------------------------------------------------------
  ;; Opaque handle classes
  ;; -----------------------------------------------------------------
  (:export #:context
           #:context-pointer
           #:contextp
           #:document
           #:document-pointer
           #:documentp
           #:page
           #:page-pointer
           #:pagep
           #:pixmap
           #:pixmap-pointer
           #:pixmapp
           #:annotation
           #:annotation-pointer)

  ;; -----------------------------------------------------------------
  ;; Geometry primitives
  ;; -----------------------------------------------------------------
  (:export #:rect
           #:make-rect
           #:rect-x0
           #:rect-y0
           #:rect-x1
           #:rect-y1
           #:rect-width
           #:rect-height
           #:rect-union
           #:rect-intersect
           #:rect-empty-p
           #:matrix
           #:make-matrix
           #:matrix-a
           #:matrix-b
           #:matrix-c
           #:matrix-d
           #:matrix-e
           #:matrix-f
           #:identity-matrix
           #:scale-matrix
           #:translate-matrix
           #:rotate-matrix
           #:matrix-determinant
           #:invert-matrix
           #:transform-point
           #:transform-rect)

  ;; -----------------------------------------------------------------
  ;; Context management
  ;; -----------------------------------------------------------------
  (:export #:make-context
           #:drop-context
           #:with-context
           #:*default-context*
           #:register-document-handlers)

  ;; -----------------------------------------------------------------
  ;; Document operations
  ;; -----------------------------------------------------------------
  (:export #:open-document
           #:drop-document
           #:with-document
           #:count-pages
           #:document-needs-password-p
           #:authenticate-password
           #:save-document
           #:pdf-document-p)

  ;; -----------------------------------------------------------------
  ;; Page operations
  ;; -----------------------------------------------------------------
  (:export #:load-page
           #:drop-page
           #:with-page
           #:bound-page
           #:render-page-to-pixmap
           #:save-page-as-png
           #:save-pixmap-as-png
           #:drop-pixmap
           #:do-pages)

  ;; -----------------------------------------------------------------
  ;; Text extraction and search
  ;; -----------------------------------------------------------------
  (:export #:extract-text
           #:extract-html
           #:search-page
           #:search-document
           #:*search-max-hits*)

  ;; -----------------------------------------------------------------
  ;; Annotations
  ;; -----------------------------------------------------------------
  (:export #:create-annotation
           #:annotation-type
           #:set-annotation-rect
           #:add-redaction
           #:+annot-redact+
           #:+annot-highlight+
           #:+annot-underline+
           #:+annot-squiggly+
           #:+annot-strike-out+
           #:+annot-text+
           #:+annot-free-text+
           #:+annot-link+)

  ;; -----------------------------------------------------------------
  ;; Redaction (the headline feature)
  ;; -----------------------------------------------------------------
  (:export #:redact-page
           #:redact-document
           #:redact-pdf-file
           #:redact-options
           #:make-redact-options
           #:redact-options-black-boxes
           #:redact-options-image-method
           #:redact-options-line-art
           #:redact-options-text
           #:+redact-image-none+
           #:+redact-image-remove+
           #:+redact-image-pixels+
           #:+redact-image-unless-invisible+
           #:+redact-line-art-none+
           #:+redact-line-art-remove-if-covered+
           #:+redact-line-art-remove-if-touched+
           #:+redact-text-remove+
           #:+redact-text-none+))
