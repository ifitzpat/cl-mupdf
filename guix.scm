;;; guix.scm --- Guix package definition for cl-mupdf

;;; This file can be used to test cl-mupdf locally with Guix:
;;;
;;;   guix shell -D -f guix.scm          # Development environment
;;;   guix shell -f guix.scm -- sbcl     # SBCL with cl-mupdf loaded
;;;   guix build -f guix.scm             # Build the package
;;;
;;; Inside the shell, you can:
;;;   sbcl
;;;   * (asdf:load-system :cl-mupdf)
;;;   * (asdf:test-system :cl-mupdf)

(define-module (cl-mupdf)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages pdf))

(define vcs-file?
  ;; Return true if the given file is under version control.
  (or (git-predicate (current-source-directory))
      (const #t)))  ; not in a Git checkout

(define-public cl-mupdf
  (package
    (name "cl-mupdf")
    (version "0.1.0")
    (source
     (local-file "." "cl-mupdf-checkout"
                 #:recursive? #t
                 #:select? vcs-file?))
    (build-system asdf-build-system/sbcl)
    (arguments
     (list
      #:asd-systems ''("cl-mupdf")
      #:asd-test-systems ''("cl-mupdf/tests")
      #:tests? #t
      #:phases
      #~(modify-phases %standard-phases
          ;; cl-mupdf loads libmupdf.so via cffi:load-foreign-library at
          ;; runtime, so we make sure the loader can find the .so we
          ;; built against by patching the in-tree default search list.
          (add-after 'unpack 'patch-libmupdf-path
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((mupdf (assoc-ref inputs "mupdf")))
                (substitute* "ffi.lisp"
                  (("\"libmupdf\\.so\"" all)
                   (string-append "\"" mupdf "/lib/libmupdf.so\""))))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "sbcl" "--non-interactive"
                        "--eval" "(require :asdf)"
                        "--eval" "(push (truename \".\") asdf:*central-registry*)"
                        "--eval" "(asdf:load-system :cl-mupdf/tests)"
                        "--eval" "(unless (fiveam:run! :cl-mupdf)
                                     (sb-ext:exit :code 1))")))))))
    (native-inputs
     (list sbcl-fiveam))                ; Testing framework
    (inputs
     (list mupdf                        ; The library we wrap
           sbcl-cffi                    ; Foreign function interface
           sbcl-alexandria              ; Utilities library
           sbcl-trivial-garbage))       ; Portable finalizers
    (home-page "https://github.com/ifitzpat/cl-mupdf")
    (synopsis "Common Lisp CFFI bindings for MuPDF (PDF redaction & rendering)")
    (description
     "cl-mupdf is a Common Lisp CFFI wrapper around Artifex's MuPDF
library.  Its primary use case is content-stream level PDF redaction:
unlike libraries that simply paint black boxes over sensitive regions,
MuPDF's @code{pdf_redact_page} actually removes the underlying text
runs and image data from the PDF content stream, which is the only
correct way to redact a PDF.

Features:
@itemize
@item Open and inspect PDF (and other MuPDF-supported) documents
@item Enumerate, render and save pages as PNG
@item Place redaction annotations and apply them via pdf_redact_page
@item One-shot @code{redact-pdf-file} helper for the common case
@item Lisp wrappers around fz_context, fz_document, fz_page, fz_pixmap
      and pdf_annot, with finalizers via trivial-garbage
@item Lisp-side @code{rect} and @code{matrix} structs that marshal to
      MuPDF's fz_rect / fz_matrix
@end itemize

cl-mupdf calls libmupdf.so through CFFI; the version of MuPDF the
context expects is configurable via @code{*mupdf-version*}.")
    (license license:agpl3+)))

;; Return the package for use with 'guix shell -f guix.scm'
cl-mupdf
