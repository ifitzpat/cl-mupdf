;;;; cl-mupdf.asd --- ASDF system definition for cl-mupdf


;;; ============================================================================
;;; Core CFFI bindings to MuPDF
;;; ============================================================================
;;;
;;; cl-mupdf provides Common Lisp CFFI bindings to Artifex's MuPDF
;;; (https://mupdf.com).  Its primary use case is content-stream level
;;; PDF redaction via pdf_redact_page, but it also exposes the basic
;;; document/page/render API of MuPDF.
;;;
;;; Usage:
;;;   (ql:quickload :cl-mupdf)
;;;   (cl-mupdf:redact-pdf-file "in.pdf" "out.pdf"
;;;                             :marks '((1 (100 100 200 120))))

(asdf:defsystem #:cl-mupdf
  :description "Common Lisp CFFI bindings for MuPDF (PDF redaction & rendering)"
  :author "Ian FitzPatrick <ian@ianfitzpatrick.eu>"
  :license "AGPLv3"
  :version "0.1.0"
  :homepage "https://github.com/ifitzpat/cl-mupdf"
  :bug-tracker "https://github.com/ifitzpat/cl-mupdf/issues"
  :source-control (:git "https://github.com/ifitzpat/cl-mupdf.git")

  :depends-on (#:cffi
               #:alexandria
               #:trivial-garbage)

  :components ((:file "package")
               (:file "ffi"      :depends-on ("package"))
               (:file "cl-mupdf" :depends-on ("package" "ffi")))

  :in-order-to ((test-op (test-op #:cl-mupdf/tests))))


;;; ============================================================================
;;; Test suite
;;; ============================================================================

(asdf:defsystem #:cl-mupdf/tests
  :description "Test suite for cl-mupdf"
  :author "Ian FitzPatrick <ian@ianfitzpatrick.eu>"
  :license "AGPLv3"

  :depends-on (#:cl-mupdf
               #:fiveam)

  :components ((:module "tests"
                :components ((:file "test-package")
                             (:file "cl-mupdf-tests"
                              :depends-on ("test-package")))))

  :perform (test-op (o c)
             (uiop:symbol-call :fiveam '#:run! :cl-mupdf)))
