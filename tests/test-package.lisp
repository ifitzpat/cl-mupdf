;;;; tests/test-package.lisp --- Test package definition for cl-mupdf

(defpackage #:cl-mupdf-tests
  (:use #:cl #:cl-mupdf #:fiveam)
  (:documentation "Test suite for cl-mupdf"))

(in-package #:cl-mupdf-tests)

;;; Define test suites

(def-suite :cl-mupdf
    :description "Master test suite for cl-mupdf")

(def-suite :geometry
    :in :cl-mupdf
    :description "Rect/matrix primitives (no foreign calls)")

(def-suite :package-shape
    :in :cl-mupdf
    :description "Symbols, exports, conditions and constant values")

(def-suite :ffi-loading
    :in :cl-mupdf
    :description "Library loading; skipped if libmupdf is not available")

(def-suite :context
    :in :cl-mupdf
    :description "Context creation and disposal (requires libmupdf)")

(def-suite :document
    :in :cl-mupdf
    :description "Document open/close/page operations (requires libmupdf and a sample PDF)")

(def-suite :redaction
    :in :cl-mupdf
    :description "Content-stream redaction round-trip (requires libmupdf and a sample PDF)")

(def-suite :matrix-algebra
    :in :cl-mupdf
    :description "Pure-Lisp matrix inversion and transformation helpers")

(def-suite :extraction
    :in :cl-mupdf
    :description "Text extraction and search (requires libmupdf and a sample PDF)")
