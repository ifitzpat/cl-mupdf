;;;; tests/cl-mupdf-tests.lisp --- Test bodies for cl-mupdf
;;;;
;;;; The pure-Lisp suites (:GEOMETRY, :PACKAGE-SHAPE) always run.
;;;; The suites that need libmupdf to be loadable (:FFI-LOADING, :CONTEXT,
;;;; :DOCUMENT, :REDACTION) skip themselves cleanly if cl-mupdf:LOAD-MUPDF
;;;; signals an error.

(in-package #:cl-mupdf-tests)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(defun mupdf-available-p ()
  "Try to load libmupdf.  Return T on success, NIL on failure."
  (handler-case
      (progn (cl-mupdf:load-mupdf) t)
    (error () nil)))

(defun sample-pdf-pathname ()
  "Locate an optional sample PDF for the integration tests.  Users
who want the redaction round-trip to actually run can drop a
'sample.pdf' file into tests/ or set CL_MUPDF_SAMPLE_PDF in the env."
  (or (uiop:getenv "CL_MUPDF_SAMPLE_PDF")
      (let ((here (asdf:system-relative-pathname :cl-mupdf "tests/sample.pdf")))
        (when (probe-file here) here))))

;;; ============================================================================
;;; :GEOMETRY -- pure Lisp, no foreign calls
;;; ============================================================================

(in-suite :geometry)

(test rect-construction
  "MAKE-RECT and accessors round-trip cleanly."
  (let ((r (cl-mupdf:make-rect :x0 1.0 :y0 2.0 :x1 3.0 :y1 4.0)))
    (is (= 1.0 (cl-mupdf:rect-x0 r)))
    (is (= 2.0 (cl-mupdf:rect-y0 r)))
    (is (= 3.0 (cl-mupdf:rect-x1 r)))
    (is (= 4.0 (cl-mupdf:rect-y1 r)))
    (is (= 2.0 (cl-mupdf:rect-width r)))
    (is (= 2.0 (cl-mupdf:rect-height r)))))

(test rect-empty
  (is-true  (cl-mupdf:rect-empty-p (cl-mupdf:make-rect :x0 5 :y0 5 :x1 5 :y1 5)))
  (is-true  (cl-mupdf:rect-empty-p (cl-mupdf:make-rect :x0 6 :y0 0 :x1 5 :y1 1)))
  (is-false (cl-mupdf:rect-empty-p (cl-mupdf:make-rect :x0 0 :y0 0 :x1 1 :y1 1))))

(test rect-union/intersect
  (let ((a (cl-mupdf:make-rect :x0 0 :y0 0 :x1 10 :y1 10))
        (b (cl-mupdf:make-rect :x0 5 :y0 5 :x1 15 :y1 15)))
    (let ((u (cl-mupdf:rect-union a b)))
      (is (= 0  (cl-mupdf:rect-x0 u)))
      (is (= 0  (cl-mupdf:rect-y0 u)))
      (is (= 15 (cl-mupdf:rect-x1 u)))
      (is (= 15 (cl-mupdf:rect-y1 u))))
    (let ((i (cl-mupdf:rect-intersect a b)))
      (is (= 5  (cl-mupdf:rect-x0 i)))
      (is (= 5  (cl-mupdf:rect-y0 i)))
      (is (= 10 (cl-mupdf:rect-x1 i)))
      (is (= 10 (cl-mupdf:rect-y1 i))))))

(test matrix-builders
  (let ((id (cl-mupdf:identity-matrix)))
    (is (= 1.0 (cl-mupdf:matrix-a id)))
    (is (= 0.0 (cl-mupdf:matrix-b id)))
    (is (= 0.0 (cl-mupdf:matrix-c id)))
    (is (= 1.0 (cl-mupdf:matrix-d id)))
    (is (= 0.0 (cl-mupdf:matrix-e id)))
    (is (= 0.0 (cl-mupdf:matrix-f id))))
  (let ((s (cl-mupdf:scale-matrix 2.5)))
    (is (= 2.5 (cl-mupdf:matrix-a s)))
    (is (= 2.5 (cl-mupdf:matrix-d s))))
  (let ((s (cl-mupdf:scale-matrix 2.0 3.0)))
    (is (= 2.0 (cl-mupdf:matrix-a s)))
    (is (= 3.0 (cl-mupdf:matrix-d s))))
  (let ((tr (cl-mupdf:translate-matrix 7 11)))
    (is (= 7.0 (cl-mupdf:matrix-e tr)))
    (is (= 11.0 (cl-mupdf:matrix-f tr)))))

(test redact-options-defaults
  (let ((o (cl-mupdf:make-redact-options)))
    (is-true (cl-mupdf:redact-options-black-boxes o))
    (is (= cl-mupdf:+redact-image-pixels+
           (cl-mupdf:redact-options-image-method o)))
    (is (= cl-mupdf:+redact-line-art-remove-if-touched+
           (cl-mupdf:redact-options-line-art o)))
    (is (= cl-mupdf:+redact-text-remove+
           (cl-mupdf:redact-options-text o)))))

;;; ============================================================================
;;; :PACKAGE-SHAPE -- exports and constants
;;; ============================================================================

(in-suite :package-shape)

(test exports-exist
  "Every documented public symbol must be present and exported."
  (dolist (name '("MAKE-CONTEXT" "DROP-CONTEXT" "WITH-CONTEXT"
                  "OPEN-DOCUMENT" "DROP-DOCUMENT" "WITH-DOCUMENT"
                  "COUNT-PAGES" "LOAD-PAGE" "DROP-PAGE" "WITH-PAGE"
                  "BOUND-PAGE" "RENDER-PAGE-TO-PIXMAP"
                  "SAVE-PAGE-AS-PNG" "SAVE-PIXMAP-AS-PNG"
                  "CREATE-ANNOTATION" "ADD-REDACTION"
                  "REDACT-PAGE" "REDACT-DOCUMENT" "REDACT-PDF-FILE"
                  "MAKE-REDACT-OPTIONS"))
    (let ((sym (find-symbol name :cl-mupdf)))
      (is (and sym
               (eq :external (nth-value 1 (find-symbol name :cl-mupdf))))
          "symbol ~A is not exported from CL-MUPDF" name))))

(test annot-redact-constant
  "PDF_ANNOT_REDACT must equal 12 (offset in mupdf's pdf_annot_type)."
  (is (= 12 cl-mupdf:+annot-redact+)))

(test redact-image-method-constants
  (is (= 0 cl-mupdf:+redact-image-none+))
  (is (= 1 cl-mupdf:+redact-image-remove+))
  (is (= 2 cl-mupdf:+redact-image-pixels+))
  (is (= 3 cl-mupdf:+redact-image-unless-invisible+)))

(test mupdf-error-condition-shape
  (let ((c (make-condition 'cl-mupdf:mupdf-error :message "boom")))
    (is (typep c 'error))
    (is (string= "boom" (cl-mupdf:mupdf-error-message c)))))

;;; ============================================================================
;;; :FFI-LOADING -- runtime loading of libmupdf
;;; ============================================================================

(in-suite :ffi-loading)

(test load-mupdf
  "LOAD-MUPDF either succeeds or signals MUPDF-LIBRARY-NOT-FOUND."
  (handler-case
      (progn (cl-mupdf:load-mupdf)
             (pass "libmupdf loaded"))
    (cl-mupdf:mupdf-library-not-found (e)
      (skip "libmupdf is not installed: ~A"
            (cl-mupdf:mupdf-error-message e)))))

;;; ============================================================================
;;; :CONTEXT -- requires libmupdf
;;; ============================================================================

(in-suite :context)

(test context-roundtrip
  (unless (mupdf-available-p)
    (skip "libmupdf not available; skipping context tests"))
  (let ((ctx (cl-mupdf:make-context)))
    (is (cl-mupdf:contextp ctx))
    (is-false (cffi:null-pointer-p (cl-mupdf:context-pointer ctx)))
    (cl-mupdf:drop-context ctx)
    (is (cffi:null-pointer-p (cl-mupdf:context-pointer ctx)))))

(test with-context-binds-default
  (unless (mupdf-available-p)
    (skip "libmupdf not available; skipping context tests"))
  (cl-mupdf:with-context (ctx)
    (is (eq ctx cl-mupdf:*default-context*))))

;;; ============================================================================
;;; :DOCUMENT -- requires libmupdf and a sample PDF
;;; ============================================================================

(in-suite :document)

(test open-and-count
  (let ((sample (sample-pdf-pathname)))
    (unless (and (mupdf-available-p) sample)
      (skip "libmupdf or sample.pdf not available; skipping document tests"))
    (cl-mupdf:with-context (ctx)
      (declare (ignore ctx))
      (cl-mupdf:with-document (doc sample)
        (is (cl-mupdf:documentp doc))
        (is (>= (cl-mupdf:count-pages doc) 1))
        (cl-mupdf:with-page (p doc 0)
          (let ((box (cl-mupdf:bound-page p)))
            (is (typep box 'cl-mupdf:rect))
            (is (> (cl-mupdf:rect-width box) 0))
            (is (> (cl-mupdf:rect-height box) 0))))))))

;;; ============================================================================
;;; :REDACTION -- the headline feature
;;; ============================================================================

(in-suite :redaction)

(test redact-pdf-file-roundtrip
  (let ((sample (sample-pdf-pathname)))
    (unless (and (mupdf-available-p) sample)
      (skip "libmupdf or sample.pdf not available; skipping redaction tests"))
    (let ((out (asdf:system-relative-pathname :cl-mupdf "tests/redacted.pdf")))
      (when (probe-file out) (delete-file out))
      (cl-mupdf:redact-pdf-file
       sample out
       :marks '((0 (50.0 50.0 250.0 80.0))))
      (is (probe-file out))
      ;; Reopen the result and confirm it still has at least one page.
      (cl-mupdf:with-context (ctx)
        (declare (ignore ctx))
        (cl-mupdf:with-document (doc out)
          (is (>= (cl-mupdf:count-pages doc) 1)))))))

;;; ============================================================================
;;; :MATRIX-ALGEBRA -- pure Lisp, no foreign calls
;;; ============================================================================

(in-suite :matrix-algebra)

(defun matrix-approximately-equal (a b &optional (tolerance 1e-5))
  (every (lambda (slot)
           (< (abs (- (funcall slot a) (funcall slot b))) tolerance))
         (list #'cl-mupdf:matrix-a #'cl-mupdf:matrix-b
               #'cl-mupdf:matrix-c #'cl-mupdf:matrix-d
               #'cl-mupdf:matrix-e #'cl-mupdf:matrix-f)))

(test matrix-determinant-identity
  (is (= 1.0 (cl-mupdf:matrix-determinant (cl-mupdf:identity-matrix)))))

(test matrix-determinant-scale
  (is (= 6.0 (cl-mupdf:matrix-determinant (cl-mupdf:scale-matrix 2.0 3.0)))))

(test invert-identity
  (is (matrix-approximately-equal
       (cl-mupdf:identity-matrix)
       (cl-mupdf:invert-matrix (cl-mupdf:identity-matrix)))))

(test invert-scale
  (let* ((m (cl-mupdf:scale-matrix 4.0))
         (i (cl-mupdf:invert-matrix m)))
    (is (= 0.25 (cl-mupdf:matrix-a i)))
    (is (= 0.25 (cl-mupdf:matrix-d i)))))

(test invert-translate
  (let* ((m (cl-mupdf:translate-matrix 10 20))
         (i (cl-mupdf:invert-matrix m)))
    (is (= -10.0 (cl-mupdf:matrix-e i)))
    (is (= -20.0 (cl-mupdf:matrix-f i)))))

(test invert-singular-signals
  (let ((m (cl-mupdf:make-matrix :a 0 :b 0 :c 0 :d 0)))
    (signals cl-mupdf:mupdf-error
      (cl-mupdf:invert-matrix m))))

(test transform-point-translate
  (multiple-value-bind (x y)
      (cl-mupdf:transform-point 3.0 4.0 (cl-mupdf:translate-matrix 10 20))
    (is (= 13.0 x))
    (is (= 24.0 y))))

(test transform-point-scale
  (multiple-value-bind (x y)
      (cl-mupdf:transform-point 3.0 4.0 (cl-mupdf:scale-matrix 2.0))
    (is (= 6.0 x))
    (is (= 8.0 y))))

(test transform-rect-scale
  (let* ((r (cl-mupdf:make-rect :x0 10 :y0 20 :x1 30 :y1 40))
         (m (cl-mupdf:scale-matrix 2.0))
         (out (cl-mupdf:transform-rect r m)))
    (is (= 20.0 (cl-mupdf:rect-x0 out)))
    (is (= 40.0 (cl-mupdf:rect-y0 out)))
    (is (= 60.0 (cl-mupdf:rect-x1 out)))
    (is (= 80.0 (cl-mupdf:rect-y1 out)))))

(test pixel-to-userspace-roundtrip
  "The headline use case for the matrix helpers: a pixel-space
detection box (e.g. from YOLO on a 2x-rendered page) is mapped back
to PDF user-space by inverting the rendering matrix.  Round-tripping
must give back the original user-space rect."
  (let* ((user-rect (cl-mupdf:make-rect :x0 100 :y0 200 :x1 300 :y1 250))
         (render-m  (cl-mupdf:scale-matrix 2.0))
         ;; Forward: user-space -> pixel-space (what render-page-to-pixmap does)
         (pixel-rect (cl-mupdf:transform-rect user-rect render-m))
         ;; Inverse: pixel-space -> user-space (what we want for YOLO output)
         (back (cl-mupdf:transform-rect pixel-rect
                                        (cl-mupdf:invert-matrix render-m))))
    (is (matrix-approximately-equal render-m render-m)) ; sanity
    (is (< (abs (- 100 (cl-mupdf:rect-x0 back))) 1e-4))
    (is (< (abs (- 200 (cl-mupdf:rect-y0 back))) 1e-4))
    (is (< (abs (- 300 (cl-mupdf:rect-x1 back))) 1e-4))
    (is (< (abs (- 250 (cl-mupdf:rect-y1 back))) 1e-4))))

;;; ============================================================================
;;; :EXTRACTION -- text extraction and search (requires libmupdf + sample PDF)
;;; ============================================================================

(in-suite :extraction)

(test extract-text-page
  (let ((sample (sample-pdf-pathname)))
    (unless (and (mupdf-available-p) sample)
      (skip "libmupdf or sample.pdf not available; skipping extraction tests"))
    (cl-mupdf:with-context (ctx)
      (declare (ignore ctx))
      (cl-mupdf:with-document (doc sample)
        (cl-mupdf:with-page (p doc 0)
          (let ((text (cl-mupdf:extract-text p)))
            (is (stringp text))
            (is (> (length text) 0))))))))

(test extract-text-document-page-separator
  (let ((sample (sample-pdf-pathname)))
    (unless (and (mupdf-available-p) sample)
      (skip "libmupdf or sample.pdf not available; skipping extraction tests"))
    (cl-mupdf:with-context (ctx)
      (declare (ignore ctx))
      (cl-mupdf:with-document (doc sample)
        (let ((text (cl-mupdf:extract-text doc)))
          (is (stringp text))
          (is (> (length text) 0))
          ;; Multi-page documents must contain at least one form-feed.
          (when (> (cl-mupdf:count-pages doc) 1)
            (is (find #\Page text))))))))

(test extract-html-page
  (let ((sample (sample-pdf-pathname)))
    (unless (and (mupdf-available-p) sample)
      (skip "libmupdf or sample.pdf not available; skipping extraction tests"))
    (cl-mupdf:with-context (ctx)
      (declare (ignore ctx))
      (cl-mupdf:with-document (doc sample)
        (cl-mupdf:with-page (p doc 0)
          (let ((html (cl-mupdf:extract-html p)))
            (is (stringp html))
            (is (> (length html) 0))
            ;; XHTML output from MuPDF starts with a div / span tree
            (is (search "<" html))))))))

(test search-page-returns-rects
  (let ((sample (sample-pdf-pathname)))
    (unless (and (mupdf-available-p) sample)
      (skip "libmupdf or sample.pdf not available; skipping extraction tests"))
    (cl-mupdf:with-context (ctx)
      (declare (ignore ctx))
      (cl-mupdf:with-document (doc sample)
        (cl-mupdf:with-page (p doc 0)
          ;; Pick the first short word that appears in the page text and
          ;; verify search-page finds it.  This adapts to whatever
          ;; sample PDF the user dropped into tests/.
          (let* ((text (cl-mupdf:extract-text p))
                 (word (first
                        (remove-if-not
                         (lambda (w) (and (>= (length w) 3) (<= (length w) 12)))
                         (uiop:split-string text :separator '(#\Space #\Newline #\Tab))))))
            (when word
              (let ((hits (cl-mupdf:search-page p word)))
                (is (listp hits))
                (is (>= (length hits) 1))
                (is (every (lambda (r) (typep r 'cl-mupdf:rect)) hits))))))))))
