;;;; cl-mupdf.lisp --- High-level Lisp API on top of the FFI bindings
;;;;
;;;; This file provides:
;;;;
;;;;   * Lisp-side struct types for fz_rect, fz_matrix and pdf_redact_options
;;;;   * Wrapper classes for opaque MuPDF handles (context, document, page,
;;;;     pixmap, annotation) with finalizers via trivial-garbage
;;;;   * Convenience functions: open-document, count-pages, load-page,
;;;;     render-page-to-pixmap, save-pixmap-as-png
;;;;   * The headline redaction API: redact-page, redact-document,
;;;;     redact-pdf-file
;;;;   * with-* macros that bind/clean up resources

(in-package #:cl-mupdf)

;;; ============================================================================
;;; Geometry primitives - plain Lisp structs
;;; ============================================================================

(defstruct rect
  "Axis-aligned rectangle in PDF user space (points, 1/72 inch).
The y-axis grows upward.  Compatible with the layout of MuPDF's
fz_rect: (x0, y0) is the lower-left, (x1, y1) the upper-right."
  (x0 0.0 :type single-float)
  (y0 0.0 :type single-float)
  (x1 0.0 :type single-float)
  (y1 0.0 :type single-float))

(defun rect-width (r)
  (- (rect-x1 r) (rect-x0 r)))

(defun rect-height (r)
  (- (rect-y1 r) (rect-y0 r)))

(defun rect-empty-p (r)
  (or (>= (rect-x0 r) (rect-x1 r))
      (>= (rect-y0 r) (rect-y1 r))))

(defun rect-union (a b)
  (make-rect :x0 (min (rect-x0 a) (rect-x0 b))
             :y0 (min (rect-y0 a) (rect-y0 b))
             :x1 (max (rect-x1 a) (rect-x1 b))
             :y1 (max (rect-y1 a) (rect-y1 b))))

(defun rect-intersect (a b)
  (let ((x0 (max (rect-x0 a) (rect-x0 b)))
        (y0 (max (rect-y0 a) (rect-y0 b)))
        (x1 (min (rect-x1 a) (rect-x1 b)))
        (y1 (min (rect-y1 a) (rect-y1 b))))
    (make-rect :x0 x0 :y0 y0 :x1 x1 :y1 y1)))

(defstruct matrix
  "2x3 affine transform [a b ; c d ; e f] in MuPDF/PDF convention.
Maps user-space coordinates (x, y) to (a*x + c*y + e, b*x + d*y + f)."
  (a 1.0 :type single-float)
  (b 0.0 :type single-float)
  (c 0.0 :type single-float)
  (d 1.0 :type single-float)
  (e 0.0 :type single-float)
  (f 0.0 :type single-float))

(defun identity-matrix ()
  (make-matrix))

(defun scale-matrix (sx &optional (sy sx))
  (make-matrix :a (float sx 0.0) :d (float sy 0.0)))

(defun translate-matrix (tx ty)
  (make-matrix :e (float tx 0.0) :f (float ty 0.0)))

(defun rotate-matrix (degrees)
  (let* ((rad (* (float degrees 0.0) (/ (float pi 0.0) 180.0)))
         (s (sin rad))
         (c (cos rad)))
    (make-matrix :a c :b s :c (- s) :d c)))

;;; --- helpers to marshal Lisp structs <-> foreign struct values --------------
;;;
;;; CFFI represents pass-by-value foreign structs as Lisp plists whose
;;; keys are the slot names as symbols.  The helpers below convert
;;; between our public RECT/MATRIX structs and that representation.

(defun rect->plist (r)
  "Convert a Lisp RECT into a plist suitable for passing to a CFFI
function that takes (:struct fz-rect-c) by value."
  (list 'x0 (float (rect-x0 r) 0.0)
        'y0 (float (rect-y0 r) 0.0)
        'x1 (float (rect-x1 r) 0.0)
        'y1 (float (rect-y1 r) 0.0)))

(defun plist->rect (plist)
  (make-rect :x0 (float (getf plist 'x0) 0.0)
             :y0 (float (getf plist 'y0) 0.0)
             :x1 (float (getf plist 'x1) 0.0)
             :y1 (float (getf plist 'y1) 0.0)))

(defun matrix->plist (m)
  (list 'a (float (matrix-a m) 0.0)
        'b (float (matrix-b m) 0.0)
        'c (float (matrix-c m) 0.0)
        'd (float (matrix-d m) 0.0)
        'e (float (matrix-e m) 0.0)
        'f (float (matrix-f m) 0.0)))

(defun coerce-rect (designator)
  "Accept a RECT, a (x0 y0 x1 y1) list, or a #(x0 y0 x1 y1) vector."
  (etypecase designator
    (rect designator)
    (list (make-rect :x0 (float (first  designator) 0.0)
                     :y0 (float (second designator) 0.0)
                     :x1 (float (third  designator) 0.0)
                     :y1 (float (fourth designator) 0.0)))
    (vector (make-rect :x0 (float (aref designator 0) 0.0)
                       :y0 (float (aref designator 1) 0.0)
                       :x1 (float (aref designator 2) 0.0)
                       :y1 (float (aref designator 3) 0.0)))))

;;; ============================================================================
;;; Redact options
;;; ============================================================================

(defstruct redact-options
  "Lisp side mirror of MuPDF's pdf_redact_options.
BLACK-BOXES   - non-nil paints black boxes over redacted regions.
IMAGE-METHOD  - one of +redact-image-none+, +redact-image-remove+,
                +redact-image-pixels+, +redact-image-unless-invisible+.
LINE-ART      - one of the +redact-line-art-*+ constants.
TEXT          - one of +redact-text-remove+ or +redact-text-none+."
  (black-boxes  t)
  (image-method +redact-image-pixels+ :type integer)
  (line-art     +redact-line-art-remove-if-touched+ :type integer)
  (text         +redact-text-remove+ :type integer))

(defun redact-options->foreign (opts ptr)
  (setf (cffi:foreign-slot-value ptr '(:struct pdf-redact-options-c) 'black-boxes)
        (if (redact-options-black-boxes opts) 1 0))
  (setf (cffi:foreign-slot-value ptr '(:struct pdf-redact-options-c) 'image-method)
        (redact-options-image-method opts))
  (setf (cffi:foreign-slot-value ptr '(:struct pdf-redact-options-c) 'line-art)
        (redact-options-line-art opts))
  (setf (cffi:foreign-slot-value ptr '(:struct pdf-redact-options-c) 'text)
        (redact-options-text opts))
  ptr)

;;; ============================================================================
;;; Opaque handle classes
;;; ============================================================================

(defclass context ()
  ((pointer :initarg :pointer :reader context-pointer))
  (:documentation
   "Wraps a fz_context*.  Free with DROP-CONTEXT or wait for the
finalizer."))

(defun contextp (x) (typep x 'context))

(defclass document ()
  ((pointer :initarg :pointer :reader document-pointer)
   (context :initarg :context :reader document-context)
   (pdf-p   :initarg :pdf-p   :reader pdf-document-p))
  (:documentation
   "Wraps an fz_document*.  PDF-DOCUMENT-P is true if MuPDF identified
the document as a PDF (i.e. pdf_document_from_fz_document succeeded)."))

(defun documentp (x) (typep x 'document))

(defclass page ()
  ((pointer  :initarg :pointer  :reader page-pointer)
   (document :initarg :document :reader page-document)
   (number   :initarg :number   :reader page-number))
  (:documentation
   "Wraps an fz_page*.  PAGE-DOCUMENT is the parent DOCUMENT instance,
PAGE-NUMBER is the zero-based index used to load it."))

(defun pagep (x) (typep x 'page))

(defclass pixmap ()
  ((pointer :initarg :pointer :reader pixmap-pointer)
   (context :initarg :context :reader pixmap-context)))

(defun pixmapp (x) (typep x 'pixmap))

(defclass annotation ()
  ((pointer :initarg :pointer :reader annotation-pointer)
   (page    :initarg :page    :reader annotation-page)
   (type    :initarg :type    :reader annotation-type)))

;;; ============================================================================
;;; Error callback support
;;; ============================================================================

(defvar *last-mupdf-error* nil
  "Last error message reported by libmupdf via the error callback,
captured per-thread is left to the user.")

(cffi:defcallback mupdf-error-cb :void
    ((user :pointer) (msg :string))
  (declare (ignore user))
  (setf *last-mupdf-error* msg))

(cffi:defcallback mupdf-warning-cb :void
    ((user :pointer) (msg :string))
  (declare (ignore user))
  ;; Warnings are silently swallowed; users that care can rebind the
  ;; callback themselves.
  (declare (ignore msg)))

(defun install-error-callbacks (ctx-ptr)
  (%fz-set-error-callback   ctx-ptr (cffi:callback mupdf-error-cb)   (cffi:null-pointer))
  (%fz-set-warning-callback ctx-ptr (cffi:callback mupdf-warning-cb) (cffi:null-pointer)))

(defun maybe-error (form-name)
  "If *LAST-MUPDF-ERROR* was set during a foreign call, signal it as
a MUPDF-ERROR.  Should be called immediately after the foreign call."
  (let ((msg *last-mupdf-error*))
    (when msg
      (setf *last-mupdf-error* nil)
      (error 'mupdf-error
             :message (format nil "~A: ~A" form-name msg)))))

;;; ============================================================================
;;; Context management
;;; ============================================================================

(defvar *default-context* nil
  "When bound, MuPDF operations may default to using this context if
none is supplied.  Established by WITH-CONTEXT.")

(defun make-context (&key (max-store +fz-store-default+)
                          (version *mupdf-version*)
                          (register-handlers t))
  "Create a fresh MuPDF context.  Equivalent to fz_new_context plus,
optionally, fz_register_document_handlers (the default).

The returned CONTEXT object has a finalizer that will fz_drop_context
the underlying pointer when garbage collected; you can release it
explicitly with DROP-CONTEXT."
  (load-mupdf)
  (let ((ptr (%fz-new-context-imp (cffi:null-pointer)
                                  (cffi:null-pointer)
                                  max-store
                                  version)))
    (when (cffi:null-pointer-p ptr)
      (error 'mupdf-error
             :message
             (format nil "fz_new_context_imp returned NULL (version mismatch?  Expected ~A)"
                     version)))
    (install-error-callbacks ptr)
    (when register-handlers
      (%fz-register-document-handlers ptr))
    (let ((ctx (make-instance 'context :pointer ptr)))
      (trivial-garbage:finalize
       ctx
       (lambda ()
         (unless (cffi:null-pointer-p ptr)
           (%fz-drop-context ptr))))
      ctx)))

(defun drop-context (ctx)
  "Explicitly release the foreign fz_context backing CTX.  After this
call, CTX must not be used."
  (let ((ptr (context-pointer ctx)))
    (unless (cffi:null-pointer-p ptr)
      (%fz-drop-context ptr)
      (setf (slot-value ctx 'pointer) (cffi:null-pointer))
      (trivial-garbage:cancel-finalization ctx)))
  ctx)

(defmacro with-context ((var &rest options) &body body)
  "Bind VAR to a fresh CONTEXT for the duration of BODY, then drop it.
Also binds *DEFAULT-CONTEXT* so the high-level functions can be called
without an explicit context argument."
  `(let* ((,var (make-context ,@options))
          (*default-context* ,var))
     (unwind-protect (progn ,@body)
       (drop-context ,var))))

(defun register-document-handlers (&optional (ctx (default-context)))
  "Call fz_register_document_handlers on CTX.  MAKE-CONTEXT does this
automatically; only call this if you constructed a context with
:REGISTER-HANDLERS NIL."
  (%fz-register-document-handlers (context-pointer ctx)))

(defun default-context ()
  (or *default-context*
      (error "No MuPDF context is currently bound.  Wrap operations in WITH-CONTEXT or pass :context explicitly.")))

;;; ============================================================================
;;; Documents
;;; ============================================================================

(defun open-document (filename &key (context (default-context)))
  "Open a document from FILENAME.  Returns a DOCUMENT object.
The format is auto-detected from the file's contents and/or extension."
  (let* ((ctx-ptr (context-pointer context))
         (doc-ptr (%fz-open-document ctx-ptr (namestring filename))))
    (maybe-error 'open-document)
    (when (cffi:null-pointer-p doc-ptr)
      (error 'mupdf-error
             :message (format nil "fz_open_document failed for ~A" filename)))
    (let* ((pdf-ptr (%pdf-document-from-fz-document ctx-ptr doc-ptr))
           (pdf-p   (not (cffi:null-pointer-p pdf-ptr)))
           (doc (make-instance 'document
                               :pointer doc-ptr
                               :context context
                               :pdf-p   pdf-p)))
      (trivial-garbage:finalize
       doc
       (lambda ()
         (unless (cffi:null-pointer-p doc-ptr)
           (%fz-drop-document ctx-ptr doc-ptr))))
      doc)))

(defun drop-document (doc)
  "Release the underlying fz_document.  After this, DOC must not be used."
  (let ((ptr (document-pointer doc))
        (ctx (context-pointer (document-context doc))))
    (unless (cffi:null-pointer-p ptr)
      (%fz-drop-document ctx ptr)
      (setf (slot-value doc 'pointer) (cffi:null-pointer))
      (trivial-garbage:cancel-finalization doc)))
  doc)

(defmacro with-document ((var filename &rest open-args) &body body)
  `(let ((,var (open-document ,filename ,@open-args)))
     (unwind-protect (progn ,@body)
       (drop-document ,var))))

(defun count-pages (document)
  (%fz-count-pages (context-pointer (document-context document))
                   (document-pointer document)))

(defun document-needs-password-p (document)
  (not (zerop (%fz-needs-password (context-pointer (document-context document))
                                  (document-pointer document)))))

(defun authenticate-password (document password)
  (not (zerop (%fz-authenticate-password
               (context-pointer (document-context document))
               (document-pointer document)
               password))))

(defun save-document (document filename)
  "Save DOCUMENT (which must be a PDF) to FILENAME.  Uses MuPDF's
default write options."
  (unless (pdf-document-p document)
    (error 'mupdf-error
           :message "save-document: not a PDF document"))
  (%pdf-save-document (context-pointer (document-context document))
                      (document-pointer document)
                      (namestring filename)
                      (cffi:null-pointer))
  (maybe-error 'save-document)
  filename)

;;; ============================================================================
;;; Pages
;;; ============================================================================

(defun load-page (document number)
  "Load page NUMBER (zero-based) of DOCUMENT and return a PAGE object."
  (let* ((ctx-ptr (context-pointer (document-context document)))
         (page-ptr (%fz-load-page ctx-ptr (document-pointer document) number)))
    (maybe-error 'load-page)
    (when (cffi:null-pointer-p page-ptr)
      (error 'mupdf-error
             :message (format nil "fz_load_page failed (page ~A)" number)))
    (let ((p (make-instance 'page
                            :pointer  page-ptr
                            :document document
                            :number   number)))
      (trivial-garbage:finalize
       p
       (lambda ()
         (unless (cffi:null-pointer-p page-ptr)
           (%fz-drop-page ctx-ptr page-ptr))))
      p)))

(defun drop-page (page)
  (let ((ptr (page-pointer page))
        (ctx (context-pointer (document-context (page-document page)))))
    (unless (cffi:null-pointer-p ptr)
      (%fz-drop-page ctx ptr)
      (setf (slot-value page 'pointer) (cffi:null-pointer))
      (trivial-garbage:cancel-finalization page)))
  page)

(defmacro with-page ((var document number) &body body)
  `(let ((,var (load-page ,document ,number)))
     (unwind-protect (progn ,@body)
       (drop-page ,var))))

(defmacro do-pages ((var document &optional result) &body body)
  "Iterate over the pages of DOCUMENT, binding VAR to each in turn.
Each page is loaded and dropped within one iteration."
  (alexandria:with-gensyms (doc i count)
    `(let* ((,doc ,document)
            (,count (count-pages ,doc)))
       (dotimes (,i ,count ,result)
         (with-page (,var ,doc ,i)
           ,@body)))))

(defun bound-page (page)
  "Return a RECT giving the bounding box of PAGE in PDF user space."
  (plist->rect
   (%fz-bound-page
    (context-pointer (document-context (page-document page)))
    (page-pointer page))))

;;; ============================================================================
;;; Rendering
;;; ============================================================================

(defun render-page-to-pixmap (page &key (matrix (identity-matrix))
                                        (alpha nil))
  "Render PAGE to a fresh PIXMAP using the device-RGB colorspace.
MATRIX scales/rotates the output (default identity); pass
(SCALE-MATRIX 2.0) for 144 DPI rendering, etc."
  (let* ((ctx-ptr (context-pointer (document-context (page-document page))))
         (cs-ptr  (%fz-device-rgb ctx-ptr))
         (pix-ptr (%fz-new-pixmap-from-page
                   ctx-ptr
                   (page-pointer page)
                   (matrix->plist matrix)
                   cs-ptr
                   (if alpha 1 0))))
    (maybe-error 'render-page-to-pixmap)
    (when (cffi:null-pointer-p pix-ptr)
      (error 'mupdf-error
             :message "fz_new_pixmap_from_page returned NULL"))
    (let ((pix (make-instance 'pixmap
                              :pointer pix-ptr
                              :context (document-context (page-document page)))))
      (trivial-garbage:finalize
       pix
       (lambda ()
         (unless (cffi:null-pointer-p pix-ptr)
           (%fz-drop-pixmap ctx-ptr pix-ptr))))
      pix)))

(defun drop-pixmap (pixmap)
  (let ((ptr (pixmap-pointer pixmap))
        (ctx (context-pointer (pixmap-context pixmap))))
    (unless (cffi:null-pointer-p ptr)
      (%fz-drop-pixmap ctx ptr)
      (setf (slot-value pixmap 'pointer) (cffi:null-pointer))
      (trivial-garbage:cancel-finalization pixmap)))
  pixmap)

(defun save-pixmap-as-png (pixmap filename)
  (%fz-save-pixmap-as-png (context-pointer (pixmap-context pixmap))
                          (pixmap-pointer pixmap)
                          (namestring filename))
  (maybe-error 'save-pixmap-as-png)
  filename)

(defun save-page-as-png (page filename &key (matrix (identity-matrix)))
  "Convenience: render PAGE to a pixmap and save it as PNG to FILENAME."
  (let ((pix (render-page-to-pixmap page :matrix matrix)))
    (unwind-protect (save-pixmap-as-png pix filename)
      (drop-pixmap pix))))

;;; ============================================================================
;;; Annotations
;;; ============================================================================

(defun create-annotation (page type)
  "Create a new annotation of TYPE on PAGE.  PAGE must belong to a
PDF document.  TYPE is one of the +annot-*+ constants.  Returns an
ANNOTATION wrapper."
  (let* ((doc (page-document page)))
    (unless (pdf-document-p doc)
      (error 'mupdf-error
             :message "create-annotation: not a PDF document"))
    (let* ((ctx-ptr (context-pointer (document-context doc)))
           (pdf-pg  (%pdf-page-from-fz-page ctx-ptr (page-pointer page)))
           (annot   (%pdf-create-annot ctx-ptr pdf-pg type)))
      (maybe-error 'create-annotation)
      (when (cffi:null-pointer-p annot)
        (error 'mupdf-error
               :message "pdf_create_annot returned NULL"))
      (make-instance 'annotation
                     :pointer annot
                     :page    page
                     :type    type))))

(defun set-annotation-rect (annotation rect)
  "Set the bounding rectangle of ANNOTATION to RECT (a Lisp RECT
struct, list, or vector)."
  (let* ((page    (annotation-page annotation))
         (ctx-ptr (context-pointer (document-context (page-document page))))
         (r       (coerce-rect rect)))
    (%pdf-set-annot-rect ctx-ptr
                         (annotation-pointer annotation)
                         (rect->plist r))
    (maybe-error 'set-annotation-rect))
  annotation)

(defun add-redaction (page rect)
  "Place a redaction annotation on PAGE covering RECT.  This only
*marks* the area; call REDACT-PAGE (or REDACT-DOCUMENT) afterwards to
actually strip the underlying content."
  (let ((annot (create-annotation page +annot-redact+)))
    (set-annotation-rect annot rect)
    annot))

;;; ============================================================================
;;; Redaction (the headline feature)
;;; ============================================================================

(defun redact-page (page &key (options (make-redact-options)))
  "Apply pdf_redact_page to PAGE, removing the content under any
existing redaction marks.  OPTIONS is a REDACT-OPTIONS struct that
controls how text, images and line art are stripped.

Returns T on success.  Note that the surrounding document must
subsequently be re-saved with SAVE-DOCUMENT for the redactions to be
persisted."
  (let* ((doc     (page-document page))
         (ctx-ptr (context-pointer (document-context doc)))
         (pdf-doc (%pdf-document-from-fz-document ctx-ptr (document-pointer doc))))
    (when (cffi:null-pointer-p pdf-doc)
      (error 'mupdf-error :message "redact-page: not a PDF document"))
    (let ((pdf-pg (%pdf-page-from-fz-page ctx-ptr (page-pointer page))))
      (cffi:with-foreign-object (opts '(:struct pdf-redact-options-c))
        (redact-options->foreign options opts)
        (let ((rc (%pdf-redact-page ctx-ptr pdf-doc pdf-pg opts)))
          (maybe-error 'redact-page)
          (values (not (zerop rc))))))))

(defun redact-document (document &key (options (make-redact-options)))
  "Apply REDACT-PAGE to every page of DOCUMENT.  Returns the number of
pages on which redactions were performed."
  (let ((count 0))
    (do-pages (p document count)
      (when (redact-page p :options options)
        (incf count)))))

(defun redact-pdf-file (input-pdf output-pdf
                        &key marks
                             (options (make-redact-options)))
  "End-to-end helper for the common case: open INPUT-PDF, place a set
of redaction marks specified by MARKS, run REDACT-DOCUMENT and save the
result to OUTPUT-PDF.

MARKS is a list of (PAGE-INDEX RECT) pairs, where PAGE-INDEX is the
zero-based page number and RECT is anything COERCE-RECT understands
\(a RECT struct, a (x0 y0 x1 y1) list, or a 4-element vector).

Example:

  (cl-mupdf:redact-pdf-file \"in.pdf\" \"out.pdf\"
    :marks '((0 (100 100 200 120))
             (0 (250 100 350 120))
             (3 (50  600 550 650))))

Returns OUTPUT-PDF."
  (with-context (ctx)
    (declare (ignore ctx))
    (with-document (doc input-pdf)
      ;; Place redaction marks.
      (dolist (mark marks)
        (destructuring-bind (page-index rect) mark
          (with-page (p doc page-index)
            (add-redaction p rect))))
      ;; Apply redactions on every page.
      (redact-document doc :options options)
      ;; Save out.
      (save-document doc output-pdf)))
  output-pdf)

(defun mupdf-version ()
  "Return the MuPDF version string cl-mupdf was compiled to expect.
This must match the runtime libmupdf or fz_new_context_imp will fail."
  *mupdf-version*)
