;;; Define a set of backwards-compatible functions and methods for the future-*
;;; -> promise-* renaming that was previously in cl-async-future.

(in-package :cl-async-future)

(eval-when (:load-toplevel :compile-toplevel)
  (defun str-replace (string old new)
    "Replace a portion of a string with another."
    (let ((pos (search old string :test 'string=)))
      (if pos
          (str-replace (concatenate 'string (subseq string 0 pos) new (subseq string (+ pos (length old)))) old new)
          string))))

(defmacro with-forwarded (name (promisified) &body body)
  (let ((_str-name (gensym "str-name")))
    `(let* ((,_str-name (string-downcase (string ,name)))
            (,promisified (intern (string-upcase (str-replace ,_str-name "future" "promise")))))
       `(progn
          ,,@body
          ;; would rather these be explicit exports in package.lisp
          ;(export ',,name)
          ))))

(defmacro forward-function (name)
  (with-forwarded name (promisified)
    `(setf (symbol-function ',name) (symbol-function ',promisified))))

(defmacro forward-macro (name)
  (with-forwarded name (promisified)
    `(setf (macro-function ',name) (macro-function ',promisified))))

;; -----------------------------------------------------------------------------
;; let the forwarding begin!
;; -----------------------------------------------------------------------------

(defclass future (promise)
  ((preserve-callbacks :accessor promise-preserve-callbacks :initarg :preserve-callbacks :initform nil
    :documentation "When nil (the default) detaches callbacks after running
                    promise.")
   (reattach-callbacks :accessor promise-reattach-callbacks :initarg :reattach-callbacks :initform t
    :documentation "When a promise's callback returns another promise, bind all
                    callbacks from this promise onto the returned one. Allows
                    values to transparently be derived from many layers deep of
                    promises, almost like a real call stack.")))

(defun make-future (&key preserve-callbacks (reattach-callbacks t))
  "Create a blank future."
  (make-instance 'future :preserve-callbacks preserve-callbacks
                         :reattach-callbacks reattach-callbacks))

(forward-function future-finished-p)
(forward-function lookup-forwarded-future)
(forward-function futurep)
(forward-function reset-future)
(forward-macro multiple-future-bind)
(forward-macro future-handler-case)

(setf (macro-function 'wait-for) (macro-function 'wait))

