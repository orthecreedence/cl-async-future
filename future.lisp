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

(setf (macro-function 'wait-for) (macro-function 'wait))

;; -----------------------------------------------------------------------------
;; old error handling stuff
;; -----------------------------------------------------------------------------

(defmacro %handler-case (body &rest bindings)
  "Simple wrapper around handler-case that allows switching out the form to make
   macroexpansion a lot easier to deal with."
  `(handler-case ,body ,@bindings))

(defmacro wrap-event-handler (future-gen error-forms)
  "Used to wrap the future-generation forms of future syntax macros. This macro
   is not to be used directly, but instead by future-handler-case.

   It allows itself to be recursive, but any recursions will simply add their
   error forms for a top-level list and return the form they are given as the
   body. This allows a top-level form to add an error handler to a future, while
   gathering the lower-level forms' handler-case bindings into one big handler
   function (created with make-nexted-handler-cases).

   Note that since normally the wrap-event-handler forms expand outside in, we
   have to do some trickery with the error-handling functions to make sure the
   order of the handler-case forms (as far as what level of the tree we're on)
   are preserved."
  (let ((signal-error (gensym "signal-error"))
        (handler-fn (gensym "handler-fn"))
        (vals (gensym "vals")))
    ;; hijack any child wrap-event-handler macros to just return their
    ;; future-gen form verbatim, but add their error handlers to the error
    ;; handling chain
    `(macrolet ((wrap-event-handler (future-gen error-forms)
                  (let ((old-signal-error (gensym "old-signal-error")))
                    `(progn
                       ;; "inject" the next-level down error handler in between the
                       ;; error triggering function and the error handler one level
                       ;; up. this preserves the handler-case tree (as opposed to
                       ;; reversing it)
                       ;; NOTE that signal-error is defined *below* in the body
                       ;; of the macrolet form
                       (let ((,old-signal-error ,',signal-error))
                         (setf ,',signal-error
                               (lambda (ev)
                                 (%handler-case
                                   (funcall ,old-signal-error ev)
                                   ,@error-forms))))
                       ;; return the future-gen form verbatim
                       ,future-gen))))
       ;; define a function that signals the error, and a top-level error handler
       ;; which uses the error-forms passed to THIS macro instance. any instance
       ;; of `wrap-event-handler` that occurs in the `future-gen` form will inject
       ;; its error handler between handler-fn and signal-error.
       (let* ((,signal-error (lambda (ev) (error ev)))
              (,handler-fn (lambda (ev)
                             (%handler-case
                               (funcall ,signal-error ev)
                               ,@error-forms)))
              ;; sub (wrap-event-handler ...) forms are expanded with ,future-gen
              ;; they add their handler-case forms into a lambda which is injected
              ;; into the error handling chain,
              (,vals (multiple-value-list ,future-gen)))
         (if (futurep (car ,vals))
             (progn
               (attach-errback (car ,vals) ,handler-fn)
               (car ,vals))
             (apply #'values ,vals))))))

(defmacro future-handler-case (body-form &rest error-forms &environment env)
  "Wrap all of our lovely attach macro up with an event handler. This is more or
   less restricted to the form it's run in.

   Note that we only have to wrap (attach) because *all other syntax macros* use
   attach. This greatly simplifies our code.

   Note that if we just wrap `attach` directly in a macrolet, it expands
   infinitely (probably no what we want). So we're doing some trickery here. We
   use the environment from the top-level macro to grab the original macro
   function and make it available from *within* the macrolet. This allows
   the macrolet to redefine the `attach` macro while also simultaneously
   expanding the previous definition of it. This allows wrapped calls of
   future-handler-case to add layers of error handling around any `attach` call
   that is within lexical grasp."
  (if (or (find :future-debug *features*)
          (find :future-debug *features*))
      ;; we're debugging futures...disable all error handling (so errors bubble
      ;; up to main loop)
      body-form
      ;; wrap the top-level form in a handler-case to catch any errors we may
      ;; have before the futures are even generated.
      `(%handler-case
         ;; redefine our attach macro so that the future-gen forms are
         ;; wrapped (recursively, if called more than once) in the
         ;; `wrap-event-handler` macro.
         (macrolet ((attach (future-gen fn &environment ml-env)
                      (let ((args (gensym "phc-wrap-args")))
                        ;; call the original attach macro (via our pass env).
                        ;; this allows calling it without throwing macrolet
                        ;; into an endless loop
                        (funcall (macro-function 'attach ',env)
                          `(attach
                             (wrap-event-handler ,future-gen ,',error-forms)
                             ;; create a wrapper function around the given
                             ;; callback that applies our error handlers
                             (lambda (&rest ,args)
                               (%handler-case
                                 (apply ,fn ,args)
                                 ,@',error-forms)))
                          ml-env))))
             ,body-form)
         ,@error-forms)))

