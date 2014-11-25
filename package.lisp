(defpackage :cl-async-future
  (:use :cl :blackbird)
  (:nicknames :asf)
  (:export #:future
           #:future-finished-p
           #:make-future
           #:lookup-forwarded-future
           #:futurep
           #:reset-future
           #:multiple-future-bind
           #:future-handler-case
           #:wait-for))
(in-package :cl-async-future)

(loop for sym being the external-symbols of (find-package :blackbird) do
  (import sym)
  (export sym))


