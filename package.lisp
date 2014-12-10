(defpackage :cl-async-future
  (:use :cl :blackbird-base :blackbird-syntax :blackbird)
  (:nicknames :asf)
  (:import-from :blackbird-base
                #:finish
                #:lookup-forwarded-promise
                #:promise-values)
  (:export #:future
           #:future-finished-p
           #:make-future
           #:attach-errback
           #:lookup-forwarded-future
           #:signal-error
           #:futurep
           #:finish
           #:reset-future
           #:attach
           #:alet
           #:alet*
           #:aif
           #:multiple-future-bind
           #:wait-for
           #:adolist
           #:future-handler-case))
(in-package :cl-async-future)

(loop for sym being the external-symbols of (find-package :blackbird) do
  (import sym)
  (export sym))

