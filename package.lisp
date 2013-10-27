(defpackage :cl-async-future
  (:use :cl)
  (:nicknames :asf)
  (:export #:future
           #:future-finished-p
           #:future-backtrace
           #:make-future
           #:add-backtrace-entry
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

