(defpackage z80
  (:use :common-lisp)
  (:import-from :place-utils :applyf :funcallf)
  (:import-from :alexandria :assoc-value)
  (:export #:cpu #:emulate #:emulate-rom))
