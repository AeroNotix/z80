(defpackage z80
  (:use :common-lisp)
  (:import-from :place-utils :applyf :funcallf)
  (:export #:cpu #:emulate #:emulate-rom))
