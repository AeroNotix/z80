(defpackage #:z80-asm
  (:use :cl)
  (:import-from :alexandria :assoc-value))

;; (defun assemble (path)
;;   ;; Take the path, assemble the file
;;   )

;; (defparameter function-pointer-table-index #x00)

;; (defparameter label-table NIL)

;; (defparameter z80-registers (list 'a 'b 'c 'd 'e 'f 'af 'bc 'de 'hl))

;; (defun register-p (maybe-reg)
;;   (find maybe-reg z80-registers :test #'equal))

;; (defun z80-function-arguments-to-push (args)
;;   (remove-if-not #'register-p args))

;; (defun push-registers (args)
;;   (let ((registers (z80-function-arguments-to-push args)))
;;     (loop for register in registers
;;        collect
;;          (list 'push register))))

;; (defun count-asm-instructions (instructions)
;;   (length (alexandria:flatten instructions)))

;; (defun generate-asm-function  (label instructions)
;;   (let ((function-size (count-asm-instructions instructions)))
;;     (push (cons label function-pointer-table-index) label-table)
;;     (incf function-pointer-table-index function-size)))

;; (defmacro defz80fun (label args &body body)
;;   `(defmacro ,label ,args
;;      ,(generate-asm-function
;;        label
;;        (list
;;         (push-registers args)
;;         'ret))))

;; (defmacro dup (times &body body)
;;   `(progn ,@(loop for i upto times
;;                collect `(progn ,@body))))

;; (dup 6 (+ 1 1))

;; (defz80fun multiply (x y)
;;   (local-label decrement-again)
;;   (add y x)
;;   (dec y)
;;   (jnz decrement-again))
