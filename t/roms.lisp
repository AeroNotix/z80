(in-package :z80-tests)

(def-suite roms)
(in-suite roms)

(defun test-roms ()
  (run! 'roms))

(let ((rom-path (asdf:system-relative-pathname :z80 "t/test-roms")))
  (defun get-rom (rom-name)
    (make-pathname :directory (namestring rom-path) :name rom-name)))

(defclass fake-peripheral () ())

(defmethod z80::read-from ((fp fake-peripheral))
  #x0050)

(defun test-rom (rom-filename value-extractor expected-value)
  (let ((rom (get-rom rom-filename))
        (cpu (make-instance 'z80:cpu :peripherals (list (make-instance 'fake-peripheral)))))
    (is (eq (funcall value-extractor (z80:emulate-rom cpu rom)) expected-value))))

(def-test expected-register-value-roms (:suite roms)
  (let ((roms-and-expected-values (list (list "jump-to-port-address.rom" #'z80::reg-a 123)
                                        (list "load-tests.rom" #'z80::reg-l 123)
                                        (list "load-literals.rom" #'z80::reg-l 7)
                                        (list "xors.rom" #'z80::reg-a 255)
                                        (list "flags.rom" #'z80::reg-a 123))))
    (mapcar (lambda (args) (apply #'test-rom args)) roms-and-expected-values)))
