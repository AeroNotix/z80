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

(def-test jump-to-port-address (:suite roms)
    (let ((rom (get-rom "jump-to-port-address.rom"))
          (cpu (make-instance 'z80:cpu :peripherals (list (make-instance 'fake-peripheral )))))
      (is (z80::reg-a (z80:emulate-rom cpu rom)) 123)))
