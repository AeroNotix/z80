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

(defmacro with-stdout-to-string (&body forms)
  `(let ((*standard-output* (make-string-output-stream)))
     ,@forms
     (get-output-stream-string *standard-output*)))

(defun test-register-assertion-rom (rom-filename value-extractor expected-value)
  (let ((rom (get-rom rom-filename))
        (cpu (make-instance 'z80:cpu
                            :peripherals (list (make-instance 'fake-peripheral)))))
    (z80:emulate-rom cpu rom)
    (is (eql (funcall value-extractor cpu) expected-value))))

(defun test-io-assertion-rom (rom-filename expected-output &optional (starting-pc 0))
  (let ((rom (get-rom rom-filename))
        (cpu (make-instance 'z80:cpu
                            :peripherals (list (make-instance 'z80::terminal-printer-peripheral)))))
    (is (equal expected-output (with-stdout-to-string (z80:emulate-rom cpu rom :starting-pc starting-pc))))))

(defun test-ram-assertion-rom (rom-filename expected-value start end)
  (let ((rom (get-rom rom-filename))
        (cpu (make-instance 'z80:cpu)))
    (is (equalp (subseq  (z80::ram (z80:emulate-rom cpu rom)) start end) expected-value))))

(defun apply-tests (asserter args)
  (handler-bind ((warning (lambda (w) (declare (ignore w)) (muffle-warning))))
    (mapcar (lambda (arg) (apply asserter arg)) args)))

(def-test expected-register-value-roms (:suite roms)
  ;; assertion values are just any old value chosen. Often chosen to
  ;; be relatively "impossible" to achieve accidentally from the
  ;; operations in the ROM. E.g. wouldn't assert 255 when 255 was
  ;; manually placed into a register
  (let ((roms-and-expected-values (list (list "jump-to-port-address.rom" #'z80::reg-a 123)
                                        (list "load-tests.rom" #'z80::reg-l 123)
                                        (list "load-literals.rom" #'z80::reg-l 7)
                                        (list "xors.rom" #'z80::reg-a 255)
                                        (list "flags.rom" #'z80::reg-a 123)
                                        (list "jz-flag.rom" #'z80::reg-a 255)
                                        (list "jnz-flag.rom" #'z80::reg-a 255)
                                        (list "jc-flag.rom" #'z80::reg-a 66)
                                        (list "load-indirect.rom" #'z80::reg-a 123)
                                        (list "cp-nz-test.rom" #'z80::reg-a 255)
                                        (list "cp-z-test.rom" #'z80::reg-a 255))))
    (apply-tests #'test-register-assertion-rom roms-and-expected-values)))

(def-test expected-io-roms (:suite roms)
  (let ((roms-and-expected-output (list (list "peripheral-test.rom" (format nil "THISISATEST~C~C" #\Newline #\Return) #x0100))))
    (apply-tests #'test-io-assertion-rom roms-and-expected-output)))

(def-test expected-ram-roms (:suite roms)
  (let ((roms-and-expected-ram-data (list (list "block-move-ldir.rom"
                                                #(#x56 #x56 #x56 #x56 #x56
                                                  #x56 #x56 #x56 #x56 #x56
                                                  #x56 #x56 #x56 #x56 #x56
                                                  #x56 #x56 #x56 #x56 #x56)
                                                #x23
                                                (+ #x23 #x14)))))
    (apply-tests #'test-ram-assertion-rom roms-and-expected-ram-data)))
