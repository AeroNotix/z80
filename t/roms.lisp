(in-package :z80-tests)

(def-suite roms)
(in-suite roms)

#|

assertion values are just any old value chosen. Often chosen to be
relatively "impossible" to achieve accidentally from the operations in
the ROM. E.g. wouldn't assert 255 when 255 was manually placed into a
register

|#

(defun test-roms ()
  (run! 'roms))

(let ((rom-path (asdf:system-relative-pathname :z80 "t/test-roms")))
  (defun get-rom (rom-name)
    (make-pathname :directory (namestring rom-path) :name rom-name)))

(defun useful-registers (cpu)
  (list (reg-a cpu)
        (reg-b cpu)
        (reg-c cpu)
        (reg-d cpu)
        (reg-e cpu)
        (reg-h cpu)
        (reg-l cpu)
        (mem-hl cpu)))

(defclass fake-peripheral () ())

(defmethod z80::read-from ((fp fake-peripheral)) #x0050)

(defmacro with-stdout-to-string (&body forms)
  `(let ((*standard-output* (make-string-output-stream)))
     ,@forms
     (get-output-stream-string *standard-output*)))

(defun test-register-assertion-rom (rom-filename value-extractor expected-value)
  (let ((rom (get-rom rom-filename))
        (cpu (make-instance 'z80:cpu
                            :peripherals (list (make-instance 'fake-peripheral)))))
    (z80:emulate-rom cpu rom)
    (is (equalp (funcall value-extractor cpu) expected-value)
        (format nil "~A did not produce the expected-value~%" rom-filename))))

(defun test-output-assertion-rom (rom-filename expected-output &optional (starting-pc 0))
  (let ((rom (get-rom rom-filename))
        (cpu (make-instance 'z80:cpu
                            :peripherals (list (make-instance 'simple-io-peripheral)))))
    (is (equal expected-output (with-stdout-to-string (z80:emulate-rom cpu rom :starting-pc starting-pc)))
        (format nil "~A did not produce the expected-value~%" rom-filename))))

(defun test-input-assertion-rom (rom-filename expected-input start end &optional (starting-pc 0))
  (let* ((rom (get-rom rom-filename))
         (sis (make-string-input-stream expected-input))
         (cpu (make-instance 'z80:cpu
                             :peripherals (list (make-instance 'simple-io-peripheral :input-stream sis)))))
    (is (equalp (subseq (ram (z80:emulate-rom cpu rom :starting-pc starting-pc)) start end) expected-input)
        (format nil "~A did not produce the expected-value~%" rom-filename))))

(defun test-ram-assertion-rom (rom-filename expected-value start end)
  (let ((rom (get-rom rom-filename))
        (cpu (make-instance 'z80:cpu)))
    (is (equalp (subseq (ram (z80:emulate-rom cpu rom)) start end) expected-value)
        (format nil "~A did not produce the expected-value~%" rom-filename))))

(defun apply-tests (asserter args)
  (handler-bind ((warning (lambda (w) (declare (ignore w)) (muffle-warning))))
    (mapcar (lambda (arg) (apply asserter arg)) args)))

(def-test expected-register-value-roms (:suite roms)
  (let ((roms-and-expected-values (list (list "jump-to-port-address.rom" #'reg-a 123)
                                        (list "load-tests.rom" #'reg-l 123)
                                        (list "load-literals.rom" #'reg-l 7)
                                        (list "xors.rom" #'reg-a 255)
                                        (list "flags.rom" #'reg-a 123)
                                        (list "jz-flag.rom" #'reg-a 255)
                                        (list "jnz-flag.rom" #'reg-a 255)
                                        (list "jc-flag.rom" #'reg-a 66)
                                        (list "load-indirect.rom" #'reg-a 123)
                                        (list "cp-nz-test.rom" #'reg-a 255)
                                        (list "cp-z-test.rom" #'reg-a 255)
                                        (list "res-set.rom" #'reg-a 170)
                                        (list "index-addressing.rom" (lambda (cpu)
                                                                       (list (reg-ix cpu)
                                                                             (reg-iy cpu)
                                                                             (reg-a cpu)
                                                                             (reg-b cpu)
                                                                             (elt (ram cpu) (+ 3 (reg-iy cpu)))
                                                                             (elt (ram cpu) (+ 4 (reg-iy cpu)))))
                                              (list 65535 #x15 65 66 65 66))
                                        (list "generated-res.rom" #'useful-registers
                                              (make-list 8 :initial-element #x00))
                                        (list "generated-set.rom" #'useful-registers
                                              (make-list 8 :initial-element #xFF)))))
    (apply-tests #'test-register-assertion-rom roms-and-expected-values)))

(def-test expected-output-roms (:suite roms)
  (let ((roms-and-expected-output (list (list "peripheral-test.rom" (format nil "THISISATEST~C~C" #\Newline #\Return) #x0100)
                                        (list "bf-unconditional.rom" "Hello, world!" #x94)
                                        (list "output.rom" "HELLO!!OLLEHHH"))))
    (apply-tests #'test-output-assertion-rom roms-and-expected-output)))

(def-test expected-input-roms (:suite roms)
  (let ((roms-and-expected-input (list
                                  (list "input.rom" "ABCDEFGHJI" #x1000 #x100A))))
    (apply-tests #'test-input-assertion-rom roms-and-expected-input)))

(def-test expected-ram-roms (:suite roms)
  (let ((roms-and-expected-ram-data (list (list "block-move-ldir.rom"
                                                (make-array 20 :initial-element #x56)
                                                #x23
                                                (+ #x23 #x14)))))
    (apply-tests #'test-ram-assertion-rom roms-and-expected-ram-data)))
