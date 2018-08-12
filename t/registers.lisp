(in-package :z80-tests)

(def-suite registers)
(in-suite registers)

(defun test-registers ()
  (run! 'registers))

(def-test maintain-register-invariants (:suite registers)
  (let ((registers (list 'z80::reg-a 'z80::reg-b 'z80::reg-c 'z80::reg-d 'z80::reg-e 'z80::reg-h 'z80::reg-l))
        (c (make-instance 'z80::cpu)))
    (loop for register in registers
       do (funcall (fdefinition `(setf ,register)) (random 255) c))
    (is (eq (z80::reg-f c) (logand (z80::reg-af c) #x00FF)))
    (is (eq (z80::reg-c c) (logand (z80::reg-bc c) #x00FF)))
    (is (eq (z80::reg-e c) (logand (z80::reg-de c) #x00FF)))
    (is (eq (z80::reg-l c) (logand (z80::reg-hl c) #x00FF)))
    (is (eq (z80::reg-af c) (logior (ash (z80::reg-a c) 8) (z80::reg-f c))))
    (is (eq (z80::reg-bc c) (logior (ash (z80::reg-b c) 8) (z80::reg-c c))))
    (is (eq (z80::reg-de c) (logior (ash (z80::reg-d c) 8) (z80::reg-e c))))
    (is (eq (z80::reg-hl c) (logior (ash (z80::reg-h c) 8) (z80::reg-l c))))
    (is (eq (z80::reg-a c) (ash (z80::reg-af c) -8)))
    (is (eq (z80::reg-b c) (ash (z80::reg-bc c) -8)))
    (is (eq (z80::reg-d c) (ash (z80::reg-de c) -8)))
    (is (eq (z80::reg-h c) (ash (z80::reg-hl c) -8)))))

(def-test memory-accessors (:suite registers)
  (let ((c (make-instance 'z80::cpu))
        (memory-locations
         (list 'z80::reg-af 'z80::mem-af
               'z80::reg-bc 'z80::mem-bc
               'z80::reg-de 'z80::mem-de
               'z80::reg-hl 'z80::mem-hl)))
    (loop for (address-register memory-location) on memory-locations :by #'cddr
       do
         (let ((address (random 65535))
               (value   (random 255)))
           (funcall (fdefinition `(setf ,address-register)) address c)
           (funcall (fdefinition `(setf ,memory-location)) value c)
           (is (eq (funcall memory-location c) value))
           (is (eq (elt (z80::ram c) (funcall address-register c))
                   (funcall memory-location c)))))))
