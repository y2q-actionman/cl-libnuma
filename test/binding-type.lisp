(in-package :cl-libnuma.test)

;; types
(defun test-binding-cffi-types ()
  (assert-progn
    (cffi-type-exists 'libnuma-bitmask-type)
    (cffi-type-exists '(libnuma-bitmask-type :specifying :cpu))
    (cffi-type-exists '(libnuma-bitmask-type :specifying :node))
    (cffi-type-exists '(libnuma-bitmask-type :specifying t))
    (assume-condition ()
      (cffi-type-exists '(libnuma-bitmask-type :specifying 'hoge)))
    (cffi-type-exists 'mbind-flag)))

(defun test-make-numa-bitmask ()
  (assert-progn
    (make-numa-bitmask)
    (make-numa-bitmask :cpu)
    (make-numa-bitmask :node)
    (make-numa-bitmask 1)
    (make-numa-bitmask 5)))

(defun make-random-numa-bitmask (&optional (size-spec t))
  (loop with mask = (make-numa-bitmask size-spec)
     for i from 0 below (length mask)
     do (setf (bit mask i) (random 2))
     finally (return mask)))

(defun test-numa-bitmask-conversion ()
  (flet ((test-mask-conversion (mask-type)
	   (let* ((mask (make-random-numa-bitmask mask-type))
		  (new-mask
		   (cl-libnuma::with-temporal-struct-bitmask-pointer
		       (foreign-bmp (convert-to-foreign mask 'libnuma-bitmask-type))
		     (convert-from-foreign foreign-bmp 'libnuma-bitmask-type))))
	     (assert (typep new-mask 'numa-bitmask))
	     (loop for i across mask
		for j across new-mask
		always (= i j)))))
    (assert-progn
      (test-mask-conversion t)
      (test-mask-conversion :cpu)
      (test-mask-conversion :node)
      (test-mask-conversion 1)
      (test-mask-conversion 5)
      (null (convert-from-foreign (null-pointer) 'libnuma-bitmask-type))
      )))

(defun test-binding-internal-cffi-types ()
  (assert-progn
    (cffi-type-exists 'cl-libnuma::libc-return-boolean)
    (convert-from-foreign (convert-to-foreign 0 :int)
			  'cl-libnuma::libc-return-boolean)
    (not (convert-from-foreign (convert-to-foreign -1 :int)
			       'cl-libnuma::libc-return-boolean))
    (assume-condition ()
      (convert-to-foreign t 'cl-libnuma::libc-return-boolean))
    (assume-condition ()
      (convert-to-foreign nil 'cl-libnuma::libc-return-boolean))

    (cffi-type-exists 'cl-libnuma::libc-return-int)
    (= 0 (convert-from-foreign (convert-to-foreign 0 :int)
			       'cl-libnuma::libc-return-int))
    (= 1 (convert-from-foreign (convert-to-foreign 1 :int)
			       'cl-libnuma::libc-return-int))
    (not (convert-from-foreign (convert-to-foreign -1 :int)
			       'cl-libnuma::libc-return-int))
    (assume-condition ()
      (convert-to-foreign 0 'cl-libnuma::libc-return-int))
    (assume-condition ()
      (convert-to-foreign 1 'cl-libnuma::libc-return-int))
    (assume-condition ()
      (convert-to-foreign -1 'cl-libnuma::libc-return-int))

    (cffi-type-exists 'cl-libnuma::malloc-pointer)
    (null-pointer-p
     (convert-to-foreign nil 'cl-libnuma::malloc-pointer))
    (not (convert-from-foreign (null-pointer)
			       'cl-libnuma::malloc-pointer))
    (with-foreign-pointer (ptr 1)
      (let* ((lptr (convert-from-foreign ptr 'cl-libnuma::malloc-pointer))
	     (mlptr (convert-to-foreign lptr 'cl-libnuma::malloc-pointer)))
	(and lptr mlptr)))))

(defun test-binding-type ()
  (and (test-binding-cffi-types)
       (test-make-numa-bitmask)
       (test-numa-bitmask-conversion)
       (test-binding-internal-cffi-types)))
