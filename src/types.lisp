;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

(in-package :cl-libnuma)

;;; A representation of 'struct bitmask'
(define-foreign-type libnuma-bitmask-type ()
  ((specifying :initarg :specifying :initform t :accessor libnuma-bitmask-type-specifying))
  (:actual-type :pointer)	 ; (:pointer (:struct struct-bitmask))
  (:documentation "A CFFI type of 'struct bitmask' of libnuma"))

(define-parse-method libnuma-bitmask-type (&key (specifying t))
  (ecase specifying
    ((:cpu :node t)
     (make-instance 'libnuma-bitmask-type :specifying specifying))))

(deftype numa-bitmask ()
  "A lisp representation of 'struct bitmask' of libnuma"
  'bit-vector)

(defun make-numa-bitmask (&optional (size-spec t))
  (flet ((numa-bitmask-enough-size ()
	   (max (numa-num-possible-cpus)
		(numa-num-possible-nodes))))
    (let ((size (etypecase size-spec
		  (symbol (ecase size-spec
			    (:cpu (numa-num-configured-cpus))
			    (:node (numa-num-configured-nodes))
			    ((t) (numa-bitmask-enough-size))))
		  (integer size-spec)
		  (null (numa-bitmask-enough-size)))))
      (make-array `(,size) :element-type 'bit
		  :initial-element 0)))) ; follows numa_bitmask_alloc()

(defmethod cffi:translate-to-foreign (lisp-bitmask (type libnuma-bitmask-type))
  (let ((bmp (ecase (libnuma-bitmask-type-specifying type)
	       (:cpu (numa-allocate-cpumask*))
	       (:node (numa-allocate-nodemask*))
	       ((t) (numa-bitmask-alloc* (length lisp-bitmask))))))
    (loop for i from 0 below (length lisp-bitmask)
       when (numa-bitmask-isbitset lisp-bitmask i)
       do (numa-bitmask-setbit* bmp i))
    bmp))

(defmethod cffi:translate-from-foreign (bmp (type libnuma-bitmask-type))
  (when (null-pointer-p bmp)		; 'bmp' may be null if numa_parse_***() calls.
    (return-from cffi:translate-from-foreign nil))
  (let* ((size-spec (ecase (libnuma-bitmask-type-specifying type)
		      (:cpu :cpu)
		      (:node :node)
		      ((t) (* +CHAR-BIT+ (numa-bitmask-nbytes* bmp)))))
	 (lisp-bitmask (make-numa-bitmask size-spec)))
    (loop for i from 0 below (length lisp-bitmask)
       when (numa-bitmask-isbitset* bmp i)
       do (numa-bitmask-setbit lisp-bitmask i))
    lisp-bitmask))

(defmethod cffi:free-translated-object (bmp (type libnuma-bitmask-type) param)
  (declare (ignore param))
  (case (libnuma-bitmask-type-specifying type)
    (:cpu (numa-free-cpumask* bmp))
    (:node (numa-free-nodemask* bmp))
    (t (numa-bitmask-free* bmp))))


;;; A representation of a return value from libc.
(defun lisp-to-libc-return-*** (value)
  (assert nil
	  ()
	  "lisp-to-libc-return-*** should not be called (with ~A)"
	  value))

(defun lisp-from-libc-return-boolean (value)
  (>= value 0))

(defctype libc-return-boolean 
    (:wrapper :int
	      :to-c lisp-to-libc-return-***
	      :from-c lisp-from-libc-return-boolean)
  "This type is a wrapper for int returned from libc ('0 == true' and '-1 == false')")


(defun lisp-from-libc-return-int (value)
  (if (>= value 0) value nil))

(defctype libc-return-int
    (:wrapper :int
	      :to-c lisp-to-libc-return-***
	      :from-c lisp-from-libc-return-int)
  "This type is a wrapper for int except returns NIL from minus values.")


(defun lisp-to-malloc-pointer (value)
  (if value value (null-pointer)))

(defun lisp-from-malloc-pointer (value)
  (if (null-pointer-p value) nil value))

(defctype malloc-pointer
    (:wrapper :pointer
	      :to-c lisp-to-malloc-pointer
	      :from-c lisp-from-malloc-pointer)
  "This type is a wrapper for pointer except treats NIL as a null pointer.")


;;; Conditions
(define-condition not-found-warning (simple-warning)
  ())
