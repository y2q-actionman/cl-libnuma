(in-package :cl-libnuma)

(defmacro with-temporal-bitmask ((var form &key (check-predefined t)) &body body)
  `(let ((,var ,form))
     ,(when check-predefined
	    `(assert (not (or (eq ,var *numa-all-nodes-ptr*)
			      (eq ,var *numa-no-nodes-ptr*)
			      (eq ,var *numa-all-cpus-ptr*)))))
     (unwind-protect
	  (progn ,@body)
       (numa-bitmask-free ,var))))

;; TODO
;; (defmacro with-numa-alloc ())

(defun describe-bitmask (bitmask)
  (loop initially (fresh-line)

     for i from 0 below (numa-bitmask-weight bitmask)
     as bit = (numa-bitmask-isbitset bitmask i)
     if (plusp bit)
     do (write-char #\X)
     else
     do (write-char #\_)
       
     finally (terpri)))
