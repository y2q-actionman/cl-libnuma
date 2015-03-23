(in-package :cl-libnuma)

(defun predefined-bitmask-p (bitmask)
  (or (eq bitmask *numa-all-nodes-ptr*)
      (eq bitmask *numa-no-nodes-ptr*)
      (eq bitmask *numa-all-cpus-ptr*)))

(defmacro with-freeing-bitmask ((var form &key (check-predefined t)) &body body)
  `(let ((,var ,form))
     ,(when check-predefined
	    `(assert (not (predefined-bitmask-p ,var))))
     (unwind-protect
	  (progn ,@body)
       (numa-bitmask-free ,var))))
