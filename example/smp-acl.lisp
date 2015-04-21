;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This work is free. You can redistribute it and/or modify it under
;; the terms of the Do What The Fuck You Want To Public License,
;; Version 2, as published by Sam Hocevar. See http://www.wtfpl.net/
;; for more details.

;; This code works on Allegro Only

(in-package :cl-user)

(defpackage :cl-libnuma.example.acl-smp
  (:use :cl :cl-libnuma :excl))

(in-package :cl-libnuma.example.acl-smp)

(defparameter *allocation-size* (* 16 (numa-pagesize)))

(defun libnuma-alloc-test (allocation-type loop-count)
  (let* ((nodes (numa-num-configured-nodes))
	 (a-mems
	  (loop for n from 0 below nodes
	     collect
	       (ecase allocation-type
		 (:onnode (numa-alloc-onnode *allocation-size* n))
		 (:interleave (numa-alloc-interleaved *allocation-size*))
		 (:only-one-node (numa-alloc-onnode *allocation-size* 0)))))
	 (procs))
    (unwind-protect
	 (flet ((worker-function (node mem)
		  (numa-run-on-node node)
		  (format t "~&I am node=~A, mem=~A~%"
			  (numa-get-run-node-mask) mem)
		  ;; zero fill
		  (loop for pos from 0 below *allocation-size*
		     do (setf (system:memref-int mem 0 pos :unsigned-byte) 0))
		  ;; main loop
		  (loop for lc from 1 to loop-count
		     do
		     ;; increment
		       (loop for pos from 0 below *allocation-size*
			  do (incf (system:memref-int mem 0 pos :unsigned-byte)))
		     ;; summing
		       (loop for pos from 0 below *allocation-size*
			  sum (system:memref-int mem 0 pos :unsigned-byte)
			  into result
			  finally
			    (format t "~&I am node=~A, mem=~A, loop ~A~%"
				    (numa-get-run-node-mask) mem lc)
			    (format t "~& result = ~A~%" result)))))
	   (loop for n from 0 below nodes
	      do (push (mp:process-run-function
			"libnuma test" #'worker-function n (nth n a-mems))
		       procs)
	      finally
		(map nil #'mp:process-join procs)))
      (map nil #'mp:process-kill procs)
      (loop for mem in a-mems
	 do (numa-free mem *allocation-size*)))))
