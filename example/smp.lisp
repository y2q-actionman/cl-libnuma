(in-package :cl-user)

(asdf:load-system :bordeaux-threads)
(asdf:load-system :cffi)

(defpackage :cl-libnuma.example.smp
  (:use :cl :cl-libnuma :cffi))

(in-package :cl-libnuma.example.smp)

(defparameter *allocation-pages* 16)
(defparameter *allocation-size* (* (numa-pagesize)
				   *allocation-pages*))

(defparameter *print-message* nil)

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
	 (loop for node from 0 below nodes
	    do (push (bordeaux-threads:make-thread
		      (let ((node node)
			    (mem (nth node a-mems)))
			(declare (optimize (speed 3) (safety 0))
				 (type fixnum node *allocation-size* loop-count))
			(lambda ()
			  (numa-run-on-node node)
			  (when *print-message*
			    (format t "~&I am node=~A, mem=~A~%" (numa-get-run-node-mask) mem))
			  ;; zero fill
			  (loop for pos of-type fixnum from 0 below *allocation-size*
			     do (setf (cffi:mem-aref mem :unsigned-char pos) 0))
			  ;; main loop
			  (loop for lc of-type fixnum from 1 to loop-count
			     do
			     ;; increment
			       (loop for pos of-type fixnum from 0 below *allocation-size*
				  do (incf (cffi:mem-aref mem :unsigned-char pos)))
			     ;; summing
			       (loop for pos of-type fixnum from 0 below *allocation-size*
				  sum (cffi:mem-aref mem :unsigned-char pos)
				  into result of-type integer
				  finally
				    (when *print-message*
				      (format t "~&I am node=~A, mem=~A, loop ~A~%" (numa-get-run-node-mask) mem lc)
				      (format t "~& result = ~A~%" result)))))))
		     procs)
	    finally
	      (map nil #'bordeaux-threads:join-thread procs))
      (map nil #'bordeaux-threads:destroy-thread procs))
    (when *print-message*
      ;; describe pages
      (let* ((all-pages
	      (loop for mem-start in a-mems
		 append
		   (loop for count from 0 below *allocation-pages*
		      as mem = mem-start then (cffi:inc-pointer mem (numa-pagesize))
		      collect mem)))
	     (all-pages (sort all-pages
			      #'(lambda (p q) (< (cffi:pointer-address p)
						 (cffi:pointer-address q)))))
	     (page-status (numa-move-pages 0 all-pages nil)))
	(unless page-status
	  (format t "numa-move-pages failed"))
	(loop initially (format t "~&Page Status~%")
	   for page in all-pages
	   for status in page-status
	   do (format t "~&page ~A: ~:[error =~;node =~] ~A~%" page (>= status 0) status))))
    (loop for mem in a-mems
       do (numa-free mem *allocation-size*))))
