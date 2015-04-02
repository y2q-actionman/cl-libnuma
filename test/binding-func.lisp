(in-package :cl-libnuma.test)

(defun test-numa-parser ()
  ;; numa-parse-bitmap
  ;; numa-parse-nodestring
  ;; numa-parse-nodestring-all
  ;; numa-parse-cpustring
  ;; numa-parse-cpustring-all
  t)

(defun test-numa-node-size ()
  (loop for node from 0 below (numa-num-configured-nodes)
     do (multiple-value-bind (size free) (numa-node-size node)
	  (multiple-value-bind (size64 free64) (numa-node-size64 node)
	    (and-assert
	     (integerp size) (integerp free)
	     (integerp size64) (integerp free64))
	    (when (<= size64 most-positive-fixnum)
	      (assert (= size size64)))
	    (when (<= free64 most-positive-fixnum)
	      (assert (= free free64))))))
  ;; error case
  (and-assert
   (not (numa-node-size -1))
   (not (numa-node-size64 -1))
   (not (numa-node-size (numa-num-configured-nodes)))
   (not (numa-node-size64 (numa-num-configured-nodes))))
  t)

(defun test-numa-preferred ()
  (assert (integerp (numa-preferred)))
  ;; in range
  (loop for node from 0 below (numa-num-configured-nodes)
     do (numa-set-preferred node)
       (assert (= node (numa-preferred))))
  ;; out of range
  (let ((orig-preferred (numa-preferred)))
    (numa-set-preferred (numa-num-configured-nodes))
    (assert (= orig-preferred (numa-preferred))))
  ;; sets default
  (numa-set-preferred -1)
  (assert (integerp (numa-preferred)))
  t)

(defun test-numa-interleave-mask ()
  (let ((orig-mask (numa-get-interleave-mask)))
    (assert (typep orig-mask 'numa-bitmask))
    (unwind-protect
	 (progn
	   ;; interleaving all
	   (numa-set-interleave-mask *numa-all-nodes-bitmask*)
	   (assert (equal (numa-get-interleave-mask)
			  *numa-all-nodes-bitmask*))
	   ;; no interleaving
	   (numa-set-interleave-mask *numa-no-nodes-bitmask*)
	   (assert (equal (numa-get-interleave-mask)
			  *numa-no-nodes-bitmask*))
	   ;; random..
	   (let ((test-mask (make-random-numa-bitmask :node)))
	     (numa-set-interleave-mask test-mask)
	     (assert (equal (numa-get-interleave-mask)
			    test-mask))))
      (numa-set-interleave-mask orig-mask)))
  t)

(defun test-numa-membind ()
  ;; numa-set-membind
  ;; numa-get-membind
  t)

(defun test-numa-alloc ()
  ;; numa-alloc-***
  t)

(defun test-numa-run-on ()
  ;; numa-run-on-node
  ;; numa-run-on-node-mask
  ;; numa-run-on-node-mask-all
  ;; numa-get-run-node-mask
  t)

(defun test-numa-memory-location ()
  ;; numa-tonode-memory
  ;; numa-tonodemask-memory
  ;; numa-setlocal-memory
  ;; numa-police-memory
  t)

(defun test-numa-distance ()
  t)

(defun test-numa-affinity ()
  t)

(defun test-numa-node-to/of-cpu ()
  t)

(defun test-numa-bitmask ()
  t)

(defun test-numa-move-pages ()
  t)

(defun test-numa-migrate-pages ()
  t)

(defun test-numa-error ()
  (macrolet ((try-call (switch-var func-name &rest func-args)
	       `(let ((old-value ,switch-var))
		  (unwind-protect
		       (always-success
			 (setf ,switch-var nil)
			 (,func-name ,@func-args))
		    (setf ,switch-var old-value)))))
    (and-assert
     (typep *numa-exit-on-error* 'boolean)
     (typep *numa-exit-on-warn* 'boolean)
     (try-call *numa-exit-on-error*
	       numa-error "hoge")
     (try-call *numa-exit-on-warn*
	       numa-warn 0 "hoge"))))

(defun test-binding-func ()
  (and-assert
   (typep (numa-available) 'boolean)

   (integerp (numa-max-possible-node))
   (integerp (numa-num-possible-nodes))
   (integerp (numa-num-possible-cpus))

   (integerp (numa-max-node))
   (integerp (numa-num-configured-nodes))
   (typep (numa-get-mems-allowed) 'numa-bitmask)

   (integerp (numa-num-configured-cpus))
   (typep *numa-all-nodes-bitmask* 'numa-bitmask)
   (typep *numa-no-nodes-bitmask* 'numa-bitmask)
   (typep *numa-all-cpus-bitmask* 'numa-bitmask)

   (integerp (numa-num-task-cpus))
   (integerp (numa-num-task-nodes))

   (test-numa-parser)

   (test-numa-node-size)

   (test-numa-preferred)
   (integerp (numa-get-interleave-node))
   (test-numa-interleave-mask)
   ;; numa-interleave-memory
   ;; numa-bind
   ;; numa-set-localalloc
   (test-numa-membind)

   (test-numa-alloc)
   
   (test-numa-run-on)

   (test-numa-memory-location)
   (always-success
     (numa-set-bind-policy nil)
     (numa-set-bind-policy t))
   (always-success
     (numa-set-strict t)
     (numa-set-strict nil))

   (test-numa-distance)

   (test-numa-affinity)
   (test-numa-node-to/of-cpu)

   (test-numa-bitmask)

   (test-numa-move-pages)

   (test-numa-migrate-pages)

   (test-numa-error)
   ))
