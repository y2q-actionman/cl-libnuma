(in-package :cl-libnuma.test)

(defun test-numa-parser ()
  ;; numa-parse-bitmap
  ;; numa-parse-nodestring
  ;; numa-parse-nodestring-all
  ;; numa-parse-cpustring
  ;; numa-parse-cpustring-all
  t)

(defun test-numa-node-size ()
  ;; numa-node-size
  ;; numa-node-size64
  t)

(defun test-numa-preferred ()
  ;; numa-preferred
  ;; numa-set-preferred
  t)

(defun test-numa-interleave-mask ()
  ;; numa-get-interleave-mask
  ;; numa-set-interleave-mask
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
		       (progn (setf ,switch-var nil)
			      (,func-name ,@func-args)
			      t)
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
   (typep *numa-all-nodes* 'numa-bitmask)
   (typep *numa-no-nodes* 'numa-bitmask)
   (typep *numa-all-cpus* 'numa-bitmask)

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
   (progn (numa-set-bind-policy nil)
	  (numa-set-bind-policy t)	; default is 'strict'
	  t)
   (progn (numa-set-strict t)
	  (numa-set-strict nil)     	; default is 'preferred'
	  t)

   (test-numa-distance)

   (test-numa-affinity)
   (test-numa-node-to/of-cpu)

   (test-numa-bitmask)

   (test-numa-move-pages)

   (test-numa-migrate-pages)

   (test-numa-error)
   ))
