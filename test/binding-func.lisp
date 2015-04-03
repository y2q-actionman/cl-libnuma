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
	    (assert-progn
	     (integerp size) (integerp free)
	     (integerp size64) (integerp free64))
	    (when (<= size64 most-positive-fixnum)
	      (assert (= size size64)))
	    (when (<= free64 most-positive-fixnum)
	      (assert (= free free64))))))
  ;; error case
  (assert-progn
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
  (numa-set-localalloc)
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

(defun test-numa-interleave-memory (&optional (scale 16))
  (let* ((size (* (numa-pagesize) scale))
	 (memory (numa-alloc size)))
    (unless memory
      (warn "numa-alloc failed")
      (return-from test-numa-interleave-memory t))
    (unwind-protect
	 (assert-progn
	  (when (find 1 (numa-get-interleave-mask))
	    (numa-interleave-memory memory size (numa-get-interleave-mask)))
	  (numa-interleave-memory memory size *numa-all-nodes-bitmask*)
	  (not (numa-interleave-memory memory size *numa-no-nodes-bitmask*)))
      (numa-free memory size)))
  t)

;; TODO: On error cases, checks whether numa_error() was called or not.
(defun test-numa-bind ()
  (let ((orig-mask (numa-get-run-node-mask)))
    (unwind-protect
	 (progn
	   (numa-bind *numa-all-nodes-bitmask*)
	   (numa-bind *numa-no-nodes-bitmask*)) ; causes error
      (numa-bind orig-mask)))
  t)

;; TODO: On error cases, checks whether numa_error() was called or not.
(defun test-numa-membind ()
  (let ((allowed-mask (numa-get-mems-allowed))
	(orig-mask (numa-get-membind)))
    (assert (typep orig-mask 'numa-bitmask))
    (unwind-protect
	 (progn
	   ;; set the orig-mask again
	   (numa-set-membind orig-mask)
	   (assert (equal (numa-get-membind)
			  orig-mask))
	   ;; allows all available nodes
	   (numa-set-membind allowed-mask)
	   (assert (equal (numa-get-membind)
			  allowed-mask))
	   
	   ;; Error cases.
	   ;; error case 1: set the no-nodes mask.
	   (numa-set-membind *numa-no-nodes-bitmask*)
	   (assert (not (equal (numa-get-membind)
			       *numa-no-nodes-bitmask*)))
	   ;; error case 2: set an unavaiavle node.
	   (let ((unavaiavle-node (position 0 allowed-mask)))
	     (when unavaiavle-node
	       (let ((test-mask (make-numa-bitmask :node)))
		 (numa-bitmask-setbit test-mask unavaiavle-node)
		 (numa-set-membind test-mask)
		 (assert (not (equal (numa-get-membind)
				     test-mask)))))))
      (numa-set-membind orig-mask)))
  t)

(defun test-numa-alloc ()
  (flet ((test-alloc-read-write (name alloc-fn &optional (size (numa-pagesize)))
	   ;; This test does:
	   ;; - Try allocation.
	   ;; - Read and write from it.
	   ;; - free it.
	   (let ((mem (funcall alloc-fn size)))
	     (if (not mem)
		 (warn "~A call failed" name)
		 (unwind-protect
		      (assert
		       (= (loop for i from 0 below size
			     as n = (random 255)
			     do (setf (cffi:mem-aref mem :unsigned-char i) n)
			     sum n)
			  (loop for i from 0 below size
			     as n = (cffi:mem-aref mem :unsigned-char i)
			     sum n)))
		   (numa-free mem size))))))
    (loop for node from 0 below (numa-num-configured-nodes)
       do (test-alloc-read-write 'numa-alloc-onnode
				 #'(lambda (size)
				     (numa-alloc-onnode size node))))
    (test-alloc-read-write 'numa-alloc-local
			   #'numa-alloc-local)
    (test-alloc-read-write 'numa-alloc-interleaved
			   #'numa-alloc-interleaved)
    (let ((allowed-mask (numa-get-mems-allowed)))
      (test-alloc-read-write 'numa-alloc-interleaved-subset
			     #'(lambda (size)
				 (numa-alloc-interleaved-subset size allowed-mask))))
    (test-alloc-read-write 'numa-alloc
			   #'numa-alloc))
  ;; numa-realloc
  (let ((size1 (numa-pagesize))
	(size2 (* 2 (numa-pagesize)))
	(sum-at-write 0)
	(sum-at-read 0)
	mem1 mem2)
    (unwind-protect
	 (block nil
	   ;; initializes
	   (setf mem1 (numa-alloc size1))
	   (unless mem1
	     (warn "numa-alloc failed")
	     (return))
	   (loop for i from 0 below size1
	      as n = (random 255)
	      do (setf (cffi:mem-aref mem1 :unsigned-char i) n)
	      do (incf sum-at-write n))
	   ;; realloc
	   (setf mem2 (numa-realloc mem1 size1 size2))
	   (unless mem2
	     (warn "numa-realloc failed")
	     (return))
	   (setf mem1 nil)
	   ;; checks whether values are preserved.
	   (loop for i from 0 below size1
	      as n = (cffi:mem-aref mem2 :unsigned-char i)
	      do (incf sum-at-read n))
	   (assert (= sum-at-write sum-at-read))
	   ;; writes newly allocated spaces
	   (loop for i from size1 below size2
	      as n = (random 255)
	      do (setf (cffi:mem-aref mem2 :unsigned-char i) n)
	      do (incf sum-at-write n))
	   ;; check results
	   (setf sum-at-read 0)
	   (loop for i from 0 below size2
	      as n = (cffi:mem-aref mem2 :unsigned-char i)
	      do (incf sum-at-read n))
	   (assert (= sum-at-write sum-at-read)))
      (when mem1
	(numa-free mem1 size1))
      (when mem2
	(numa-free mem2 size2))))
  t)

(defun test-numa-run-on ()
  (let ((orig-mask (numa-get-run-node-mask)))
    (unwind-protect
	 (progn
	   (assert (typep orig-mask 'numa-bitmask))
	   (loop for node from 0 below (numa-num-configured-nodes)
	      when (numa-bitmask-isbitset orig-mask node)
	      do (assert (numa-run-on-node node)))
	   (assert (numa-run-on-node -1)) ; all nodes
	   (macrolet ((test-run-on-node-mask (funcname)
			`(progn
			   (loop with tmp-mask = (make-numa-bitmask :node)
			      for node from 0 below (numa-num-configured-nodes)
			      do (numa-bitmask-clearall tmp-mask)
				(numa-bitmask-setbit tmp-mask node)
				(assert (,funcname tmp-mask)))
			   (assert (not (,funcname *numa-no-nodes-bitmask*))) ; no nodes -- error
			   (assert (,funcname *numa-all-nodes-bitmask*))))) ; all nodes
	     (test-run-on-node-mask numa-run-on-node-mask)
	     (multiple-value-bind (ret condition)
		 (ignore-errors
		   (numa-run-on-node-mask-all *numa-all-nodes-bitmask*))
	       (cond ((and (not ret) condition)
		      (warn "This libnuma does not supports numa_run_on_node_mask_all()"))
		     (t
		      (test-run-on-node-mask numa-run-on-node-mask-all))))))
      (numa-run-on-node-mask orig-mask)))
  t)

;; TODO: check numa_error() called at failure.
(defun test-numa-memory-location ()
  (let* ((size (numa-pagesize))
	 (memory (numa-alloc size)))
    (unless memory
      (warn "numa-alloc() failed")
      (return-from test-numa-memory-location))
    (unwind-protect 
	 (progn
	   (loop for node from 0 below (numa-num-configured-nodes)
	      do (numa-tonode-memory memory size node))
	   (loop with tmp-mask = (make-numa-bitmask :node)
	      for node from 0 below (numa-num-configured-nodes)
	      do (numa-bitmask-clearall tmp-mask)
		(numa-bitmask-setbit tmp-mask node)
		(numa-tonodemask-memory memory size tmp-mask))
	   (numa-setlocal-memory memory size)
	   (numa-police-memory memory size))
      (numa-free memory size)))
  t)

(defun test-numa-distance ()
  (loop with nodes = (numa-num-configured-nodes)
     for i from 0 below nodes
     do (loop for j from 0 below nodes
	   do (assert (integerp (numa-distance i j)))))
  t)

(defun test-numa-affinity ()
  (let ((orig-cpu-mask (numa-sched-getaffinity 0))) ; pid == 0, means the current process
    (assert (typep orig-cpu-mask 'numa-bitmask))
    (unwind-protect
	 (progn
	   ;; set again
	   (assert (numa-sched-setaffinity 0 orig-cpu-mask))
	   ;; all nodes
	   (assert (numa-sched-setaffinity 0 *numa-all-cpus-bitmask*))
	   ;; drop a bit
	   (when (> (length orig-cpu-mask) 1)
	     (loop for i from 0 below (length orig-cpu-mask)
		as tmp-cpu-mask = (copy-seq orig-cpu-mask)
		do (numa-bitmask-clearbit tmp-cpu-mask i)
		  (assert (numa-sched-setaffinity 0 tmp-cpu-mask)))))
      (numa-sched-setaffinity 0 orig-cpu-mask)))
  t)

(defun test-numa-node-to/of-cpu ()
  ;; (assert (not (numa-node-to-cpus -1))) ; This causes Segfault in libnuma!!
  (assert (not (numa-node-to-cpus (numa-num-configured-nodes))))
  (assert (not (numa-node-of-cpu -1)))
  (assert (not (numa-node-of-cpu (numa-num-configured-cpus))))
  ;; node -> cpu -> node
  (loop for node from 0 below (numa-num-configured-nodes)
     as cpu-mask = (numa-node-to-cpus node)
     do (assert (typep cpu-mask 'numa-bitmask))
     do (loop for cpu from 0 below (length cpu-mask)
	   when (numa-bitmask-isbitset cpu-mask cpu)
	   do (assert (= node (numa-node-of-cpu cpu)))))
  ;; cpu -> node -> cpu
  (loop for cpu from 0 below (numa-num-configured-cpus)
     as node = (numa-node-of-cpu cpu)
     do (assert (integerp node))
     do (let ((cpu-mask (numa-node-to-cpus node)))
	  (assert (numa-bitmask-isbitset cpu-mask cpu))))
  t)

(defun test-numa-bitmask ()
  (flet ((test-raw-mask (alloc-func)	; 'struct bitmask*' of libnuma
	   (let ((mask1 (funcall alloc-func))
		 (mask2 (funcall alloc-func)))
	     (unwind-protect
		  (progn
		    (when (or (null-pointer-p mask1)
			      (null-pointer-p mask2))
		      (warn "numa_bitmask_alloc failed")
		      (return-from test-raw-mask))
		    (assert (plusp (numa-bitmask-nbytes* mask1)))
		    (assert (zerop (numa-bitmask-weight*
				    (numa-bitmask-clearall* mask1))))
		    (assert (numa-bitmask-isbitset*
			     (numa-bitmask-setbit* mask1 0) 0))
		    (let ((filled-bits (numa-bitmask-weight*
					(numa-bitmask-setall* mask1))))
		      (assert (not (numa-bitmask-isbitset*
				    (numa-bitmask-clearbit* mask1 0) 0)))
		      (assert (= (1- filled-bits)
				 (numa-bitmask-weight* mask1))))
		    (assert (not (numa-bitmask-equal* mask1 mask2)))
		    (copy-bitmask-to-bitmask* mask1 mask2)
		    (assert (numa-bitmask-equal* mask1 mask2)))
	       (when mask1
		 (numa-bitmask-free* mask1))
	       (when mask2
		 (numa-bitmask-free* mask2)))))
	 (test-lisp-mask (size-spec)	; 'numa-bitmask' of cl-libnuma
	   (let ((mask1 (make-numa-bitmask size-spec))
		 (mask2 (make-numa-bitmask size-spec)))
	     (assert (plusp (numa-bitmask-nbytes mask1)))
	     (numa-bitmask-clearall mask1)
	     (assert (zerop (numa-bitmask-weight mask1)))
	     (numa-bitmask-setbit mask1 0)
	     (assert (numa-bitmask-isbitset mask1 0))
	     (numa-bitmask-setall mask1)
	     (let ((filled-bits (numa-bitmask-weight mask1)))
	       (numa-bitmask-clearbit mask1 0)
	       (assert (not (numa-bitmask-isbitset mask1 0)))
	       (assert (= (1- filled-bits)
			  (numa-bitmask-weight mask1))))
	     (assert (not (numa-bitmask-equal mask1 mask2)))
	     (map-into mask2 #'identity mask1)
	     (assert (numa-bitmask-equal mask1 mask2)))))
    (test-raw-mask #'numa-allocate-nodemask*)
    (test-raw-mask #'numa-allocate-cpumask*)
    (test-raw-mask #'(lambda () (numa-bitmask-alloc* 32)))
    (test-lisp-mask :cpu)
    (test-lisp-mask :node))
  ;; nodemask_t
  (let* ((lisp-mask1 (make-random-numa-bitmask :node))
	 (raw-mask1 (convert-to-foreign lisp-mask1 '(numa-bitmask-type :specifying :node)))
	 (raw-mask2 (numa-allocate-nodemask*))
	 lisp-mask2)
    (unwind-protect
	 (with-foreign-object (nodemask1 '(:struct cl-libnuma.grovel::nodemask_t))
	   (copy-bitmask-to-nodemask* raw-mask1 nodemask1)
	   (copy-nodemask-to-bitmask* nodemask1 raw-mask2)
	   (setf lisp-mask2
		 (convert-from-foreign raw-mask2 '(numa-bitmask-type :specifying :node)))
	   (assert (equal lisp-mask1 lisp-mask2)))
      (numa-free-nodemask* raw-mask2)
      (free-converted-object raw-mask1 'numa-bitmask-type nil)))
  t)

(defun test-numa-move-pages (&optional (count 16))
  (let ((pages (loop repeat count
		  when (numa-alloc (numa-pagesize))
		  collect it
		  else
		  do (warn "numa-alloc failed")
		    (return-from test-numa-move-pages t))))
    (unwind-protect
	 (let ((initial-status (numa-move-pages 0 pages nil)))
	   (assert initial-status)
	   (assert (= (length pages) (length initial-status)))
	   ;; touch pages
	   (loop for page in pages
	      do (setf (mem-aref page :unsigned-char 0) 0))
	   (let ((touched-status (numa-move-pages 0 pages nil)))
	     (assert touched-status)
	     (assert (= (length pages) (length touched-status)))
	     (loop for page in pages
		for i-stat in initial-status
		for t-stat in touched-status
		when (= i-stat 2)	; -ENOENT
		do (when (= t-stat 2)
		     (warn "page ~A was not paged (status = ~A), and touched, but the status was not changed (~A)"
			   page i-stat t-stat))))
	   ;; move pages
	   (let* ((a-node (position 1 (numa-get-mems-allowed)))
		  (target-nodes (make-list (length pages) :initial-element a-node))
		  (moved-status (numa-move-pages 0 pages target-nodes)))
	     (assert moved-status)
	     (assert (= (length pages) (length moved-status)))
	     (loop for page in pages
		for m-stat in moved-status
		unless (= m-stat a-node)
		do (warn "page ~A was not moved to node ~A (status = ~A)."
			 page a-node m-stat))))
      (loop for page in pages
	 do (numa-free page (numa-pagesize)))))
  t)

(defun test-numa-migrate-pages ()
  ;; This test has no way to restore pages..
  (let* ((node-mask (numa-get-mems-allowed))
	 (node1 (position 1 node-mask))
	 (node2 (position 1 node-mask :start (1+ node1))))
    (unless (and (integerp node1)
		 (integerp node2)
		 (< node1 node2))
      (warn "numa-migrate-pages cannot be properly tested on this environment.")
      (return-from test-numa-migrate-pages t))
    (let ((mask1 (make-numa-bitmask :node))
	  (mask2 (make-numa-bitmask :node)))
      (numa-bitmask-setbit mask1 node1)
      (numa-bitmask-setbit mask2 node2)
      (assert (numa-migrate-pages 0 mask1 mask2))))
  t)

(defun test-numa-error ()
  (macrolet ((try-call (switch-var func-name &rest func-args)
	       `(let ((old-value ,switch-var))
		  (unwind-protect
		       (always-success
			 (setf ,switch-var nil)
			 (,func-name ,@func-args))
		    (setf ,switch-var old-value)))))
    (assert-progn
     (typep *numa-exit-on-error* 'boolean)
     (typep *numa-exit-on-warn* 'boolean)
     (try-call *numa-exit-on-error*
	       numa-error "hoge")
     (try-call *numa-exit-on-warn*
	       numa-warn 0 "hoge"))))

(defun test-binding-func ()
  (assert-progn
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
   (test-numa-interleave-memory)
   (test-numa-bind)
   ;; numa-set-localalloc -- in test-numa-preferred
   (test-numa-membind)

   (test-numa-alloc)
   
   (test-numa-run-on)

   (test-numa-memory-location)
   (always-success
     (numa-set-bind-policy nil)
     (numa-set-bind-policy t))		; default is 'strict'
   (always-success
     (numa-set-strict t)
     (numa-set-strict nil))		; default is 'preferred'

   (test-numa-distance)

   (test-numa-affinity)
   (test-numa-node-to/of-cpu)

   (test-numa-bitmask)

   (test-numa-move-pages)

   (test-numa-migrate-pages)

   (test-numa-error)

   (integerp (numa-pagesize))
   ))
