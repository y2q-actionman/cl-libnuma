(in-package :cl-libnuma.test)

(defmacro if-numa-function-exists (test-form then-form &optional else-form)
  (let ((condition-var (gensym "condition"))
	(result-var (gensym "result")))
    `(handler-case (progn ,test-form)
       (not-found-warning (,condition-var)
	 (declare (ignorable ,condition-var))
	 ,(if else-form
	      else-form
	      `(format *error-output* "~&~A~%" ,condition-var)))
       (:no-error (&rest ,result-var)
	 (declare (ignore ,result-var))
	 ,then-form))))

(defun test-numa-parser ()
  ;; numa-parse-bitmap
  ;; This test fully depends on '/sys' filesystem.
  (loop for i from 0 below (numa-num-configured-nodes)
     as cpumap-path = (format nil "/sys/devices/system/node/node~D/cpumap" i)
     do (with-open-file (stream cpumap-path :direction :input)
	  (let ((cpumap-string (read-line stream)))
	    (assert (numa-parse-bitmap
		     (concatenate 'string cpumap-string "
"))))))
  (flet ((test-parse-string (func type)
	   (let ((num-max (ecase type
			    (:node (numa-num-configured-nodes))
			    (:cpu (numa-num-configured-cpus)))))
	     (assert-progn
	       (assert-when (eq type :node)
		 (numa-bitmask-equal (funcall func "")
				     *numa-no-nodes-bitmask*))
	       (numa-bitmask-equal (funcall func "all")
				   (ecase type
				     (:node *numa-all-nodes-bitmask*)
				     (:cpu *numa-all-cpus-bitmask*)))
	       (not (funcall func "??")))
	     ;; using 0
	     (when (<= 0 num-max)
	       (assert-progn
		 (typep (funcall func "0") 'numa-bitmask)
		 (typep (funcall func "!0") 'numa-bitmask)
		 (typep (funcall func "+0") 'numa-bitmask)
		 (typep (funcall func "0-0") 'numa-bitmask)))
	     ;; using 0, 1
	     (when (<= 1 num-max)
	       (assert-progn
		 (typep (funcall func "0,1") 'numa-bitmask)
		 (typep (funcall func "0-1") 'numa-bitmask)
		 (typep (funcall func "+0,1") 'numa-bitmask)
		 (typep (funcall func "+0-1") 'numa-bitmask))))))
    (test-parse-string #'numa-parse-nodestring :node)
    (if-numa-function-exists (numa-parse-nodestring-all "")
			     (test-parse-string #'numa-parse-nodestring-all :node))
    (test-parse-string #'numa-parse-cpustring :cpu)
    (if-numa-function-exists (numa-parse-cpustring-all "")
			     (test-parse-string #'numa-parse-cpustring-all :cpu)))
  t)

(defun test-numa-node-size ()
  (loop for node from 0 below (numa-num-configured-nodes)
     do (multiple-value-bind (size free) (numa-node-size node)
	  (multiple-value-bind (size64 free64) (numa-node-size64 node)
	    (assert-progn
	      (integerp size) (integerp free)
	      (integerp size64) (integerp free64)
	      (assert-when (<= size64 most-positive-fixnum)
		(= size size64))
	      (assert-when (<= free64 most-positive-fixnum)
		(= free free64))))))
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
  ;; special value: -1
  (numa-set-preferred :local)
  (assert (integerp (numa-preferred)))
  (numa-set-preferred -1)
  (assert (integerp (numa-preferred)))
  ;; out of range
  (let ((orig-preferred (numa-preferred)))
    (numa-set-preferred (numa-num-configured-nodes))
    (assert (= orig-preferred (numa-preferred))))
  ;; sets default -- numa-set-localalloc
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
	 ;; TODO: On error cases, checks whether numa_error() was called or not.
	 (progn
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
	   (if-numa-function-exists (setf mem2 (numa-realloc mem1 size1 size2))
				    (unless mem2
				      (warn "numa-realloc failed")
				      (return))
				    (return)) ; escapes from this block
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
	   (assert (numa-run-on-node :all)) ; all nodes
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
	     (if-numa-function-exists (numa-run-on-node-mask-all *numa-all-nodes-bitmask*)
				      (test-run-on-node-mask numa-run-on-node-mask-all))))
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
  (macrolet ((test-scenario (mask1-val mask2-val suffix)
	       (flet ((nb (name)
			(find-symbol (concatenate 'string "NUMA-BITMASK-" (symbol-name name) suffix) :cl-libnuma))
		      (copier ()
			(find-symbol (concatenate 'string "COPY-BITMASK-TO-BITMASK" suffix) :cl-libnuma)))
		 (let ((mask1 (gensym)) (mask2 (gensym)))
		   `(let ((,mask1 ,mask1-val)
			  (,mask2 ,mask2-val))
		      (assert (plusp (,(nb 'nbytes) ,mask1)))
		      (assert (zerop (,(nb 'weight)
		      		       (,(nb 'clearall) ,mask1))))
		      (assert (,(nb 'isbitset)
		      		(,(nb 'setbit) ,mask1 0) 0))
		      (let ((filled-bits (,(nb 'weight)
		      			   (,(nb 'setall) ,mask1))))
		      	(assert (not (,(nb 'isbitset)
		      		       (,(nb 'clearbit) ,mask1 0) 0)))
		      	(assert (= (1- filled-bits)
		      		   (,(nb 'weight) ,mask1))))
		      (assert (not (,(nb 'equal) ,mask1 ,mask2)))
		      (if-numa-function-exists (,(copier) ,mask1 ,mask2)
					       (assert (,(nb 'equal) ,mask1 ,mask2))))))))
    (flet ((test-raw-mask (alloc-func)	; 'struct bitmask*' of libnuma
	     (cl-libnuma::with-temporal-struct-bitmask-pointer (mask1 (funcall alloc-func))
	       (cl-libnuma::with-temporal-struct-bitmask-pointer (mask2 (funcall alloc-func))
		 (test-scenario mask1 mask2 "*"))))
	   (test-lisp-mask (size-spec)	; 'numa-bitmask' of cl-libnuma
	     (test-scenario (make-numa-bitmask size-spec) (make-numa-bitmask size-spec) "")))
      (test-raw-mask #'cl-libnuma::numa-allocate-nodemask*)
      (test-raw-mask #'cl-libnuma::numa-allocate-cpumask*)
      (test-raw-mask #'(lambda () (cl-libnuma::numa-bitmask-alloc* 32)))
      (test-lisp-mask :cpu)
      (test-lisp-mask :node)))
  ;; nodemask_t
  (let ((lisp-mask1 (make-random-numa-bitmask :node)))
    (cl-libnuma::with-temporal-struct-bitmask-pointer
	(raw-mask1 (convert-to-foreign lisp-mask1 '(libnuma-bitmask-type :specifying :node)))
      (cl-libnuma::with-temporal-struct-bitmask-pointer
	  (raw-mask2 (cl-libnuma::numa-allocate-nodemask*))
	(with-foreign-object (nodemask1 '(:struct cl-libnuma.grovel::nodemask_t))
	  (cl-libnuma::copy-bitmask-to-nodemask* raw-mask1 nodemask1)
	  (cl-libnuma::copy-nodemask-to-bitmask* nodemask1 raw-mask2)
	  (let ((lisp-mask2
		 (convert-from-foreign raw-mask2 '(libnuma-bitmask-type :specifying :node))))
	    (assert (equal lisp-mask1 lisp-mask2)))))))
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

    (typep *numa-exit-on-error* 'boolean)
    (typep *numa-exit-on-warn* 'boolean)

    (integerp (numa-pagesize))
    ))
