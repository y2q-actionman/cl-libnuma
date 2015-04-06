(in-package :cl-libnuma)

(defconstant +cl-libnuma-target-api-version+ 2
  "The API version of libnuma when cl-libnuma is coded. (2015-03-31)")

;;; Types

;; A representation of 'struct bitmask'
(define-foreign-type libnuma-bitmask-type ()
  ((specifying :initarg :specifying :initform nil :accessor libnuma-bitmask-type-specifying))
  (:actual-type :pointer))	 ; (:pointer (:struct struct-bitmask))

(define-parse-method libnuma-bitmask-type (&key (specifying nil))
  (ecase specifying
    ((:cpu :node nil)
     (make-instance 'libnuma-bitmask-type :specifying specifying))))

(deftype numa-bitmask ()
  "A lisp representation of 'struct bitmask' of libnuma"
  'bit-vector)

(defun make-numa-bitmask (&optional size-spec)
  (flet ((numa-bitmask-enough-size ()
	   (max (numa-num-possible-cpus)
		(numa-num-possible-nodes))))
    (let ((size (etypecase size-spec
		  (symbol (ecase size-spec
			    (:cpu (numa-num-configured-cpus))
			    (:node (numa-num-configured-nodes))
			    ((nil) (numa-bitmask-enough-size))))
		  (integer size-spec)
		  (null (numa-bitmask-enough-size)))))
      (make-array `(,size) :element-type 'bit
		  :initial-element 0)))) ; follows numa_bitmask_alloc()

(defmethod cffi:translate-to-foreign (lisp-bitmask (type libnuma-bitmask-type))
  (let ((bmp (ecase (libnuma-bitmask-type-specifying type)
	       (:cpu (numa-allocate-cpumask*))
	       (:node (numa-allocate-nodemask*))
	       ((nil) (numa-bitmask-alloc* (length lisp-bitmask))))))
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
		      ((nil) (* +CHAR-BIT+ (numa-bitmask-nbytes* bmp)))))
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

;; A representation of a return value from libc.
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


;;; Utils

(defmacro with-temporal-struct-bitmask-pointer ((var form) &body body)
  `(let ((,var ,form))
     (unwind-protect
	  (if (null-pointer-p ,var)
	      (error "allocating 'struct bitmask' was failed")
	      (progn ,@body))
       (numa-bitmask-free* ,var))))


;;; Funtions

(defcfun numa-available
    libc-return-boolean)


(defcfun numa-max-possible-node
    :int)

(defcfun numa-num-possible-nodes
    :int)

(defcfun (numa-num-possible-cpus* "numa_num_possible_cpus")
    :int)

(defun numa-num-possible-cpus ()
  (handler-case (numa-num-possible-cpus*)
    ;; This code assumes a simple-error is reported when the function
    ;; is not exported.
    (simple-error (condition)
      (declare (ignore condition))
      ;; A workaround for numa_num_possible_cpus(), which is not
      ;; exported until libnuma-2.0.8-rc4.
      (with-temporal-struct-bitmask-pointer (bmp (numa-allocate-cpumask*))
	(numa-bitmask-weight* (numa-bitmask-setall* bmp))))))


(defcfun numa-max-node
    :int)

(defcfun numa-num-configured-nodes
    :int)

(defcfun numa-get-mems-allowed
    (libnuma-bitmask-type :specifying :node))


(defcfun numa-num-configured-cpus
    :int)

(defcvar (*numa-all-nodes-bitmask* "numa_all_nodes_ptr" :read-only t)
    (libnuma-bitmask-type :specifying :node))

(defcvar (*numa-no-nodes-bitmask* "numa_no_nodes_ptr" :read-only t) 
    (libnuma-bitmask-type :specifying :node))

(defcvar (*numa-all-cpus-bitmask* "numa_all_cpus_ptr" :read-only t)
    (libnuma-bitmask-type :specifying :cpu))


(defcfun numa-num-task-cpus
    :int)

(defcfun numa-num-task-nodes
    :int)


(defcfun (numa-parse-bitmap* "numa_parse_bitmap")
    libc-return-boolean
  (line :string)
  (mask (:pointer (:struct struct-bitmask))))

(defun numa-parse-bitmap (line)
  (with-temporal-struct-bitmask-pointer (cpumask (numa-allocate-cpumask*))
    (if (numa-parse-bitmap* line cpumask)
	(convert-from-foreign cpumask '(libnuma-bitmask-type :specifying :cpu)))))

(defcfun numa-parse-nodestring
    (libnuma-bitmask-type :specifying :node)
  (string :string))

(defcfun numa-parse-nodestring-all
    (libnuma-bitmask-type :specifying :node)
  (string :string))

(defcfun numa-parse-cpustring
    (libnuma-bitmask-type :specifying :cpu)
  (string :string))

(defcfun numa-parse-cpustring-all
    (libnuma-bitmask-type :specifying :cpu)
  (string :string))


(defcfun (numa-node-size* "numa_node_size")
    :long
  (node :int)
  (freep (:pointer :long)))

(defun numa-node-size (node)
  (with-foreign-object (freep :long)
    (let ((size (numa-node-size* node freep)))
      (if (>= size 0)
	  (values size (mem-aref freep :long))))))
  
(defcfun (numa-node-size64* "numa_node_size64")
    :long-long
  (node :int)
  (freep (:pointer :long-long)))

(defun numa-node-size64 (node)
  (with-foreign-object (freep :long-long)
    (let ((size (numa-node-size64* node freep)))
      (if (>= size 0)
	  (values size (mem-aref freep :long-long))))))


(defcfun numa-preferred
    :int)

(defcfun (numa-set-preferred* "numa_set_preferred")
    :void
  (node :int))

(defun numa-set-preferred (node)
  (etypecase node
    (symbol
     (ecase node
       ((:local) (numa-set-preferred* -1))))
    (integer
     (numa-set-preferred* node))))

(defcfun numa-get-interleave-node
    :int)

(defcfun numa-get-interleave-mask
    (libnuma-bitmask-type :specifying :node))

(defcfun numa-set-interleave-mask
    :void
  (nodemask (libnuma-bitmask-type :specifying :node)))

(defcfun numa-interleave-memory
    :void
  (start :pointer)
  (size size_t)
  (nodemask (libnuma-bitmask-type :specifying :node)))

(defcfun numa-bind
    :void
  (nodemask (libnuma-bitmask-type :specifying :node)))

(defcfun numa-set-localalloc
    :void)

(defcfun numa-set-membind
    :void
  (nodemask (libnuma-bitmask-type :specifying :node)))

(defcfun numa-get-membind
    (libnuma-bitmask-type :specifying :node))

  
(defcfun numa-alloc-onnode
    malloc-pointer
  (size size_t)
  (node :int))				; if out-of-range, this will fail.

(defcfun numa-alloc-local
    malloc-pointer
  (size size_t))

(defcfun numa-alloc-interleaved
    malloc-pointer
  (size size_t))

(defcfun numa-alloc-interleaved-subset
    malloc-pointer
  (size size_t)
  (nodemask (libnuma-bitmask-type :specifying :node)))

(defcfun numa-alloc
    malloc-pointer
  (size size_t))

(defcfun numa-realloc
    malloc-pointer
  (old-addr malloc-pointer)
  (old-size size_t)
  (new-size size_t))

(defcfun numa-free
    :void
  (start malloc-pointer)
  (size size_t))


(defcfun (numa-run-on-node* "numa_run_on_node")
    libc-return-boolean
  (node :int))

(defun numa-run-on-node (node)
  (typecase node
    (symbol
     (ecase node
       ((:all) (numa-run-on-node* -1))))
    (integer
     (numa-run-on-node* node))))

(defcfun numa-run-on-node-mask
    libc-return-boolean
  (nodemask (libnuma-bitmask-type :specifying :node)))

(defcfun (numa-run-on-node-mask-all* "numa_run_on_node_mask_all")
    libc-return-boolean
  (nodemask (libnuma-bitmask-type :specifying :node)))

(defun numa-run-on-node-mask-all (nodemask)
  (handler-case (numa-run-on-node-mask-all* nodemask)
    ;; This code assumes a simple-error is reported when the function
    ;; is not found.
    (simple-error (condition)
      ;; This function is added at 2.0.9-rc3
      (warn "This libnuma does not have numa_run_on_node_mask_all(), added at libnuma-2.0.9-rc3.")
      (error condition))))

(defcfun numa-get-run-node-mask
    (libnuma-bitmask-type :specifying :node))


(defcfun numa-tonode-memory
    :void
  (start :pointer)
  (size size_t)
  (node :int))				; if out-of-range, this will fail

(defcfun numa-tonodemask-memory
    :void
  (start :pointer)
  (size size_t)
  (nodemask (libnuma-bitmask-type :specifying :node)))

(defcfun numa-setlocal-memory
    :void
  (start :pointer)
  (size size_t))

(defcfun numa-police-memory
    :void
  (start :pointer)
  (size size_t))

(defcfun numa-set-bind-policy
    :void
  (strict? (:boolean :int)))

(defcfun numa-set-strict
    :void
  (strict? (:boolean :int)))


(defcfun numa-distance
    libc-return-int
  (node1 :int)
  (node2 :int))


(defcfun (numa-sched-getaffinity* "numa_sched_getaffinity")
    libc-return-boolean
  (pid pid_t)
  (mask (:pointer (:struct struct-bitmask))))

(defun numa-sched-getaffinity (pid)
  (with-temporal-struct-bitmask-pointer (bmp (numa-allocate-cpumask*))
    (if (numa-sched-getaffinity* pid bmp)
	(convert-from-foreign bmp '(libnuma-bitmask-type :specifying :cpu)))))

(defcfun numa-sched-setaffinity
    libc-return-boolean
  (pid pid_t)
  (mask (libnuma-bitmask-type :specifying :cpu)))

(defcfun (numa-node-to-cpus* "numa_node_to_cpus")
    libc-return-boolean
  (node :int)
  (mask (:pointer (:struct struct-bitmask))))

(defun numa-node-to-cpus (node)
  (with-temporal-struct-bitmask-pointer (bmp (numa-allocate-cpumask*))
    (if (numa-node-to-cpus* node bmp)
	(convert-from-foreign bmp '(libnuma-bitmask-type :specifying :cpu)))))

(defcfun numa-node-of-cpu
    libc-return-int
  (cpu :int))


(defcfun (numa-allocate-cpumask* "numa_allocate_cpumask")
    (:pointer (:struct struct-bitmask))) ; should be freed with numa_free_cpumask()

;; Lisp API is make-numa-bitmask

;; numa_free_cpumask() is defined in the wrapper.

(defcfun (numa-allocate-nodemask* "numa_allocate_nodemask")
    (:pointer (:struct struct-bitmask))) ; should be freed with numa_free_nodemask()

;; Lisp API is make-numa-bitmask

;; numa_free_nodemask() is defined in the wrapper.


(defcfun (numa-bitmask-alloc* "numa_bitmask_alloc")
    (:pointer (:struct struct-bitmask)) ; should be freed with numa_bitmask_free()
  (n :unsigned-int))

(defcfun (numa-bitmask-clearall* "numa_bitmask_clearall")
    (:pointer (:struct struct-bitmask))
  (bmp (:pointer (:struct struct-bitmask))))

(defun numa-bitmask-clearall (lisp-bitmask)
  (fill lisp-bitmask 0)
  lisp-bitmask)

(defcfun (numa-bitmask-clearbit* "numa_bitmask_clearbit")
    (:pointer (:struct struct-bitmask))
  (bmp (:pointer (:struct struct-bitmask)))
  (n :unsigned-int))

(defun numa-bitmask-clearbit (lisp-bitmask n)
  (setf (bit lisp-bitmask n) 0)
  lisp-bitmask)

(defcfun (numa-bitmask-equal* "numa_bitmask_equal")
    (:boolean :int)
  (bmp1 (:pointer (:struct struct-bitmask)))
  (bmp2 (:pointer (:struct struct-bitmask))))

(defun numa-bitmask-equal (lisp-bitmask1 lisp-bitmask2)
  (equal lisp-bitmask1 lisp-bitmask2))

(defcfun (numa-bitmask-free* "numa_bitmask_free")
    :void
  (bmp (:pointer (:struct struct-bitmask))))

;; No 'numa-bitmask-free' for lisp-bitmask.

(defcfun (numa-bitmask-isbitset* "numa_bitmask_isbitset")
    :boolean
  (bmp (:pointer (:struct struct-bitmask)))
  (n :unsigned-int))

(defun numa-bitmask-isbitset (lisp-bitmask n)
  (= 1 (bit lisp-bitmask n)))

(defcfun (numa-bitmask-nbytes* "numa_bitmask_nbytes")
    :unsigned-int
  (bmp (:pointer (:struct struct-bitmask))))

(defun numa-bitmask-nbytes (lisp-bitmask)
  (ceiling (length lisp-bitmask) +CHAR-BIT+))

(defcfun (numa-bitmask-setall* "numa_bitmask_setall")
    (:pointer (:struct struct-bitmask))
  (bmp (:pointer (:struct struct-bitmask))))

(defun numa-bitmask-setall (lisp-bitmask)
  (fill lisp-bitmask 1)
  lisp-bitmask)

(defcfun (numa-bitmask-setbit* "numa_bitmask_setbit")
    (:pointer (:struct struct-bitmask))
  (bmp (:pointer (:struct struct-bitmask)))
  (n :unsigned-int))

(defun numa-bitmask-setbit (lisp-bitmask n)
  (setf (bit lisp-bitmask n) 1)
  lisp-bitmask)

(defcfun (copy-bitmask-to-nodemask* "copy_bitmask_to_nodemask")
    :void
  (bmp (:pointer (:struct struct-bitmask)))
  (nodemask (:pointer (:struct nodemask_t))))

(defcfun (copy-nodemask-to-bitmask* "copy_nodemask_to_bitmask")
    :void
  (nodemask (:pointer (:struct nodemask_t)))
  (bmp (:pointer (:struct struct-bitmask))))

(defcfun (copy-bitmask-to-bitmask* "copy_bitmask_to_bitmask")
    :void
  (bmpfrom (:pointer (:struct struct-bitmask)))
  (bmpto (:pointer (:struct struct-bitmask))))

(defun copy-bitmask-to-bitmask (lisp-bitmask-from lisp-bitmask-to)
  (map-into lisp-bitmask-to #'identity lisp-bitmask-from))

(defcfun (numa-bitmask-weight* "numa_bitmask_weight")
    :unsigned-int
  (bmp (:pointer (:struct struct-bitmask))))

(defun numa-bitmask-weight (lisp-bitmask)
  (reduce #'+ lisp-bitmask))


(defcfun (numa-move-pages* "numa_move_pages")
    libc-return-boolean
  (pid :int)
  (count :unsigned-long)
  (pages (:pointer :pointer))
  (nodes (:pointer :int))
  (status (:pointer :int))
  (flags mbind-flag))

(defun numa-move-pages (pid pages nodes &optional (flags :MPOL-MF-MOVE))
  (let* ((pages-length (length pages))
	 (nodes-length (length nodes))
	 (count (cond ((null nodes)
		       pages-length)
		      ((= pages-length nodes-length)
		       pages-length)
		      (t
		       (error "The length of 'pages' and 'nodes' should be same.")))))
    (with-foreign-objects ((pages-array :pointer count)
			   (nodes-array :int (if nodes count 0))
			   (status-array :int count))
      (let ((index 0))
	(map nil (lambda (page)
		   (setf (mem-aref pages-array :pointer index) page)
		   (incf index))
	     pages))
      (when nodes
	(let ((index 0))
	  (map nil (lambda (node)
		     (setf (mem-aref pages-array :int index) node)
		     (incf index))
	       nodes)))
      (if (numa-move-pages* pid count pages-array
			    (if nodes nodes-array (null-pointer))
			    status-array flags)
	  (loop for i from 0 below count
	     collect (mem-aref status-array :int i))))))
	  

(defcfun numa-migrate-pages
    libc-return-int
  (pid :int)
  (fromnodes (libnuma-bitmask-type :specifying :node))
  (tonodes (libnuma-bitmask-type :specifying :node)))


(defcfun numa-error
    :void
  (where :string))

(defcvar *numa-exit-on-error*
    (:boolean :int))

(defcvar *numa-exit-on-warn*
    (:boolean :int))

(defcfun numa-warn
    :void
  (number :int)
  (where :string)
  &rest)


(defcfun numa-pagesize
    :int)
