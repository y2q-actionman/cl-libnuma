(in-package :cl-libnuma)

(defconstant +cl-libnuma-target-api-version+ 2
  "The API version of libnuma when cl-libnuma is coded. (2015-03-31)")

;;; Types

;; A representation of 'struct bitmask'
(defctype struct-bitmask-pointer
    (:pointer (:struct struct-bitmask)))

(define-foreign-type libnuma-bitmask-type ()
  ((specifying :initarg :specifying :initform nil :accessor libnuma-bitmask-type-specifying))
  (:actual-type :pointer))	 ; (:pointer (:struct struct-bitmask))

(define-parse-method libnuma-bitmask-type (&key (specifying nil))
  (ecase specifying
    ((:cpu :node nil)
     (make-instance 'libnuma-bitmask-type :specifying specifying))))

(deftype numa-bitmask ()
  "A lisp representation of 'struct bitmask' of libnuma"
  '(bit-vector *))

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


;; A representation of 'nodemask_t'
;; (This is an old interface of libnuma, so I don't implement anything
;; around it..)
(defctype nodemask_t-pointer
    (:pointer (:struct nodemask_t)))
;; TODO: is this typedef required?

;; A representation of a return value from libc.
(defun lisp-to-libc-return-*** (value)
  (assert nil
	  ()
	  "lisp-to-libc-return-*** should not be called (with ~A)"
	  value))

(defun lisp-from-libc-return-boolean (value)
  (declare (type fixnum value))
  (>= value 0))

(defctype libc-return-boolean 
    (:wrapper :int
	      :to-c lisp-to-libc-return-***
	      :from-c lisp-from-libc-return-boolean))

(defun lisp-from-libc-return-int (value)
  (declare (type fixnum value))
  (if (>= value 0) value nil))

(defctype libc-return-int
    (:wrapper :int
	      :to-c lisp-to-libc-return-***
	      :from-c lisp-from-libc-return-int))

(defun lisp-to-malloc-pointer (value)
  (if value value (null-pointer)))

(defun lisp-from-malloc-pointer (value)
  (if (null-pointer-p value) nil value))

(defctype malloc-pointer
    (:wrapper :pointer
	      :to-c lisp-to-malloc-pointer
	      :from-c lisp-from-malloc-pointer)
  "This type is a wrapper for a pointer except treats NIL as a null pointer.")


;;; Utils

(defmacro with-temporal-struct-bitmask-pointer ((var form) &body body)
  `(let ((,var ,form))
     (unwind-protect (progn ,@body)
       (numa-bitmask-free* ,var))))


;;; Funtions

(defcfun "numa_available"
    libc-return-boolean)


(defcfun "numa_max_possible_node"
    :int)

(defcfun "numa_num_possible_nodes"
    :int)

(defcfun (numa-num-possible-cpus* "numa_num_possible_cpus")
    :int)

;; A workaround for numa_num_possible_cpus(), which is not exported
;; until libnuma-2.0.8-rc4.
;; ( http://www.spinics.net/lists/linux-numa/msg00948.html )
;; Get the value from an allocated cpumask.
(let ((possible-cpus nil))
  (defun numa-num-possible-cpus-alternative ()
    (unless possible-cpus
      (with-temporal-struct-bitmask-pointer (bmp (numa-allocate-cpumask*))
	(setf possible-cpus
	      (numa-bitmask-weight* (numa-bitmask-setall* bmp)))))
    possible-cpus))

(defun numa-num-possible-cpus ()
  (handler-case (numa-num-possible-cpus*)
    ;; This code assumes a simple-error is reported when the function is not exported.
    (simple-error (condition)
      (declare (ignore condition))
      (numa-num-possible-cpus-alternative))))


(defcfun "numa_max_node"
    :int)

(defcfun "numa_num_configured_nodes"
    :int)

(defcfun "numa_get_mems_allowed"
    (libnuma-bitmask-type :specifying :node))


(defcfun "numa_num_configured_cpus"
    :int)

(defcvar (*numa-all-nodes-bitmask* "numa_all_nodes_ptr" :read-only t)
    (libnuma-bitmask-type :specifying :node))

(defcvar (*numa-no-nodes-bitmask* "numa_no_nodes_ptr" :read-only t) 
    (libnuma-bitmask-type :specifying :node))

(defcvar (*numa-all-cpus-bitmask* "numa_all_cpus_ptr" :read-only t)
    (libnuma-bitmask-type :specifying :cpu))


(defcfun "numa_num_task_cpus"
    :int)

(defcfun "numa_num_task_nodes"
    :int)


(defcfun (numa-parse-bitmap* "numa_parse_bitmap")
    libc-return-boolean
  (line :string)
  (mask struct-bitmask-pointer))

(defun numa-parse-bitmap (line)
  (with-temporal-struct-bitmask-pointer (cpumask (numa-allocate-cpumask*))
    (if (numa-parse-bitmap* line cpumask)
	(convert-from-foreign cpumask '(libnuma-bitmask-type :specifying :cpu)))))

(defcfun "numa_parse_nodestring"
    (libnuma-bitmask-type :specifying :node)
  (string :string))

(defcfun "numa_parse_nodestring_all"
    (libnuma-bitmask-type :specifying :node)
  (string :string))

(defcfun "numa_parse_cpustring"
    (libnuma-bitmask-type :specifying :cpu)
  (string :string))

(defcfun "numa_parse_cpustring_all"
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


(defcfun "numa_preferred"
    :int)

(defcfun "numa_set_preferred"
    :void
  (node :int))				; TODO: should wrap '-1' ?

(defcfun "numa_get_interleave_node"
    :int)

(defcfun "numa_get_interleave_mask"
    (libnuma-bitmask-type :specifying :node))

(defcfun "numa_set_interleave_mask"
    :void
  (nodemask (libnuma-bitmask-type :specifying :node)))

(defcfun "numa_interleave_memory"
    :void
  (start :pointer)
  (size size_t)
  (nodemask (libnuma-bitmask-type :specifying :node)))

(defcfun "numa_bind"
    :void
  (nodemask (libnuma-bitmask-type :specifying :node)))

(defcfun "numa_set_localalloc"
    :void)

(defcfun "numa_set_membind"
    :void
  (nodemask (libnuma-bitmask-type :specifying :node)))

(defcfun "numa_get_membind"
    (libnuma-bitmask-type :specifying :node))

  
(defcfun "numa_alloc_onnode"
    malloc-pointer
  (size size_t)
  (node :int))				; if out-of-range, this will fail.

(defcfun "numa_alloc_local"
    malloc-pointer
  (size size_t))

(defcfun "numa_alloc_interleaved"
    malloc-pointer
  (size size_t))

(defcfun "numa_alloc_interleaved_subset"
    malloc-pointer
  (size size_t)
  (nodemask (libnuma-bitmask-type :specifying :node)))

(defcfun "numa_alloc"
    malloc-pointer
  (size size_t))

(defcfun "numa_realloc"
    malloc-pointer
  (old-addr malloc-pointer)
  (old-size size_t)
  (new-size size_t))

(defcfun "numa_free"
    :void
  (start malloc-pointer)
  (size size_t))


(defcfun "numa_run_on_node"
    libc-return-boolean
  (node :int))				; TODO: should wrap '-1'?

(defcfun "numa_run_on_node_mask"
    libc-return-boolean
  (nodemask (libnuma-bitmask-type :specifying :node)))

;; TODO: check libnuma version.
;; This is added at 2013-09-10
;; http://permalink.gmane.org/gmane.linux.kernel.numa/832
(defcfun "numa_run_on_node_mask_all"
    libc-return-boolean
  (nodemask (libnuma-bitmask-type :specifying :node)))

(defcfun "numa_get_run_node_mask"
    (libnuma-bitmask-type :specifying :node))


(defcfun "numa_tonode_memory"
    :void
  (start :pointer)
  (size size_t)
  (node :int))				; if out-of-range, this will fail

(defcfun "numa_tonodemask_memory"
    :void
  (start :pointer)
  (size size_t)
  (nodemask (libnuma-bitmask-type :specifying :node)))

(defcfun "numa_setlocal_memory"
    :void
  (start :pointer)
  (size size_t))

(defcfun "numa_police_memory"
    :void
  (start :pointer)
  (size size_t))

(defcfun "numa_set_bind_policy"
    :void
  (strict? (:boolean :int)))

(defcfun "numa_set_strict"
    :void
  (strict? (:boolean :int)))


;; TODO: return nil if this returns 0, on error.
(defcfun "numa_distance"
    :int
  (node1 :int)
  (node2 :int))


(defcfun (numa-sched-getaffinity* "numa_sched_getaffinity")
    libc-return-boolean
  (pid pid_t)
  (mask struct-bitmask-pointer))

(defun numa-sched-getaffinity (pid)
  (with-temporal-struct-bitmask-pointer (bmp (numa-allocate-cpumask*))
    (if (numa-sched-getaffinity* pid bmp)
	(convert-from-foreign bmp '(libnuma-bitmask-type :specifying :cpu)))))

(defcfun "numa_sched_setaffinity"
    libc-return-boolean
  (pid pid_t)
  (mask (libnuma-bitmask-type :specifying :cpu)))

(defcfun (numa-node-to-cpus* "numa_node_to_cpus")
    libc-return-boolean
  (node :int)
  (mask struct-bitmask-pointer))

(defun numa-node-to-cpus (node)
  (with-temporal-struct-bitmask-pointer (bmp (numa-allocate-cpumask*))
    (if (numa-node-to-cpus* node bmp)
	(convert-from-foreign bmp '(libnuma-bitmask-type :specifying :cpu)))))

(defcfun "numa_node_of_cpu"
    libc-return-int
  (cpu :int))


(defcfun (numa-allocate-cpumask* "numa_allocate_cpumask")
    struct-bitmask-pointer) ; should be freed with numa_free_cpumask()

;; Lisp API is make-numa-bitmask
;; TODO: should add this?
#+ignore
(defun numa-allocate-cpumask ()
  (make-numa-bitmask :cpu))

;; numa_free_cpumask() is defined in the wrapper.

(defcfun (numa-allocate-nodemask* "numa_allocate_nodemask")
    struct-bitmask-pointer) ; should be freed with numa_free_nodemask()

;; Lisp API is make-numa-bitmask
;; TODO: should add this?
#+ignore
(defun numa-allocate-nodemask ()
  (make-numa-bitmask :node))

;; numa_free_nodemask() is defined in the wrapper.


(defcfun (numa-bitmask-alloc* "numa_bitmask_alloc")
    struct-bitmask-pointer ; should be freed with numa_bitmask_free()
  (n :unsigned-int))

(defcfun (numa-bitmask-clearall* "numa_bitmask_clearall")
    struct-bitmask-pointer
  (bmp struct-bitmask-pointer))

(defun numa-bitmask-clearall (lisp-bitmask)
  (fill lisp-bitmask 0))

(defcfun (numa-bitmask-clearbit* "numa_bitmask_clearbit")
    struct-bitmask-pointer
  (bmp struct-bitmask-pointer)
  (n :unsigned-int))

(defun numa-bitmask-clearbit (lisp-bitmask n)
  (setf (bit lisp-bitmask n) 0))

(defcfun (numa-bitmask-equal* "numa_bitmask_equal")
    (:boolean :int)
  (bmp1 struct-bitmask-pointer)
  (bmp2 struct-bitmask-pointer))

(defun numa-bitmask-equal (lisp-bitmask1 lisp-bitmask2)
  (equal lisp-bitmask1 lisp-bitmask2))

(defcfun (numa-bitmask-free* "numa_bitmask_free")
    :void
  (bmp struct-bitmask-pointer))

;; No 'numa-bitmask-free' for lisp-bitmask.

(defcfun (numa-bitmask-isbitset* "numa_bitmask_isbitset")
    :boolean
  (bmp struct-bitmask-pointer)
  (n :unsigned-int))

(defun numa-bitmask-isbitset (lisp-bitmask n)
  (= 1 (bit lisp-bitmask n)))

(defcfun (numa-bitmask-nbytes* "numa_bitmask_nbytes")
    :unsigned-int
  (bmp struct-bitmask-pointer))

(defun numa-bitmask-nbytes (lisp-bitmask)
  (ceiling (length lisp-bitmask) +CHAR-BIT+))

(defcfun (numa-bitmask-setall* "numa_bitmask_setall")
    struct-bitmask-pointer
  (bmp struct-bitmask-pointer))

(defun numa-bitmask-setall (lisp-bitmask)
  (fill lisp-bitmask 1))

(defcfun (numa-bitmask-setbit* "numa_bitmask_setbit")
    struct-bitmask-pointer
  (bmp struct-bitmask-pointer)
  (n :unsigned-int))

(defun numa-bitmask-setbit (lisp-bitmask n)
  (setf (bit lisp-bitmask n) 1))

(defcfun (copy-bitmask-to-nodemask* "copy_bitmask_to_nodemask")
    :void
  (bmp struct-bitmask-pointer)
  (nodemask nodemask_t-pointer))

(defcfun (copy-nodemask-to-bitmask* "copy_nodemask_to_bitmask")
    :void
  (nodemask nodemask_t-pointer)
  (bmp struct-bitmask-pointer))

(defcfun (copy-bitmask-to-bitmask* "copy_bitmask_to_bitmask")
    :void
  (bmpfrom struct-bitmask-pointer)
  (bmpto struct-bitmask-pointer))

(defcfun (numa-bitmask-weight* "numa_bitmask_weight")
    :unsigned-int
  (bmp struct-bitmask-pointer))

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
      (map nil (let ((index 0))
		 (lambda (page)
		   (setf (mem-aref pages-array :pointer index) page)
		   (incf index)))
	   pages)
      (when nodes
	(map nil (let ((index 0))
		   (lambda (node)
		     (setf (mem-aref pages-array :int index) node)
		     (incf index)))
	     nodes))
      (if (numa-move-pages* pid count pages-array
			    (if nodes nodes-array (null-pointer))
			    status-array flags)
	  (loop for i from 0 below count
	     collect (mem-aref status-array :int i))))))
	  

(defcfun "numa_migrate_pages"
    libc-return-int
  (pid :int)
  (fromnodes (libnuma-bitmask-type :specifying :node))
  (tonodes (libnuma-bitmask-type :specifying :node)))


(defcfun "numa_error"
    :void
  (where :string))

(defcvar "numa_exit_on_error"
    (:boolean :int))

(defcvar "numa_exit_on_warn"
    (:boolean :int))

(defcfun "numa_warn"
    :void
  (number :int)
  (where :string)
  &rest)


(defcfun "numa_pagesize"
    :int)
