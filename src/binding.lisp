(in-package :cl-libnuma)

(defconstant +cl-libnuma-target-api-version+ 2)

;;; Types

;; A representation of 'struct bitmask'
(defctype struct-bitmask-pointer
    (:pointer (:struct struct-bitmask)))

(define-foreign-type numa-bitmask-type ()
  ((specifying :initarg :specifying :initform nil :accessor numa-bitmask-type-specifying))
  (:actual-type struct-bitmask-pointer))

(define-parse-method numa-bitmask-type (&key (specifying nil))
  (make-instance 'numa-bitmask-type :specifying specifying))

(deftype numa-bitmask ()
  "A lisp representation of 'struct bitmask' of libnuma"
  '(bit-vector *))

(defun numa-bitmask-enough-size ()
  (let ((cpumask-len (numa-num-possible-cpus))
	(nodemask-len (numa-num-possible-nodes)))
    (max cpumask-len nodemask-len)))

(defun make-numa-bitmask (&optional size-spec)
  (let ((size (etypecase size-spec
		(symbol (ecase size-spec
			  (:cpu (numa-num-configured-cpus))
			  (:node (numa-num-configured-nodes))
			  ((nil) (numa-bitmask-enough-size))))
		(integer size-spec)
		(null (numa-bitmask-enough-size)))))
    (make-array `(,size) :element-type 'bit
		:initial-element 0))) ; follows numa_bitmask_alloc()

(defmethod cffi:translate-to-foreign (lisp-bitmask (type numa-bitmask-type))
  (let ((bmp (ecase (numa-bitmask-type-specifying type)
	       (:cpu (numa-allocate-cpumask*))
	       (:node (numa-allocate-nodemask*))
	       ((nil) (numa-bitmask-alloc* (length lisp-bitmask))))))
    (loop for i across lisp-bitmask
       when (plusp i)
       do (numa-bitmask-setbit* bmp i))
    bmp))

(defmethod cffi:translate-from-foreign (bmp (type numa-bitmask-type))
  (when (null-pointer-p bmp)		; 'bmp' may be null if numa_parse_***() calls.
    (return-from cffi:translate-from-foreign nil))
  (let* ((size-spec (ecase (numa-bitmask-type-specifying type)
		      (:cpu :cpu)
		      (:node :node)
		      ((nil) (* CHAR_BIT (numa-bitmask-nbytes* bmp)))))
	 (lisp-bitmask (make-numa-bitmask size-spec)))
    (loop for i from 0 below (length lisp-bitmask)
       do (setf (aref lisp-bitmask i)
		(numa-bitmask-isbitset* bmp i)))
    lisp-bitmask))

(defmethod cffi:free-translated-object (bmp (type numa-bitmask-type) param)
  (declare (ignore param))
  (case (numa-bitmask-type-specifying type)
    (:cpu (numa-free-cpumask* bmp))
    (:node (numa-free-nodemask* bmp))
    (t (numa-bitmask-free* bmp))))


;; A representation of 'nodemask_t'
;; (This is an old interface of libnuma, so I don't implement anything
;; around it..)
(defctype nodemask_t-pointer
    (:pointer (:struct nodemask_t)))


;; A representation of 'int' returned from libc.
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
    (numa-bitmask-type :specifying :node))


(defcfun "numa_num_configured_cpus"
    :int)

(defcvar (*numa-all-nodes* "numa_all_nodes_ptr" :read-only t)
    (numa-bitmask-type :specifying :node))

(defcvar (*numa-no-nodes* "numa_no_nodes_ptr" :read-only t) 
    (numa-bitmask-type :specifying :node))

(defcvar (*numa-all-cpus* "numa_all_cpus_ptr" :read-only t)
    (numa-bitmask-type :specifying :cpu))


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
	(convert-from-foreign cpumask '(numa-bitmask-type :specifying :cpu)))))

(defcfun "numa_parse_nodestring"
    (numa-bitmask-type :specifying :node)
  (string :string))

(defcfun "numa_parse_nodestring_all"
    (numa-bitmask-type :specifying :node)
  (string :string))

(defcfun "numa_parse_cpustring"
    (numa-bitmask-type :specifying :cpu)
  (string :string))

(defcfun "numa_parse_cpustring_all"
    (numa-bitmask-type :specifying :cpu)
  (string :string))


(defcfun (numa-node-size* "numa_node_size")
    :long
  (node :int)
  (freep (:pointer :long)))

(defun numa-node-size (node)
  (with-foreign-object (freep :long 1)
    (let ((size (numa-node-size* node freep)))
      (if (>= size 0)
	  (values size (mem-aref freep :long 0))))))
  
(defcfun (numa-node-size64* "numa_node_size64")
    :long-long
  (node :int)
  (freep (:pointer :long-long)))

(defun numa-node-size64 (node)
  (with-foreign-object (freep :long-long 1)
    (let ((size (numa-node-size64* node freep)))
      (if (>= size 0)
	  (values size (mem-aref freep :long-long 0))))))


(defcfun "numa_preferred"
    :int)

(defcfun "numa_set_preferred"
    :void
  (node :int))

(defcfun "numa_get_interleave_node"
    :int)

(defcfun "numa_get_interleave_mask"
    (numa-bitmask-type :specifying :node))

(defcfun "numa_set_interleave_mask"
    :void
  (nodemask (numa-bitmask-type :specifying :node)))

(defcfun "numa_interleave_memory"
    :void
  (start :pointer)
  (size size_t)
  (nodemask (numa-bitmask-type :specifying :node)))

(defcfun "numa_bind"
    :void
  (nodemask (numa-bitmask-type :specifying :node)))

(defcfun "numa_set_localalloc"
    :void)

(defcfun "numa_set_membind"
    :void
  (nodemask (numa-bitmask-type :specifying :node)))

(defcfun "numa_get_membind"
    (numa-bitmask-type :specifying :node))

  
(defcfun "numa_alloc_onnode"
    :pointer
  (size size_t)
  (node :int))

(defcfun "numa_alloc_local"
    :pointer
  (size size_t))

(defcfun "numa_alloc_interleaved"
    :pointer
  (size size_t))

(defcfun "numa_alloc_interleaved_subset"
    :pointer
  (size size_t)
  (nodemask (numa-bitmask-type :specifying :node)))

(defcfun "numa_alloc"
    :pointer
  (size size_t))

(defcfun "numa_realloc"
    :pointer
  (old-addr :pointer)
  (old-size size_t)
  (new-size size_t))

(defcfun "numa_free"
    :void
  (start :pointer)
  (size size_t))


(defcfun "numa_run_on_node"
    libc-return-boolean
  (node :int))

(defcfun "numa_run_on_node_mask"
    libc-return-boolean
  (nodemask (numa-bitmask-type :specifying :node)))

(defcfun "numa_run_on_node_mask_all"
    libc-return-boolean
  (nodemask (numa-bitmask-type :specifying :node)))

(defcfun "numa_get_run_node_mask"
    (numa-bitmask-type :specifying :node))


(defcfun "numa_tonode_memory"
    :void
  (start :pointer)
  (size size_t)
  (node :int))

(defcfun "numa_tonodemask_memory"
    :void
  (start :pointer)
  (size size_t)
  (nodemask (numa-bitmask-type :specifying :node)))

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
  (strict :int))

(defcfun "numa_set_strict"
    :void
  (strict :int))


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
	(convert-from-foreign bmp '(numa-bitmask-type :specifying :cpu)))))

(defcfun "numa_sched_setaffinity"
    libc-return-boolean
  (pid pid_t)
  (mask (numa-bitmask-type :specifying :cpu)))

(defcfun (numa-node-to-cpus* "numa_node_to_cpus")
    libc-return-boolean
  (node :int)
  (mask struct-bitmask-pointer))

(defun numa-node-to-cpus (node)
  (with-temporal-struct-bitmask-pointer (bmp (numa-allocate-cpumask*))
    (if (numa-node-to-cpus* node bmp)
	(convert-from-foreign bmp '(numa-bitmask-type :specifying :cpu)))))

(defcfun "numa_node_of_cpu"
    libc-return-int
  (cpu :int))


(defcfun (numa-allocate-cpumask* "numa_allocate_cpumask")
    struct-bitmask-pointer) ; should be freed with numa_free_cpumask()
;; Lisp API is make-numa-bitmask

;; numa_free_cpumask() is defined in the wrapper.

(defcfun (numa-allocate-nodemask* "numa_allocate_nodemask")
    struct-bitmask-pointer) ; should be freed with numa_free_nodemask()
;; Lisp API is make-numa-bitmask

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
    :int				; not a boolean, for translate-from-foreign above.
  (bmp struct-bitmask-pointer)
  (n :unsigned-int))

(defun numa-bitmask-isbitset (lisp-bitmask n)
  (bit lisp-bitmask n))

(defcfun (numa-bitmask-nbytes* "numa_bitmask_nbytes")
    :unsigned-int
  (bmp struct-bitmask-pointer))

(defun numa-bitmask-nbytes (lisp-bitmask)
  (ceiling (length lisp-bitmask) CHAR_BIT))

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
  (flags :int))

;; TODO: support nodes-list as a bitvector-type
(defun numa-move-pages (pid count pages-list nodes-list &optional (flags MPOL_MF_MOVE))
  (let ((pages-list-length (length pages-list))
	(nodes-list-length (length nodes-list)))
    (with-foreign-objects ((pages-array :pointer pages-list-length)
			   (nodes-array :int nodes-list-length)
			   (status-array :int pages-list-length))
      (loop for i from 0
	 for p in pages-list
	 do (setf (mem-aref pages-array :pointer i) p))
      (loop for i from 0
	 for n in nodes-list
	 do (setf (mem-aref nodes-array :int i) n))
      (if (numa-move-pages* pid count pages-array nodes-array
			    status-array flags)
	  (loop for i from 0 below pages-list-length
	     collect (mem-aref status-array :int i))))))
	  

(defcfun "numa_migrate_pages"
    libc-return-int
  (pid :int)
  (fromnodes (numa-bitmask-type :specifying :node))
  (tonodes (numa-bitmask-type :specifying :node)))


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
