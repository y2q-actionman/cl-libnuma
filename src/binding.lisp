(in-package :cl-libnuma)

(defconstant +cl-libnuma-target-api-version+ 2)

;;; Types

;; A representation for 'struct bitmask'
(defctype struct-bitmask-pointer
    (:pointer (:struct struct-bitmask)))

(define-foreign-type bitmask-type ()
  ((specifying :initarg :specifying :initform nil :accessor bitmask-type-specifying))
  (:actual-type struct-bitmask-pointer))

(define-parse-method bitmask-type (&key (specifying nil))
  (make-instance 'bitmask-type :specifying specifying))

(deftype bitmask ()
  "A lisp representation of 'struct bitmask' of libnuma"
  '(bit-vector *))

(defun bitmask-enough-size ()
  (let ((cpumask-len (numa-num-possible-cpus))
	(nodemask-len (numa-num-possible-nodes)))
    (max cpumask-len nodemask-len)))

(defmethod translate-to-foreign (lisp-bitmask (type bitmask-type))
  (let ((bmp (case (bitmask-type-specifying type)
	       (:cpu (numa-allocate-cpumask))
	       (:node (numa-allocate-nodemask))
	       (t (numa-bitmask-alloc (bitmask-enough-size))))))
    (loop for i across lisp-bitmask
       when (plusp i)
       do (numa-bitmask-setbit bmp i))
    bmp))

(defmethod translate-from-foreign (bmp (type bitmask-type))
  (let* ((size (case (bitmask-type-specifying type)
		 (:cpu (numa-num-configured-cpus))
		 (:node (numa-num-configured-nodes))
		 (t (bitmask-enough-size))))
	 (lisp-bitmask (make-array `(,size) :element-type 'bit
				   :initial-element 0))) ; follows numa_bitmask_alloc()
    (loop for i from 0 below size
       when (numa-bitmask-isbitset bmp i)
       do (setf (aref lisp-bitmask i) 1))
    lisp-bitmask))

(defmethod free-translated-object (bmp (type bitmask-type) param)
  (declare (ignore param))
  (case (bitmask-type-specifying type)
    (:cpu (numa-free-cpumask bmp))
    (:node (numa-free-nodemask bmp))
    (t (numa-bitmask-free bmp))))


;; A representation for 'nodemask_t'
(defctype nodemask_t-pointer
    (:pointer (:struct nodemask_t)))


;; A representation for 'int' returned from libc.
(defun lisp-to-libc-return-boolean (value)
  (if value 0 -1))

(defun lisp-from-libc-return-boolean (value)
  (declare (type fixnum value))
  (>= value 0))

(defctype libc-return-boolean 
    (:wrapper :int
	      :to-c lisp-to-libc-return-boolean
	      :from-c lisp-from-libc-return-boolean))


;;; Funtions

(defcfun "numa_available"
    libc-return-boolean)


(defcfun "numa_max_possible_node"
    :int)

(defcfun "numa_num_possible_nodes"
    :int)

;; BUG: This does not exist at the top of man page, and has no explanations!
(defcfun (numa-num-possible-cpus* "numa_num_possible_cpus")
    :int)

;; A workaround for numa_num_possible_cpus(), which is not exported
;; until libnuma-2.0.8-rc4.
;; ( http://www.spinics.net/lists/linux-numa/msg00948.html )
(let ((possible-cpus nil))
  (defun numa-num-possible-cpus-alternative ()
    (unless possible-cpus
      ;; Get the value from an allocated cpumask.
      (let ((bmp (numa-allocate-cpumask)))
	(unwind-protect
	     (setf possible-cpus
		   (progn (numa-bitmask-setall bmp)
			  (numa-bitmask-weight bmp)))
	  (numa-bitmask-free bmp))))
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
    (bitmask-type :specifying :node))


(defcfun "numa_num_configured_cpus"
    :int)

(defcvar (*numa-all-nodes* "numa_all_nodes_ptr" :read-only t)
    (bitmask-type :specifying :node))

(defcvar (*numa-no-nodes* "numa_no_nodes_ptr" :read-only t) 
    (bitmask-type :specifying :node))

(defcvar (*numa-all-cpus* "numa_all_cpus_ptr" :read-only t)
    (bitmask-type :specifying :cpu))


(defcfun "numa_num_task_cpus"
    :int)

(defcfun "numa_num_task_nodes"
    :int)


;; TODO: wrap memory management
(defcfun "numa_parse_bitmap"
    libc-return-boolean
  (line :string)
  (mask struct-bitmask-pointer))

(defcfun "numa_parse_nodestring"
    (bitmask-type :specifying :node)
  (string :string))

(defcfun "numa_parse_nodestring_all"
    (bitmask-type :specifying :node)
  (string :string))

(defcfun "numa_parse_cpustring"
    (bitmask-type :specifying :cpu)
  (string :string))

(defcfun "numa_parse_cpustring_all"
    (bitmask-type :specifying :cpu)
  (string :string))


(defcfun (numa-node-size* "numa_node_size")
    :long
  (node :int)
  (freep (:pointer :long)))

(defun numa-node-size (node)
  (with-foreign-object (freep :long 1)
    (let ((size (numa-node-size* node freep)))
      (values size (mem-aref freep :long 0)))))
  
(defcfun (numa-node-size64* "numa_node_size64")
    :long-long
  (node :int)
  (freep (:pointer :long-long)))

(defun numa-node-size64 (node)
  (with-foreign-object (freep :long-long 1)
    (let ((size (numa-node-size64* node freep)))
      (values size (mem-aref freep :long-long 0)))))


(defcfun "numa_preferred"
    :int)

(defcfun "numa_set_preferred"
    :void
  (node :int))

(defcfun "numa_get_interleave_node" ; source says this is undocumented
    :int)

(defcfun "numa_get_interleave_mask"
    (bitmask-type :specifying :node))

(defcfun "numa_set_interleave_mask"
    :void
  (nodemask (bitmask-type :specifying :node)))

(defcfun "numa_interleave_memory"
    :void
  (start :pointer)
  (size size_t)
  (nodemask (bitmask-type :specifying :node)))

(defcfun "numa_bind"
    :void
  (nodemask (bitmask-type :specifying :node)))

(defcfun "numa_set_localalloc"
    :void)

(defcfun "numa_set_membind"
    :void
  (nodemask (bitmask-type :specifying :node)))

(defcfun "numa_get_membind"
    (bitmask-type :specifying :node))

  
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
  (nodemask (bitmask-type :specifying :node)))

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
  (nodemask (bitmask-type :specifying :node)))

(defcfun "numa_run_on_node_mask_all"
    libc-return-boolean
  (nodemask (bitmask-type :specifying :node)))

(defcfun "numa_get_run_node_mask"
    ;; manpage says this returns a CPU mask, but source says it's a node mask!
    (bitmask-type :specifying :node))


(defcfun "numa_tonode_memory"
    :void
  (start :pointer)
  (size size_t)
  (node :int))

(defcfun "numa_tonodemask_memory"
    :void
  (start :pointer)
  (size size_t)
  (nodemask struct-bitmask-pointer))

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


;; TODO: wrap memory management.
(defcfun "numa_sched_getaffinity"
    libc-return-boolean
  (pid pid_t)
  (mask struct-bitmask-pointer))

(defcfun "numa_sched_setaffinity"
    libc-return-boolean
  (pid pid_t)
  (mask (bitmask-type :specifying :cpu)))

;; TODO: wrap memory management. This should return a bitmask.
;; BUG: manpage don't highlight numa_allocate_cpumask()
(defcfun "numa_node_to_cpus"
    libc-return-boolean
  (node :int)
  (mask struct-bitmask-pointer))

;; TODO: This retval is libc-semantics.
(defcfun "numa_node_of_cpu"
    :int
  (cpu :int))


;; TODO: separate 'raw' and 'bitmask' version

(defcfun "numa_allocate_cpumask"
    struct-bitmask-pointer) ; should be freed with numa_free_cpumask()

;; numa_free_cpumask() is defined in the wrapper.

(defcfun "numa_allocate_nodemask"
    struct-bitmask-pointer) ; should be freed with numa_free_nodemask()

;; numa_free_nodemask() is defined in the wrapper.


(defcfun "numa_bitmask_alloc"
    struct-bitmask-pointer ; should be freed with numa_bitmask_free()
  (n :unsigned-int))

(defcfun "numa_bitmask_clearall"
    struct-bitmask-pointer
  (bmp struct-bitmask-pointer))

(defcfun "numa_bitmask_clearbit"
    struct-bitmask-pointer
  (bmp struct-bitmask-pointer)
  (n :unsigned-int))

(defcfun "numa_bitmask_equal"
    (:boolean :int)
  (bmp1 struct-bitmask-pointer)
  (bmp2 struct-bitmask-pointer))

(defcfun "numa_bitmask_free"
    :void
  (bmp struct-bitmask-pointer))

(defcfun "numa_bitmask_isbitset"
    (:boolean :int)
  (bmp struct-bitmask-pointer)
  (n :unsigned-int))

(defcfun "numa_bitmask_nbytes"
    :unsigned-int
  (bmp struct-bitmask-pointer))

(defcfun "numa_bitmask_setall"
    struct-bitmask-pointer
  (bmp struct-bitmask-pointer))

(defcfun "numa_bitmask_setbit"
    struct-bitmask-pointer
  (bmp struct-bitmask-pointer)
  (n :unsigned-int))

(defcfun "copy_bitmask_to_nodemask"
    :void
  (bmp struct-bitmask-pointer)
  (nodemask nodemask_t-pointer))

(defcfun "copy_nodemask_to_bitmask"
    :void
  (nodemask nodemask_t-pointer)
  (bmp struct-bitmask-pointer))

(defcfun "copy_bitmask_to_bitmask"
    :void
  (bmpfrom struct-bitmask-pointer)
  (bmpto struct-bitmask-pointer))

(defcfun "numa_bitmask_weight"
    :unsigned-int
  (bmp struct-bitmask-pointer))


(defcfun (numa-move-pages* "numa_move_pages")
    :int
  (pid :int)
  (count :unsigned-long)
  (pages (:pointer :pointer))
  (nodes (:pointer :int))
  (status (:pointer :int))
  (flags :int))

(defun numa-move-pages (pid count pages-list nodes-list &optional (flags MPOL_MF_MOVE))
  (let ((pages-list-length (length pages-list))
	(nodes-list-length (length nodes-list)))
    (with-foreign-object (pages-array :pointer pages-list-length)
      (loop for i from 0
	 for p in pages-list
	 do (setf (mem-aref pages-array :pointer i) p))
      (with-foreign-object (nodes-array :int nodes-list-length)
	(loop for i from 0
	   for n in nodes-list
	   do (setf (mem-aref nodes-array :int i) n))
	(with-foreign-object (status-array :int pages-list-length)
	  (let ((ret (numa-move-pages* pid count pages-array nodes-array
				       status-array flags)))
	    (case ret
	      (-1 (values ret nil))
	      (0 (values ret
			 (loop for i from 0 below pages-list-length
			    collect (mem-aref status-array :int i))))
	      (t (error "numa_move_pages returned an unexpected value ~D" ret)))))))))
	  

(defcfun "numa_migrate_pages"
    libc-return-boolean
  (pid :int)
  (fromnodes struct-bitmask-pointer)
  (tonodes struct-bitmask-pointer))


;; numa_error() is intended to be overriden

(defcvar "numa_exit_on_error"
    (:boolean :int))

(defcvar "numa_exit_on_warn"
    (:boolean :int))

;; numa_warn() is intended to be overriden


;; source says 'not documented', and don't appear at the top of manpage, but description is found!
(defcfun "numa_pagesize"
    :int)
