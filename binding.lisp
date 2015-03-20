(in-package :cl-libnuma)

(defconstant +cl-libnuma-target-api-version+ 2)


(defctype struct-bitmask-pointer
    (:pointer (:struct struct-bitmask)))

(defctype nodemask_t-pointer
    (:pointer (:struct nodemask_t)))


(defcfun (numa-available* "numa_available")
    :int)

(defun numa-available ()
  (zerop (numa-available*)))


(defcfun "numa_max_possible_node"
    :int)

(defcfun "numa_num_possible_nodes"
    :int)

(defcfun "numa_num_possible_cpus" ; BUG: This does not exist at the top of man page, and has no explanations!
    ;; This is not exported until libnuma-2.0.8-rc4
    ;; http://www.spinics.net/lists/linux-numa/msg00948.html
    :int)


(defcfun "numa_max_node"
    :int)

(defcfun "numa_num_configured_nodes"
    :int)

(defcfun "numa_get_mems_allowed"
    struct-bitmask-pointer) ; should be freed with numa_free_nodemask()


(defcfun "numa_num_configured_cpus"
    :int)

(defcvar ("numa_all_nodes_ptr" :read-only t)
    struct-bitmask-pointer)

(defcvar ("numa_no_nodes_ptr" :read-only t) 
    struct-bitmask-pointer)

(defcvar ("numa_all_cpus_ptr" :read-only t)
    struct-bitmask-pointer)


(defcfun "numa_num_task_cpus"
    :int)

(defcfun "numa_num_task_nodes"
    :int)


(defcfun "numa_parse_bitmap"
    :int
  (line :string)
  (mask struct-bitmask-pointer))

(defcfun "numa_parse_nodestring"
    struct-bitmask-pointer ; should be freed with numa_free_nodemask()
  (string :string))

(defcfun "numa_parse_nodestring_all"
    struct-bitmask-pointer ; should be freed with numa_free_nodemask()
  (string :string))

(defcfun "numa_parse_cpustring"
    struct-bitmask-pointer ; should be freed with numa_free_cpumask()
  (string :string))

(defcfun "numa_parse_cpustring_all"
    struct-bitmask-pointer ; should be freed with numa_free_cpumask()
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
    struct-bitmask-pointer) ; should be freed with numa_free_nodemask()

(defcfun "numa_set_interleave_mask"
    :void
  (nodemask struct-bitmask-pointer))

(defcfun "numa_interleave_memory"
    :void
  (start :pointer)
  (size size_t)
  (nodemask struct-bitmask-pointer))

(defcfun "numa_bind"
    :void
  (nodemask struct-bitmask-pointer))

(defcfun "numa_set_localalloc"
    :void)

(defcfun "numa_set_membind"
    :void
  (nodemask struct-bitmask-pointer))

(defcfun "numa_get_membind"
    struct-bitmask-pointer) ; should be freed with numa_free_nodemask()

  
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
  (nodemask struct-bitmask-pointer))

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


;; TODO: This retval is libc-semantics.
(defcfun "numa_run_on_node"
    :int
  (node :int))

;; TODO: This retval is libc-semantics.
(defcfun "numa_run_on_node_mask"
    :int
  (nodemask struct-bitmask-pointer))

(defcfun "numa_run_on_node_mask_all"
    :int
  (nodemask struct-bitmask-pointer))

(defcfun "numa_get_run_node_mask"
    struct-bitmask-pointer) ; should be freed with numa_free_cpumask()


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
    :int
  (pid pid_t)
  (mask struct-bitmask-pointer))

(defcfun "numa_sched_setaffinity"
    :int
  (pid pid_t)
  (mask struct-bitmask-pointer))

;; TODO: This retval is libc-semantics.
;; TODO: wrap memory management.
;; BUG: manpage don't highlight numa_allocate_cpumask()
(defcfun "numa_node_to_cpus"
    :int
  (node :int)
  (mask struct-bitmask-pointer))

;; TODO: This retval is libc-semantics.
(defcfun "numa_node_of_cpu"
    :int
  (cpu :int))


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
    :int
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
