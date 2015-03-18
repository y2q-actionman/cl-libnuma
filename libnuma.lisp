(in-package :cl-libnuma)

(define-foreign-library libnuma
  (:unix (:or "libnuma.so.1" "libnuma.so"))
  (t (:default "libnuma")))



(defcfun "numa_available" :int)

(defcfun "numa_max_node" :int)
(defcfun "numa_max_possible_node" :int)

(defcfun "numa_num_possible_nodes" :int)
(defcfun "numa_num_possible_cpus" :int)	; BUG: This does not exist at the top of man page, and has no explanations!

(defcfun "numa_num_configured_nodes" :int)
(defcfun "numa_num_configured_cpus" :int)

(defcvar ("numa_all_nodes_ptr" :read-only t) (:pointer (:struct bitmask)))
(defcvar ("numa_no_nodes_ptr" :read-only t) (:pointer (:struct bitmask)))
(defcvar ("numa_all_cpus_ptr" :read-only t) (:pointer (:struct bitmask)))

(defcfun "numa_num_task_cpus" :int)
(defcfun "numa_num_task_nodes" :int)

;; This block is not tested.
(defcfun "numa_parse_bitmap" :int
  (line :string)
  (mask (:pointer (:struct bitmask))))
(defcfun "numa_parse_nodestring" (:pointer (:struct bitmask)) ; should be freed with numa_free_nodemask()
  (string :string))
(defcfun "numa_parse_nodestring_all" (:pointer (:struct bitmask)) ; should be freed with numa_free_nodemask()
  (string :string))
(defcfun "numa_parse_cpustring" (:pointer (:struct bitmask)) ; should be freed with numa_free_cpumask()
  (string :string))
(defcfun "numa_parse_cpustring_all" (:pointer (:struct bitmask)) ; should be freed with numa_free_cpumask()
  (string :string))

(defcfun "numa_node_size" :long
  (node :int)
  (freep (:pointer :long)))
(defcfun "numa_node_size64" :long-long
  (node :int)
  (freep (:pointer :long-long)))

(defcfun "numa_preferred" :int)
(defcfun "numa_set_preferred" :void
  (node :int))
(defcfun "numa_get_interleave_node" :int) ; source says this is undocumented
;; not tested from here in this block
(defcfun "numa_get_interleave_mask" (:pointer (:struct bitmask))) ; should be freed with numa_free_nodemask()
(defcfun "numa_set_interleave_mask" :void
  (nodemask (:pointer (:struct bitmask))))
(defcfun "numa_interleave_memory" :void
  (start :pointer)
  (size size_t)
  (nodemask :pointer (:struct bitmask)))
(defcfun "numa_bind" :void
  (nodemask :pointer (:struct bitmask)))
(defcfun "numa_set_localalloc" :void)
(defcfun "numa_set_membind" :void
  (nodemask :pointer (:struct bitmask)))
(defcfun "numa_get_membind" (:pointer (:struct bitmask))) ; should be freed with numa_free_nodemask()
(defcfun "numa_get_mems_allowed" (:pointer (:struct bitmask))) ; should be freed with numa_free_nodemask()
  
;; This block is not tested.
(defcfun "numa_alloc_onnode" :pointer
  (size size_t)
  (node :int))
(defcfun "numa_alloc_local" :pointer
  (size size_t))
(defcfun "numa_alloc_interleaved" :pointer
  (size size_t))
(defcfun "numa_alloc_interleaved_subset" :pointer
  (size size_t)
  (nodemask :pointer (:struct bitmask)))
(defcfun "numa_alloc" :pointer
  (size size_t))
(defcfun "numa_realloc" :pointer
  (old-addr :pointer)
  (old-size size_t)
  (new-size size_t))
(defcfun "numa_free" :void
  (start :pointer)
  (size size_t))

;; This block is not tested.
(defcfun "numa_run_on_node" :int
  (node :int))
(defcfun "numa_run_on_node_mask" :int
  (nodemask :pointer (:struct bitmask)))
(defcfun "numa_run_on_node_mask_all" :int
  (nodemask :pointer (:struct bitmask)))
(defcfun "numa_get_run_node_mask" (:pointer (:struct bitmask))) ; should be freed with numa_free_cpumask()

;; This block is not tested.
(defcfun "numa_tonode_memory" :void
  (start :pointer)
  (size size_t)
  (node :int))
(defcfun "numa_tonodemask_memory" :void
  (start :pointer)
  (size size_t)
  (nodemask :pointer (:struct bitmask)))
(defcfun "numa_setlocal_memory" :void
  (start :pointer)
  (size size_t))
(defcfun "numa_police_memory" :void
  (start :pointer)
  (size size_t))
(defcfun "numa_set_bind_policy" :void
  (strict :int))
(defcfun "numa_set_strict" :void
  (strict :int))

(defcfun "numa_distance" :int
  (node1 :int)
  (node2 :int))

;; This block is not tested.
(defcfun "numa_sched_getaffinity" :int
  (pid pid_t)
  (mask (:pointer (:struct bitmask))))
(defcfun "numa_sched_setaffinity" :int
  (pid pid_t)
  (mask (:pointer (:struct bitmask))))
(defcfun "numa_node_to_cpus" :int
  (node :int)
  (mask (:pointer (:struct bitmask))))
(defcfun "numa_node_of_cpu" :int
  (cpu :int))

;; This block is not tested.
(defcfun "numa_allocate_cpumask" (:pointer (:struct bitmask))) ; should be freed with numa_free_cpumask()
(defcfun "numa_free_cpumask" :void
  ;; FIXME: this is static inline!! -- use numa_bitmask_free() ?
  ;; BUG: this function takes a pointer to struct bitmask! (K&R C style!?)
  (b  (:pointer (:struct bitmask))))
(defcfun "numa_allocate_nodemask" (:pointer (:struct bitmask))) ; should be freed with numa_free_nodemask()
(defcfun "numa_free_nodemask" :void
  ;; FIXME: this is static inline!! -- use numa_bitmask_free() ?
  ;; BUG: this function takes a pointer to struct bitmask! (K&R C style!?)
  (b  (:pointer (:struct bitmask))))

;; This block is not tested.
(defcfun "numa_bitmask_alloc" (:pointer (:struct bitmask)) ; should be freed with numa_bitmask_free()
  (n :unsigned-int))
(defcfun "numa_bitmask_clearall" (:pointer (:struct bitmask))
  (bmp (:pointer (:struct bitmask))))
(defcfun "numa_bitmask_clearbit" (:pointer (:struct bitmask))
  (bmp (:pointer (:struct bitmask)))
  (n :unsigned-int))
(defcfun "numa_bitmask_equal" :int	; should be defined as a boolean?
  (bmp1 (:pointer (:struct bitmask)))
  (bmp2 (:pointer (:struct bitmask))))
(defcfun "numa_bitmask_free" :void
  (bmp (:pointer (:struct bitmask))))
(defcfun "numa_bitmask_isbitset" :int	; should be defined as a boolean?
  (bmp (:pointer (:struct bitmask)))
  (n :unsigned-int))
(defcfun "numa_bitmask_nbytes" :unsigned-int
  (bmp (:pointer (:struct bitmask))))
(defcfun "numa_bitmask_setall" (:pointer (:struct bitmask))
  (bmp (:pointer (:struct bitmask))))
(defcfun "numa_bitmask_setbit" (:pointer (:struct bitmask))
  (bmp (:pointer (:struct bitmask)))
  (n :unsigned-int))
(defcfun "copy_bitmask_to_nodemask" :void
  (bmp (:pointer (:struct bitmask)))
  (nodemask (:pointer (:struct nodemask))))
(defcfun "copy_nodemask_to_bitmask" :void
  (nodemask (:pointer (:struct nodemask)))
  (bmp (:pointer (:struct bitmask))))
(defcfun "copy_bitmask_to_bitmask" :void
  (bmpfrom (:pointer (:struct bitmask)))
  (bmpto (:pointer (:struct bitmask))))
(defcfun "numa_bitmask_weight" :unsigned-int
  (bmp (:pointer (:struct bitmask))))

;; This block is not tested.
(defcfun "numa_move_pages" :int
  (pid :int)
  (count :unsigned-long)
  (pages (:pointer :pointer))
  (nodes (:pointer :int))
  (status (:pointer :int))
  (flags :int))
(defcfun "numa_migrate_pages" :int
  (pid :int)
  (fromnodes (:pointer (:struct bitmask)))
  (tonodes (:pointer (:struct bitmask))))

;; This block is not tested. 
;; numa_error() is intended to be overriden
(defcvar "numa_exit_on_error" :int)
;; numa_warn() is intended to be overriden
(defcvar "numa_exit_on_warn" :int)

;; not documented
#|

int numa_pagesize(void);

|#

;; Should I make 'getpid' ready?
