(in-package :cl-libnuma.error-handler.test)

(defun make-bad-node-mask ()
  (let ((mask (make-numa-bitmask (numa-num-possible-nodes))))
    (numa-bitmask-setbit mask (numa-num-configured-nodes))
    mask))

(defmacro assume-numa-error (&body body)
  `(assume-condition (numa-error-condition)
     ,@body))

(defun test-libnuma-api-error-simple ()
  (assert-progn
    ;; - numa_bitmask_alloc()
    ;; Calling with '0' or '-1' will invoke numa_error(), but exit(3)
    ;; will be called after. I can't do it here.

    ;; - numa_get_mems_allowed()
    ;; If this calls numa_error(), it is very fatal, I think.

    ;; - numa_node_to_cpus() :: called with a small mask.
    (let ((node 0))
      (assert-when (> (length (numa-node-to-cpus node)) 1)
	(cl-libnuma::with-temporal-struct-bitmask-pointer
	    (bmp (cl-libnuma::numa-bitmask-alloc* 1))
	  (assume-numa-error
	    (cl-libnuma::numa-node-to-cpus* node bmp)))))

    ;; - numa_run_on_node_mask() :: failure at numa_sched_setaffinity()
    ;; - numa_run_on_node_mask_all() :: failure at numa_sched_setaffinity()
    (unwind-protect
	 (flet ((test-run-on-node-mask (func)
		  (assume-numa-error
		    (funcall func *numa-no-nodes-bitmask*))))
	   (assert-progn
	     (test-run-on-node-mask #'numa-run-on-node-mask)
	     (assert-when (if-numa-function-exists (numa-run-on-node-mask-all *numa-all-nodes-bitmask*) t nil)
	       (test-run-on-node-mask #'numa-run-on-node-mask-all))))
      (numa-run-on-node-mask *numa-all-nodes-bitmask*)))
  t)

(defun test-libnuma-api-error-mempolicy ()
  ;; == Via internal set_mempolicy() call (setpol()) ==
  (unwind-protect
       (assert-progn
	 ;; - numa_set_interleave_mask()
	 ;; calls with a too big mask, or calls after a strict binding.
	 (assert-when (< (numa-num-configured-nodes) (numa-num-possible-nodes))
	   (assume-numa-error (numa-set-membind (make-bad-node-mask))))
			
	 ;; - numa_set_membind() :: calles with an empty mask, or a mask contains a too big node.
	 (assume-numa-error (numa-set-membind *numa-no-nodes-bitmask*))
	 (assert-when (< (numa-num-configured-nodes) (numa-num-possible-nodes))
	   (assume-numa-error (numa-set-membind (make-bad-node-mask))))

	 ;; - numa_set_preferred() :: calls with a possible but too big node number.
	 (not (assume-numa-error (numa-set-preferred -1)))
	 (not (assume-numa-error (numa-set-preferred 0)))
	 (assert-when (< (numa-num-configured-nodes) (numa-num-possible-nodes))
	   (assume-numa-error (numa-set-preferred (numa-num-configured-nodes))))
	 (not (assume-numa-error
		(numa-set-preferred (numa-num-possible-nodes))))

	 ;; - numa_set_localalloc()
	 ;; This call rarely fails.
	 )
    (numa-set-localalloc))		; This is default.

  ;; == Via internal get_mempolicy() call (getpol()) ==

  ;; - numa_get_interleave_mask()
  ;; This call rarely fails.

  ;; - numa_get_membind()
  ;; This call rarely fails.

  ;; - numa_preferred()
  ;; This call rarely fails.
  t)

(defun test-libnuma-api-error-mbind ()
  (let ((alloc-size (numa-pagesize)))
    (macrolet ((with-numa-freeing ((symbol) &body body)
		 `(let ((,symbol nil))
		    (unwind-protect (progn ,@body)
		      (when ,symbol
			(numa-free ,symbol alloc-size))))))
      ;; == Via internal mbind() call (dombind()) ==

      ;; - numa_alloc_local()
      ;; This call rarely fails.
	
      ;; - numa_alloc_interleaved_subset()
      ;; Failure at using an empty mask.
      (with-numa-freeing (mem)
	(assert-progn
	  (assume-numa-error
	    (setf mem (numa-alloc-interleaved-subset alloc-size
						     *numa-no-nodes-bitmask*)))))

      (numa-set-bind-policy t)		; This is default.

      ;; - numa_alloc_onnode()
      ;; Failure at using an empty mask and 'numa_set_bind_policy(1)'. (MPOL_BIND)
      (with-numa-freeing (mem)
	(assert-progn
	  (assume-numa-error
	    (setf mem (numa-alloc-onnode alloc-size
					 *numa-no-nodes-bitmask*)))))

      (with-numa-freeing (mem)
	(setf mem (numa-alloc-interleaved alloc-size))
	(setf (cffi:mem-aref mem :char) 1) ; Touch it, in order to allocate by the kernel. 
	(assert-progn
	  ;; - numa_interleave_memory()
	  ;; Failure at using an empty mask and 'numa_set_bind_policy(1)'. (MPOL_BIND)
	  (assume-numa-error
	    (numa-interleave-memory mem alloc-size *numa-no-nodes-bitmask*))

	  ;; - numa_tonode_memory()
	  ;; Failure at using a bad node and 'numa_set_bind_policy(1)'. (MPOL_BIND)
	  (assume-numa-error
	    (numa-tonode-memory mem alloc-size (numa-num-configured-nodes)))
	      
	  ;; - numa_tonodemask_memory()
	  ;; Failure at using an empty mask and 'numa_set_bind_policy(1)'. (MPOL_BIND)
	  (assume-numa-error
	    (numa-tonodemask-memory mem alloc-size *numa-no-nodes-bitmask*))

	  ;; - numa_setlocal_memory()
	  ;; Fails if called for an allocated memory with MPOL_MF_STRICT.
	  (unwind-protect
	       (progn (numa-set-strict t) ; This leads mbind() to fail.
		      (assume-numa-error
			(numa-setlocal-memory mem alloc-size)))
	    (numa-set-strict nil))))))
  t)

(defmacro assume-numa-warn (&body body)
  `(assume-condition (numa-warn-condition)
     ,@body))

(defun test-libnuma-api-warn ()
  (assert-progn
    (assert-when (< (numa-num-configured-nodes) (numa-num-possible-nodes))
      ;; - numa_run_on_node_mask()
      ;; Not allowed node.
      (assume-numa-warn
	(numa-run-on-node-mask (make-bad-node-mask)))
      ;; - numa_run_on_node_mask_all() @ as same
      (assert-when (if-numa-function-exists (numa-run-on-node-mask-all *numa-all-nodes-bitmask*) t nil)
	(assume-numa-warn
	  (numa-run-on-node-mask-all (make-bad-node-mask)))))

    ;; - numa_parse_nodestring()
    ;; bad format.
    (assume-numa-warn
      (numa-parse-nodestring "???"))

    ;; - numa_parse_cpustring()
    ;; bad format
    (assume-numa-warn
      (numa-parse-cpustring "???"))

    ;; - numa_node_size64()
    ;; - numa_node_to_cpus()
    ;; - numa_run_on_node()
    ;; These may call numa_warn() when sysfs is not mounted.
    ;; I cannot reproduct this case easily.
    )
  t)

(defun test-binding ()
  (install-condition-callback)
  (unwind-protect
       (and
	;; Checks numa_error() is properly called when an error occured in libnuma.
	(test-libnuma-api-error-simple)
	(test-libnuma-api-error-mempolicy)
	(test-libnuma-api-error-mbind)
	;; Checks numa_warn() is properly called from libnuma.
	(test-libnuma-api-warn))
    (uninstall-condition-callback))
  t)
