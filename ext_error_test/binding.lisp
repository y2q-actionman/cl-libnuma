(in-package :cl-libnuma.ext-error.test)

(defmacro with-ext-error-callback (&body body)
  `(progn
     (install-condition-callback)
     (unwind-protect (progn ,@body)
       (uninstall-condition-callback))))

(defun test-libnuma-api-error ()
  ;; Checks numa_error() is properly called from libnuma.
  ;; I collect these examples from the source.
  
  (with-ext-error-callback
    (assert-progn
     ;; - numa_bitmask_alloc()
     ;; Calling with '0' or '-1' will invoke numa_error(), but exit(3)
     ;; will be called after. I can't do it here.

     ;; - numa_get_mems_allowed()
     ;; If this calls numa_error(), it is very fatal, I think.

     ;; - numa_node_to_cpus() :: called with a small mask.
     (let ((node 0))
       (if (> (length (numa-node-to-cpus node)) 1)
	   (cl-libnuma::with-temporal-struct-bitmask-pointer
	       (bmp (cl-libnuma::numa-bitmask-alloc* 1))
	     (assume-condition (numa-error-condition)
	       (cl-libnuma::numa-node-to-cpus* node bmp)))
	   t))				; no way to check it.
  ;; 
  ;; numa_run_on_node_mask() @ sched_setaffinity failure
  ;; numa_run_on_node_mask_all() @ sched_setaffinity failure

  ;; (internal) setpol, getpol, dombind

  ;; (setpol)
  ;; numa_set_interleave_mask()
  ;; numa_set_membind()
  ;; numa_set_preferred()
  ;; numa_set_localalloc()

  ;; (getpol)
  ;; numa_get_interleave_mask()
  ;; numa_get_membind()
  ;; numa_preferred()

  ;; (dombind)
  ;; numa_interleave_memory()
  ;; numa_tonode_memory()
  ;; numa_tonodemask_memory()
  ;; numa_setlocal_memory()
  ;; numa_alloc_interleaved_subset()
  ;; numa_alloc_onnode()
  ;; numa_alloc_local()
    ))
  t)

(defun test-libnuma-api-warn ()
  ;; Checks numa_warn() is properly called from libnuma.
  ;; I collect these examples from the source.
  
  ;; numa_parse_nodestring()
  ;; numa_parse_cpustring()

  ;; (If not sysfs..)
  ;; numa_node_size64()
  t)

(defun test-binding ()
  (and (test-libnuma-api-error)
       (test-libnuma-api-warn)
       t))
