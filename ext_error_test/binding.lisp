(in-package :cl-libnuma.ext-error.test)

(defparameter *numa-error-called* nil)
(defparameter *numa-warn-called* nil)

(defun numa-error-test-callback (where)
  (setf *numa-error-called* (or where t)))

(defun numa-warn-test-callback (number where)
  (declare (ignore number))
  (setf *numa-warn-called* (or where t)))

(defmacro with-test-callback (() &body body)
  (let ((orig-error-callback-sym (gensym))
	(orig-warn-callback-sym (gensym)))
    `(let ((*numa-error-called* nil)
	   (*numa-warn-called* nil)
	   (,orig-error-callback-sym *numa-error-callback*)
	   (,orig-warn-callback-sym *numa-warn-callback*))
       (setf *numa-error-callback* #'numa-error-test-callback
	     *numa-warn-callback* #'numa-warn-test-callback)
       (unwind-protect
	    (progn ,@body)
	 (setf *numa-error-callback* ,orig-error-callback-sym
	       *numa-warn-callback* ,orig-warn-callback-sym)))))

(defun test-callback-available? ()
  (with-test-callback ()
    (numa-error "test")
    (assert *numa-error-called*)
    (numa-warn 0 "test")
    (assert *numa-warn-called*))
  t)

(defun test-libnuma-api-error ()
  ;; Checks numa_error() is properly called from libnuma.
  ;; I collect these examples from the source.

  ;; (wrapped api)
  ;; numa_bitmask_alloc() @ -1
  ;; numa_node_to_cpus() @ small mask

  ;; (internal) setpol, getpol, dombind

  ;; numa_run_on_node_mask() @ sched_setaffinity failure

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
  t)

(defun test-libnuma-api-warn ()
  ;; Checks numa_error() is properly called from libnuma.
  ;; I collect these examples from the source.

  ;; numa_parse_nodestring()
  ;; numa_parse_cpustring()
  t)

(defun test-binding ()
  (and (test-callback-available?)
       (test-libnuma-api-error)
       (test-libnuma-api-warn)
       t))
