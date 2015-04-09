(in-package :cl-libnuma.ext-error.test)

(defun test-numa-error ()
  (macrolet ((try-call (switch-var func-name &rest func-args)
	       `(let ((old-value ,switch-var))
		  (unwind-protect
		       (always-success
			 (setf ,switch-var nil)
			 (,func-name ,@func-args))
		    (setf ,switch-var old-value)))))
    (assert-progn
     (typep *numa-exit-on-error* 'boolean)
     (typep *numa-exit-on-warn* 'boolean)
     (try-call *numa-exit-on-error*
	       numa-error "hoge")
     (try-call *numa-exit-on-warn*
	       numa-warn 0 "hoge"))))
