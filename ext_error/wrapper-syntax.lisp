(in-package :cl-libnuma.wrapper-syntax)

;; Syntax definitions
(defun parse-overriding-callback-name (nameopts)
  "Returns: (values <c-overriden-function-name> <lisp-callback-variable-name>
        <lisp-trampoline-function-name> <c-trampoline-variable-name>)"
  (flet ((generate-results
	     (c-overriden-function-name
	      &key
	      (lisp-callback-variable-name
	       (cffi::lisp-name (concatenate 'string c-overriden-function-name "_callback") t))
	      (c-trampoline-variable-name
	       (concatenate 'string c-overriden-function-name "_trampoline"))
	      (lisp-trampoline-function-name
	       (cffi::lisp-name c-trampoline-variable-name nil)))
	   (values c-overriden-function-name
		   lisp-callback-variable-name
		   c-trampoline-variable-name
		   lisp-trampoline-function-name)))
    (etypecase nameopts
      (string (generate-results nameopts))
      (list (apply #'generate-results nameopts)))))

(defun generate-overriding-callback (out nameopts rettype args c-lines)
  (multiple-value-bind (c-overriden-function-name
			lisp-callback-variable-name
			c-trampoline-variable-name
			lisp-trampoline-function-name)
      (parse-overriding-callback-name nameopts)
    (let* ((fret-c-name (cffi-grovel::c-type-name rettype))
	   (variadic nil)
	   (arg-types nil)
	   (arg-names nil)
	   (foreign-arg-types nil)
	   (foreign-arg-names nil))
      (loop for arg in args
	 when (and (symbolp arg)
		   (string= (symbol-name arg)
			    (symbol-name '&rest)))
	 do (setf variadic t)
	 else
	 collect (second arg) into types
	 and collect (first arg) into names
	 finally (setf arg-types types
		       arg-names names))
      (setf foreign-arg-types
	    (mapcar #'cffi-grovel::c-type-name arg-types)
	    foreign-arg-names
	    (mapcar (lambda (x) (cffi::foreign-name x nil)) arg-names))

      ;; C code
      (format out "~A (*~A)();~%~%"	; callback pointer
	      fret-c-name c-trampoline-variable-name)

      (format out "~A ~A(~{~{~A ~A~}~^, ~} ~@[, ...~]){~%" ; start of function definition
	      fret-c-name c-overriden-function-name
	      (loop for tp in foreign-arg-types
		 for ar in foreign-arg-names
		 collect (list tp ar))
	      variadic)
      (format out "~1Tif (~A) {~{~&~2T~A~%~}" ; if callback exists..
	      c-trampoline-variable-name
	      (if c-lines
		  c-lines
		  ;; default -- simply calls the callback.
		  (list (format nil "return (*~A)(~{~A~^, ~});"
				c-trampoline-variable-name foreign-arg-names))))
      (format out "~1T} else {~%")      ; if no callback..
      (format out "~2Treturn ~@[0~];~%" (not (eq rettype :void)))
      (format out "~1T}~%}~%~%")

      ;; lisp code
      (when variadic
	(warn "Overriding a varidic function is not straight-forward. Please use define-wrapper-syntax* carefully."))
      (push `(progn
	       (cl:defparameter ,lisp-callback-variable-name nil)
	       (cffi:defcvar (,lisp-trampoline-function-name ,c-trampoline-variable-name)
		   :pointer)
	       (cffi:defcallback ,lisp-trampoline-function-name ,rettype
		   (,@(if variadic (butlast args) args))
		 (when ,lisp-callback-variable-name
		   (funcall ,lisp-callback-variable-name ,@arg-names)))
	       (setf ,lisp-trampoline-function-name
		     (cffi:callback ,lisp-trampoline-function-name)))
	    cffi-grovel::*lisp-forms*))))
  

;; The name of wrapper-syntax must be a symbol of :cffi-grovel
;; package.  See 'cffi-grovel::form-kind'.
(cffi-grovel::define-wrapper-syntax cffi-grovel::define-overriding-callback
    (nameopts rettype &rest args)
  (generate-overriding-callback cffi-grovel::out nameopts rettype
				args nil))

(cffi-grovel::define-wrapper-syntax cffi-grovel::define-overriding-callback*
    (nameopts rettype args c-lines)
  (generate-overriding-callback cffi-grovel::out nameopts rettype
				args c-lines))
