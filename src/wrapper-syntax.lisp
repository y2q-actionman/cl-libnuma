(cl:in-package :cl-libnuma.wrapper-syntax)

(defparameter *use-next-function-default* t)

(defparameter *overriding-callback-suffix* "_callback")

(defun parse-overriding-callback-name (nameopts)
  "Returns: (values <lisp-callback-variable-name> <c-overriden-function-name> <c-callback-variable-name>
                    <use-next-function-p>)"
  (etypecase nameopts
    (string				; foreign-name
     (values (cffi::lisp-name (cffi-grovel::strcat nameopts *overriding-callback-suffix*) t)
	     nameopts
	     (cffi-grovel::strcat nameopts *overriding-callback-suffix*)
	     *use-next-function-default*))
    (symbol				; lisp symbol -- used as a foreign function name.
     (warn "The symbol ~A is used as a foreign function name.~
If this behavior is not intended, please use ~
 (<lisp-callback-variable-name> <c-overriden-function-name>) syntax"
	   nameopts)
     (parse-overriding-callback-name (cffi::foreign-name nameopts nil)))
    (list
     (destructuring-bind (nameopts-1 nameopts-2 &key (use-next-function *use-next-function-default*))
	 nameopts
       (etypecase nameopts-1		; (symbol "string")
	 (symbol
	  (check-type nameopts-2 string)
	  (values nameopts-1
		  nameopts-2
		  (cffi-grovel::strcat nameopts-2 *overriding-callback-suffix*)
		  use-next-function))
	 (string			; ("string" symbol)
	  (check-type nameopts-2 symbol)
	  (values nameopts-2
		  nameopts-1
		  (cffi-grovel::strcat nameopts-1 *overriding-callback-suffix*)
		  use-next-function)))))))

(defun generate-overriding-callback (out nameopts rettype args c-lines)
  (multiple-value-bind (lisp-callback-variable-name 
			c-overriden-function-name
			c-callback-variable-name
			use-next-function-p)
      (parse-overriding-callback-name nameopts)
    (when use-next-function-p
      (format out "#include <dlfcn.h>~%")
      (pushnew "-D_GNU_SOURCE" cffi-grovel::*cc-flags*))
    (let* ((fret-c-name (cffi-grovel::c-type-name rettype))
	   (variadic nil)
	   (fargs (loop for arg in args
		     when (and (symbolp arg)
			       (string= (symbol-name arg)
					(symbol-name '&rest)))
		     ;; Ugly hack. '&rest' is translated to a list saying type="" and name="...".
		     collect (list "" "...")
		     and do (setf variadic t)
		     else
		     collect (list (cffi-grovel::c-type-name (second arg))
				   (cffi::foreign-name (first arg) nil))))
	   (fargnames (mapcar #'second fargs)))
      ;; C code
      (format out "~A (*~A)();~%~%" ; callback pointer
	      fret-c-name c-callback-variable-name)

      (format out "~A ~A(~{~{~A ~A~}~^, ~}){~%" ; start of function definition
	      fret-c-name c-overriden-function-name fargs)

      (format out "	if (~A) {~%"	; if callback exists..
	      c-callback-variable-name)
      (if c-lines
	  (format out "~{		~A~%~}" c-lines)
	  (format out "		return (*~A)(~{~A~^, ~});~%" ; default -- simply calls the callback.
		  c-callback-variable-name fargnames))

      (format out "	} else {~%")      ; if no callback..
      (when use-next-function-p
	(format out "		~A (*sym)() = dlsym(RTLD_NEXT, \"~A\");~%"
		fret-c-name c-overriden-function-name)
	(format out "		if (sym) return (*sym)(~{~A~^, ~});~%"
		(if variadic (butlast fargnames) fargnames)))
      (format out "		return ~@[0~];~%"
	      (not (eq rettype :void)))

      (format out "	}~%}~%~%")
      ;; lisp code
      (push `(cffi:defcvar (,lisp-callback-variable-name ,c-callback-variable-name)
		 :pointer)
	    cffi-grovel::*lisp-forms*))))
  

;; The name of wrapper-syntax must be a symbol of :cffi-grovel
;; package.  See 'cffi-grovel::form-kind'.
(cffi-grovel::define-wrapper-syntax cffi-grovel::define-overriding-callback
    (nameopts rettype &rest args)
  (generate-overriding-callback cffi-grovel::out nameopts rettype
				args nil))

(cffi-grovel::define-wrapper-syntax cffi-grovel::define-overriding-callback*
    (nameopts rettype args &rest c-lines)
  (generate-overriding-callback cffi-grovel::out nameopts rettype
				args c-lines))
