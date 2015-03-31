(cl:in-package :cl-libnuma.wrapper-syntax)

;; Switch
(defparameter *next-library-default* nil)
(defparameter *print-error-message* t)

(defconstant +next-library-specials+
  '(:RTLD_NEXT :RTLD_DEFAULT))
(defconstant +overriding-callback-suffix+ "_callback")
(defconstant +return-value-name+ "cffi_overrider_return_value")
(defconstant +handle-name+ "cffi_overrider_handle")
(defconstant +dl-symbol-name+ "cffi_overrider_dl_symbol")
(defconstant +error-message-name+ "cffi_overrider_msg")


(defun parse-overriding-callback-name (nameopts)
  "Returns: (values <c-overriden-function-name> <lisp-callback-variable-name>
                    <c-callback-variable-name> <next-library>)"
  (flet ((generate-results
	     (c-overriden-function-name
	      &key
	      (lisp-callback-variable-name
	       (cffi::lisp-name (cffi-grovel::strcat c-overriden-function-name
						     +overriding-callback-suffix+)
				t))
	      (c-callback-variable-name
	       (cffi-grovel::strcat c-overriden-function-name
				    +overriding-callback-suffix+))
	      (next-library *next-library-default*))
	   (values c-overriden-function-name
		   lisp-callback-variable-name
		   c-callback-variable-name
		   next-library)))
    (etypecase nameopts
      (string				; foreign-name
       (generate-results nameopts))
      (list
       (apply #'generate-results nameopts)))))

(defun generate-overriding-callback (out nameopts rettype args c-lines)
  (multiple-value-bind (c-overriden-function-name
			lisp-callback-variable-name 
			c-callback-variable-name
			next-library)
      (parse-overriding-callback-name nameopts)
    (when *print-error-message*
      (format out "#include <stdio.h>~%"))
    (when next-library
      (format out "#include <dlfcn.h>~%")
      (when (member next-library +next-library-specials+)
	(pushnew "-D_GNU_SOURCE" cffi-grovel::*cc-flags*)))
    (let* ((fret-c-name (cffi-grovel::c-type-name rettype))
	   (return-value-name
	    (if (not (eq rettype :void)) +return-value-name+))
	   (variadic nil)
	   (fargs (loop for arg in args
		     when (and (symbolp arg)
			       (string= (symbol-name arg)
					(symbol-name '&rest)))
		     ;; Ugly hack. '&rest' is translated to a list saying type="..." and name="".
		     collect (list "..." "")
		     and do (setf variadic t)
		     else
		     collect (list (cffi-grovel::c-type-name (second arg))
				   (cffi::foreign-name (first arg) nil))))
	   (fargnames (mapcar #'second fargs))
	   (c-lines (or c-lines
			;; default -- simply calls the callback.
			(list (format nil "return (*~A)(~{~A~^, ~});"
				      c-callback-variable-name fargnames)))))
      ;; C code
      (format out "~A (*~A)();~%~%" ; callback pointer
	      fret-c-name c-callback-variable-name)

      (format out "~A ~A(~{~{~A ~A~}~^, ~}){~%" ; start of function definition
	      fret-c-name c-overriden-function-name fargs)

      (format out "~1Tif (~A) {~{~&~2T~A~%~}" ; if callback exists..
	      c-callback-variable-name c-lines)

      (format out "~1T} else {~%")      ; if no callback..

      (when return-value-name
	(format out "~2T~A ~@[~A~];~%" fret-c-name return-value-name))

      (unless (or (null next-library)
		  (and (symbolp next-library)
		       (string= (symbol-name next-library) "NIL")))
	(labels ((call-next ()
		   (format out "~3T~A (*~A)() = dlsym(~A, \"~A\");~%"
			   fret-c-name +dl-symbol-name+
			   +handle-name+ c-overriden-function-name)
		   (format out "~3Tif (!~A) {~%" +dl-symbol-name+)
		   (when *print-error-message*
		     (format out "~4Tchar *~A = dlerror();~%" +error-message-name+)
		     (format out "~4Tfprintf(stderr, \"dlsym() returned NULL: %s\\n\", ~A ? ~:*~A : \"(successed)\");~%"
			     +error-message-name+))
		   (format out "~3T} else {~%")
		   (format out "~4T~@[~A = ~](*~A)(~{~A~^, ~});~%"
			   return-value-name +dl-symbol-name+
			   (if variadic (butlast fargnames) fargnames))
		   (format out "~3T}~%"))
		 (load-lib (libname)
		   (format out "~2Tvoid* ~A = dlopen(\"~A\", RTLD_LAZY);~%" +handle-name+ libname)
		   (format out "~2Tif (!~A) {~%" +handle-name+)
		   (when *print-error-message*
		     (format out "~3Tfprintf(stderr, \"dlopen() error: %s\\n\", dlerror());~%"))
		   (format out "~2T} else {~%")
		   (call-next)
		   (format out "~3Tdlclose(~A);~%" +handle-name+)
		   (format out "~2T}~%")
		   )
		 (use-special-lib (libname)
		   (format out "~2Tvoid* ~A = ~A;~%" +handle-name+ libname)
		   (call-next)))
	  (cond ((member next-library +next-library-specials+) ; special handlers
		 (use-special-lib (symbol-name next-library)))
		((stringp next-library) ; use dlopen() and dlsym()
		 (load-lib next-library))
		((symbolp next-library) ; load-foreign-library and dlsym()
		 (load-lib (foreign-library-pathname next-library))))))

      (format out "~2Treturn ~@[~A~];~%" return-value-name) ; default for no callback.
      ;; (If 'dlopen()' or 'dlsym()' failed, this wrapper may return an undefined value.)

      (format out "~1T}~%}~%~%")
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
