(in-package :cl-libnuma.wrapper-syntax.test)

(defun test-parse-overriding-callback-name ()
  (let* ((wrapper-suffix-c cl-libnuma.wrapper-syntax::+overriding-callback-suffix+)
	 (wrapper-suffix-lisp
	  (string-upcase (substitute #\- #\_ wrapper-suffix-c)))
	 (test-foreign-name "hoge_fuga_piyo"))
    (multiple-value-bind (c-overriden-function-name
			  c-callback-variable-name
			  lisp-callback-variable-name
			  next-library)
	(cl-libnuma.wrapper-syntax::parse-overriding-callback-name test-foreign-name)
      (assert-progn
       (stringp c-overriden-function-name)
       (equal c-overriden-function-name test-foreign-name)
       (symbolp lisp-callback-variable-name)
       (equal (symbol-name lisp-callback-variable-name)
	      (format nil "*HOGE-FUGA-PIYO~A*" wrapper-suffix-lisp))
       (stringp c-callback-variable-name)
       (equal c-callback-variable-name
	      (format nil "hoge_fuga_piyo~A" wrapper-suffix-c))
       (equal next-library *next-library-default*)))
    (let ((test-lisp-callback-name 'hoge-callback-lisp)
	  (test-c-callback-name "hoge-callback-c")
	  (test-next-library "hoge-library"))
      (multiple-value-bind (c-overriden-function-name
			    c-callback-variable-name
			    lisp-callback-variable-name
			    next-library)
	  (cl-libnuma.wrapper-syntax::parse-overriding-callback-name
	   `(,test-foreign-name
	     :lisp-callback-variable-name ,test-lisp-callback-name
	     :c-callback-variable-name ,test-c-callback-name
	     :next-library ,test-next-library))
	(assert-progn
	 (equal c-overriden-function-name test-foreign-name)
	 (equal lisp-callback-variable-name test-lisp-callback-name)
	 (equal c-callback-variable-name test-c-callback-name)
	 (equal next-library test-next-library)))))
  t)

;; TODO: add 'define-overriding-callback' test, especially for 'next-library' usage.

(defun test-wrapper-syntax ()
  (and (test-parse-overriding-callback-name)
       t))
