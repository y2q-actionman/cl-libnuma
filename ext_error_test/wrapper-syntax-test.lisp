(in-package :cl-libnuma.wrapper-syntax.test)

(defun test-parse-overriding-callback-name ()
  ;; because using 'cffi::lisp-name'
  (let ((*package* (find-package :cl-libnuma.wrapper-syntax.test)))
    (multiple-value-bind (c-overriden-function-name
			  lisp-callback-variable-name
			  c-trampoline-variable-name
			  lisp-trampoline-function-name)
	(cl-libnuma.wrapper-syntax::parse-overriding-callback-name "hoge_fuga_piyo")
      (assert-progn
       (stringp c-overriden-function-name)
       (equal c-overriden-function-name "hoge_fuga_piyo")
       (symbolp lisp-callback-variable-name)
       (equal lisp-callback-variable-name '*HOGE-FUGA-PIYO-CALLBACK*)
       (stringp c-trampoline-variable-name)
       (equal c-trampoline-variable-name "hoge_fuga_piyo_trampoline")
       (symbolp lisp-trampoline-function-name)
       (equal lisp-trampoline-function-name 'HOGE-FUGA-PIYO-TRAMPOLINE)))
    (multiple-value-bind (c-overriden-function-name
			  lisp-callback-variable-name
			  c-trampoline-variable-name
			  lisp-trampoline-function-name)
	(cl-libnuma.wrapper-syntax::parse-overriding-callback-name
	 `("test-c-o-f-n"
	   :lisp-callback-variable-name test-l-c-v-n
	   :c-trampoline-variable-name "test-c-t-v-n"
	   :lisp-trampoline-function-name test-t-f-n))
      (assert-progn
       (equal c-overriden-function-name "test-c-o-f-n")
       (equal lisp-callback-variable-name 'test-l-c-v-n)
       (equal c-trampoline-variable-name "test-c-t-v-n")
       (equal lisp-trampoline-function-name 'test-t-f-n)))
    t))

;; TODO: add 'define-overriding-callback' test, especially for 'next-library' usage.

(defun test-wrapper-syntax ()
  (and (test-parse-overriding-callback-name)
       t))
