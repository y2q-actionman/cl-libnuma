(in-package :cl-libnuma.test-util)

(defmacro assert-progn (&body forms)
  `(progn ,@(loop for i in forms
	       collect `(assert ,i))
	  t))

(defmacro assume-condition ((&optional (condition-type 'error)) &body body)
  `(handler-case (progn ,@body)
     (,condition-type (condition)
       (values t condition))
     (:no-error (&rest last-values)
       (values nil last-values))))

(defmacro always-success (&body body)
  `(progn ,@body t))

(defmacro assert-when (form &body body)
  `(if ,form
       (assert-progn ,@body)
       t))

(defun cffi-type-exists (symbol)
  (and (>= (foreign-type-size symbol)
	   (foreign-type-alignment symbol))))

(defun cffi-enum-exists (cffi-type keyword)
  (foreign-enum-value cffi-type keyword :errorp t))

(defun grovel-constant-exists (sym)
  (and (symbol-value sym)
       (constantp sym)))
