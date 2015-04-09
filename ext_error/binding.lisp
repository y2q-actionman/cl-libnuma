(in-package :cl-libnuma.ext-error)

(defcfun numa-error
    :void
  (where :string))

(defcfun numa-warn
    :void
  (number :int)
  (where :string)
  &rest)
