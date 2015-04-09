(in-package :cl-libnuma.ext-error.wrapper)

(include "numa.h")

(define-overriding-callback "numa_error"
    :void
  (where :string))

;;  I think there is no good way to define a C-style variadic function
;;  from Lisp.  Moreover, libnuma doesn't provide a va_list version
;;  of numa_warn().
;; 
;;  Consequently, I break down the arguments here... 
(include "stdio.h")
(include "stdarg.h")
(include "errno.h")

(define NUMA_WARN_BUFFER_SIZE 256)

(define-overriding-callback* (:c-overriden-function-name "numa_warn"
			      :c-trampoline-variable-name "numa_warn_trampoline")
    :void
  ((num :int) (fmt :string) &rest)
  ("char buffer[NUMA_WARN_BUFFER_SIZE];"
   "va_list ap;"
   "int eno = errno;"
   ""
   "va_start(ap, fmt);"
   "vsnprintf(buffer, sizeof(buffer), fmt, ap);"
   "va_end(ap);"
   "errno = eno;"
   "return (*numa_warn_trampoline)(num, buffer);")
  )
