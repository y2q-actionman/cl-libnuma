(in-package :cl-libnuma.wrapper)

(include "numa.h")

;; These are static inline functions.

(defwrapper "numa_free_cpumask" :void
  ;; BUG: this function takes a pointer to struct bitmask! (K&R C style!?)
  (b :pointer))	; The true type is (:pointer (:struct struct-bitmask))

(defwrapper "numa_free_nodemask" :void
  ;; BUG: this function takes a pointer to struct bitmask! (K&R C style!?)
  (b :pointer))	; The true type is (:pointer (:struct struct-bitmask))
