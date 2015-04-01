(in-package :cl-libnuma.wrapper)

(include "numa.h")

;; These are static inline functions.

(defwrapper (numa-free-cpumask* "numa_free_cpumask") :void
  (b :pointer))	; The true type is (:pointer (:struct struct-bitmask))

(defwrapper (numa-free-nodemask* "numa_free_nodemask") :void
  (b :pointer))	; The true type is (:pointer (:struct struct-bitmask))
