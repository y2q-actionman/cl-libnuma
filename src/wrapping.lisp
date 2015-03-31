(in-package :cl-libnuma.wrapper)

(include "numa.h")

;; These are static inline functions.

(defwrapper (numa-free-cpumask* "numa_free_cpumask") :void
  ;; The manpage says nothing, but this function takes a pointer to
  ;; struct bitmask! (K&R C style!?)
  (b :pointer))	; The true type is (:pointer (:struct struct-bitmask))

(defwrapper (numa-free-nodemask* "numa_free_nodemask") :void
  ;; The manpage says nothing, but this function takes a pointer to
  ;; struct bitmask! (K&R C style!?)
  (b :pointer))	; The true type is (:pointer (:struct struct-bitmask))
