(in-package :cl-libnuma.grovel)

(include "numa.h")
(include "numaif.h")			; move_pages() flags.

;; To check compatibiity
(constant (LIBNUMA_API_VERSION "LIBNUMA_API_VERSION") :optional t)

(ctype size_t "size_t")
(ctype pid_t "pid_t")

(cstruct nodemask_t "nodemask_t")		; opaque
(cstruct struct-bitmask "struct bitmask")	; opaque

;; numa_move_pages() flags.
(constant (MPOL_MF_MOVE "MPOL_MF_MOVE"))
(constant (MPOL_MF_MOVE_ALL "MPOL_MF_MOVE_ALL"))
