(in-package :cl-libnuma)

(include "numa.h")
(include "numaif.h")			; move_pages() flags.

(ctype size_t "size_t")
(ctype pid_t "pid_t")

(cstruct nodemask "nodemask_t")		; opaque
(cstruct bitmask "struct bitmask")	; opaque

;; numa_move_pages() flags.
(constant (MPOL_MF_MOVE "MPOL_MF_MOVE"))
(constant (MPOL_MF_MOVE_ALL "MPOL_MF_MOVE_ALL"))
