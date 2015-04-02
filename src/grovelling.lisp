(in-package :cl-libnuma.grovel)

(include "limits.h")			; for CHAR_BIT
(include "numa.h")
(include "numaif.h")			; move_pages() flags.

(constant (+CHAR-BIT+ "CHAR_BIT"))

;; To check compatibiity
(constant (+LIBNUMA-API-VERSION+ "LIBNUMA_API_VERSION") :optional t)

(ctype size_t "size_t")
(ctype pid_t "pid_t")

(cstruct nodemask_t "nodemask_t")		; opaque
(cstruct struct-bitmask "struct bitmask")	; opaque

;; numa_move_pages() flags.
(constantenum mbind-flag
  ((:MPOL-MF-MOVE "MPOL_MF_MOVE"))
  ((:MPOL-MF-MOVE-ALL "MPOL_MF_MOVE_ALL")))
