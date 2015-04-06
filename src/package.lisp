(in-package :cl-user)

(defpackage :cl-libnuma.grovel
  (:use)
  (:export
   #:+CHAR-BIT+
   #:+LIBNUMA-API-VERSION+
   #:size_t
   #:pid_t
   #:nodemask_t
   #:struct-bitmask
   #:mbind-flag))

(defpackage :cl-libnuma.wrapper
  (:use)
  (:export
   #:numa-free-cpumask*
   #:numa-free-nodemask*))

(defpackage :cl-libnuma
  (:use :cl :cffi
	:cl-libnuma.grovel
	:cl-libnuma.wrapper)
  (:export
   #:+cl-libnuma-target-api-version+

   ;; library name
   #:libnuma

   ;; types
   #:struct-bitmask-pointer		; cffi type
   #:libnuma-bitmask-type		; cffi type
   #:numa-bitmask
   #:nodemask_t-pointer			; cffi type
   #:mbind-flag				; cffi type

   #:make-numa-bitmask

   ;; numa(3) API
   #:numa-available

   #:numa-max-possible-node
   #:numa-num-possible-nodes
   #:numa-num-possible-cpus*
   #:numa-num-possible-cpus

   #:numa-max-node
   #:numa-num-configured-nodes
   #:numa-get-mems-allowed
   
   #:numa-num-configured-cpus
   #:*numa-all-nodes-bitmask*
   #:*numa-no-nodes-bitmask*
   #:*numa-all-cpus-bitmask*

   #:numa-num-task-cpus
   #:numa-num-task-nodes

   #:numa-parse-bitmap*
   #:numa-parse-bitmap
   #:numa-parse-nodestring
   #:numa-parse-nodestring-all
   #:numa-parse-cpustring
   #:numa-parse-cpustring-all

   #:numa-node-size*
   #:numa-node-size
   #:numa-node-size64*
   #:numa-node-size64

   #:numa-preferred
   #:numa-set-preferred
   #:numa-get-interleave-node
   #:numa-get-interleave-mask
   #:numa-set-interleave-mask
   #:numa-interleave-memory
   #:numa-bind
   #:numa-set-localalloc
   #:numa-set-membind
   #:numa-get-membind

   #:numa-alloc-onnode
   #:numa-alloc-local
   #:numa-alloc-interleaved
   #:numa-alloc-interleaved-subset
   #:numa-alloc
   #:numa-realloc
   #:numa-free

   #:numa-run-on-node
   #:numa-run-on-node-mask
   #:numa-run-on-node-mask-all
   #:numa-get-run-node-mask
   
   #:numa-tonode-memory
   #:numa-tonodemask-memory
   #:numa-setlocal-memory
   #:numa-police-memory
   #:numa-set-bind-policy
   #:numa-set-strict

   #:numa-distance

   #:numa-sched-getaffinity*
   #:numa-sched-getaffinity
   #:numa-sched-setaffinity
   #:numa-node-to-cpus*
   #:numa-node-to-cpus
   #:numa-node-of-cpu

   #:numa-allocate-cpumask*
   #:numa-free-cpumask*
   #:numa-allocate-nodemask*
   #:numa-free-nodemask*

   #:numa-bitmask-alloc*
   #:numa-bitmask-clearall*
   #:numa-bitmask-clearall
   #:numa-bitmask-clearbit*
   #:numa-bitmask-clearbit
   #:numa-bitmask-equal*
   #:numa-bitmask-equal
   #:numa-bitmask-free*
   #:numa-bitmask-isbitset*
   #:numa-bitmask-isbitset
   #:numa-bitmask-nbytes*
   #:numa-bitmask-nbytes
   #:numa-bitmask-setall*
   #:numa-bitmask-setall
   #:numa-bitmask-setbit*
   #:numa-bitmask-setbit
   #:copy-bitmask-to-nodemask*
   #:copy-nodemask-to-bitmask*
   #:copy-bitmask-to-bitmask*
   #:numa-bitmask-weight*
   #:numa-bitmask-weight

   #:numa-move-pages*
   #:numa-move-pages
   #:numa-migrate-pages

   #:numa-error
   #:*numa-exit-on-error*
   #:*numa-exit-on-warn*
   #:numa-warn

   #:numa-pagesize
   ))
