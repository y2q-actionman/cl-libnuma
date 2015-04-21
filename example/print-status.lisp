;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This work is free. You can redistribute it and/or modify it under
;; the terms of the Do What The Fuck You Want To Public License,
;; Version 2, as published by Sam Hocevar. See http://www.wtfpl.net/
;; for more details.

(in-package :cl-user)

(defpackage :cl-libnuma.example.print
  (:use :cl :cl-libnuma))

(in-package :cl-libnuma.example.print)

(defvar *nodes* (numa-num-configured-nodes))
(defvar *cpus* (numa-num-task-cpus))	; because numa-num-configured-cpus is not found..

(defun print-current-status ()
  (fresh-line)

  (format t "numa-available = ~A~%" (numa-available))
  (terpri)

  (format t "numa-max-possible-node = ~A~%" (numa-max-possible-node))
  (format t "numa-num-possible-nodes = ~A~%" (numa-num-possible-nodes))
  (format t "numa-num-possible-cpus = ~A~%" (numa-num-possible-cpus))
  (terpri)

  (format t "numa-max-node = ~A~%" (numa-max-node))
  (format t "numa-num-configured-nodes = ~A~%" (numa-num-configured-nodes))
  (format t "numa-num-get-mems-allowed = ~A~%" (numa-get-mems-allowed))
  (terpri)

  (format t "numa-num-configured-cpus = ~A~%" (numa-num-configured-cpus))
  (format t "*numa-all-nodes-bitmask* = ~A~%" *numa-all-nodes-bitmask*)
  (format t "*numa-no-nodes-bitmask* = ~A~%" *numa-no-nodes-bitmask*)
  (format t "*numa-all-cpus-bitmask* = ~A~%" *numa-all-cpus-bitmask*)
  (terpri)

  (format t "numa-num-task-cpus = ~A~%" (numa-num-task-cpus))
  (format t "numa-num-task-nodes = ~A~%" (numa-num-task-nodes))
  (terpri)

  (loop for i from 0 below *nodes*
     do (format t "node ~A~%" i)
       (format t "  numa-node-size = ~A~%"
	       (multiple-value-list (numa-node-size i)))
       (format t "  numa-node-size64 = ~A~%"
	       (multiple-value-list (numa-node-size64 i))))
  (terpri)

  (format t "numa-preferred = ~A~%" (numa-preferred))
  (format t "numa-get-interleave-node = ~A~%" (numa-get-interleave-node))
  (format t "numa-get-interleave-mask = ~A~%" (numa-get-interleave-mask))
  (format t "numa-get-membind = ~A~%" (numa-get-membind))
  (terpri)

  (format t "numa-get-run-node-mask = ~A~%" (numa-get-run-node-mask))
  (terpri)

  (format t "numa-distance~%")
  (loop
     initially
       (format t "~&    ")
       (loop for i from 0 below *nodes*
	  do (format t "~3D" i))
     for i from 0 below *nodes*
     do (format t "~&~3D " i)
     do (loop for j from 0 below *nodes*
	   do (format t "~3D" (numa-distance i j)))
     finally
       (terpri))
  (terpri)

  (format t "numa-sched-getaffinity for this process~%")
  (format t "  ~A~%" (numa-sched-getaffinity 0))
  (format t "numa-node-to-cpus")
  (loop for i from 0 below *nodes*
     do (format t "~& node ~2D ~A" i (numa-node-to-cpus i)))
  (terpri)
  (format t "numa-node-of-cpu~%")
  (loop for i from 0 below *cpus* by 10
     do (loop with j-from = i
	   with j-to = (+ i 9)
	   initially (format t "~& [~2D-~2D] " j-from j-to)
	   for j from j-from to j-to
	   as node = (numa-node-of-cpu j)
	   while node
	   do (format t "~2D " node)))
  (terpri)
  (terpri)
  
  

  (format t "numa-pagesize = ~A~%" (numa-pagesize))
  (terpri))
  
