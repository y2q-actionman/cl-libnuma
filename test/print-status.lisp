(in-package :cl-user)

(defpackage :cl-libnuma-test-print
  (:use :cl :cl-libnuma))

(in-package :cl-libnuma-test-print)

(defvar *nodes* (numa-num-configured-nodes))
(defvar *cpus* (numa-num-task-cpus))	; because numa-num-configured-cpus is not found..

(defun getpid ()
  #+allegro(excl.osi:getpid)
  #-allegro(error "gomen"))

(defun print-bitmask (bitmask type)
  (flet ((print-bitmask-with-range (range)
	   (loop for i from 0 below range
	      as set? = (numa-bitmask-isbitset bitmask i)
	      do (write-char (if set? #\X #\_))
	      when (= 4 (mod i 5))
	      do (write-char #\space))))
    (case type
      (:cpu (print-bitmask-with-range (numa-num-configured-cpus)))
      (:node (print-bitmask-with-range (numa-num-configured-nodes)))
      (t (print-bitmask-with-range (* 8 (numa-bitmask-nbytes bitmask)))))))

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
  (format t "numa-num-get-mems-allowed = ")
  (with-freeing-bitmask (bitmask (numa-get-mems-allowed))
    (print-bitmask bitmask :node))
  (terpri)
  (terpri)

  (format t "numa-num-configured-cpus = ~A~%" (numa-num-configured-cpus))
  (format t "*numa-all-nodes-ptr* = ")
  (print-bitmask *numa-all-nodes-ptr* :node)
  (terpri)
  (format t "*numa-no-nodes-ptr* = ")
  (print-bitmask *numa-no-nodes-ptr* :node)
  (terpri)
  (format t "*numa-all-cpus-ptr* = ")
  (print-bitmask *numa-all-cpus-ptr* :cpu)
  (terpri)
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
  (format t "numa-get-interleave-mask = ")
  (with-freeing-bitmask (bitmask (numa-get-interleave-mask))
    (print-bitmask bitmask :node))
  (terpri)
  (format t "numa-get-membind = ")
  (with-freeing-bitmask (bitmask (numa-get-membind))
    (print-bitmask bitmask :node))
  (terpri)
  (terpri)

  (format t "numa-get-run-node-mask = ")
  (with-freeing-bitmask (bitmask (numa-get-run-node-mask))
    (print-bitmask bitmask :node))
  (terpri)
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

  (format t "numa-sched-getaffinity for this process (~D) ~%" (getpid))
  (with-freeing-bitmask (bitmask (numa-allocate-cpumask))
    (numa-sched-getaffinity (getpid) bitmask)
    (format t "  ")
    (print-bitmask bitmask :cpu))
  (terpri)
  (format t "numa-node-to-cpus")
  (with-freeing-bitmask (bitmask (numa-allocate-cpumask))
    (loop for i from 0 below *nodes*
       do (format t "~& node ~2D " i)
       do (numa-node-to-cpus i bitmask)
       do (print-bitmask bitmask :cpu)))
  (terpri)
  (format t "numa-node-of-cpu~%")
  (loop for i from 0 below *cpus* by 10
     do (loop with j-from = i
	   with j-to = (+ i 9)
	   initially (format t "~& [~2D-~2D] " j-from j-to)
	   for j from j-from to j-to
	   as node = (numa-node-of-cpu j)
	   until (minusp node)
	   do (format t "~2D " node)))
  (terpri)
  (terpri)
  
  

  (format t "numa-pagesize = ~A~%" (numa-pagesize))
  (terpri))
  
