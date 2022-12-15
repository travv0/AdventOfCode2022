(ql:quickload :alexandria)

(defpackage :aoc2022.day6
  (:use :cl)
  (:local-nicknames (:a :alexandria)))

(in-package :aoc2022.day6)

(defun has-duplicate-p (list)
  (if (null list)
      nil
      (or (member (first list) (rest list))
          (has-duplicate-p (rest list)))))

(defun find-marker (buffer len)
  (loop for i from len
        for chars on (coerce buffer 'list)
        unless (has-duplicate-p (subseq chars 0 len))
          return i))

(let ((input (a:read-file-into-string "input.txt")))
  (print (find-marker input 4))
  (print (find-marker input 14)))
