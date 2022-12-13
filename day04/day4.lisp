(ql:quickload '(:alexandria :serapeum))

(defpackage :aoc2022.day4
  (:use :cl)
  (:local-nicknames (:a :alexandria)
                    (:s :serapeum))
  (:import-from :serapeum :~> :~>>))

(in-package :aoc2022.day4)

(defparameter *test-input* "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defun parse-line (line)
  (~>> line
       (s:split-sequence #\,)
       (mapcar (lambda (s) (destructuring-bind (a b)
                               (~>> s
                                    (s:split-sequence #\-)
                                    (mapcar #'parse-integer))
                             (range a b))))))

(defun parse-input (input)
  (let ((lines (~>> input
                    s:trim-whitespace
                    (s:split-sequence #\newline)
                    (mapcar #'s:trim-whitespace))))
    (mapcar #'parse-line lines)))

(defun range (start stop)
  (a:iota (1+ (- stop start)) :start start))

(defun complete-overlap-p (section1 section2)
  (let ((intersect-length (length (intersection section1 section2))))
    (or (= (length section1) intersect-length)
        (= (length section2) intersect-length))))

(defparameter *sections*
  (parse-input (a:read-file-into-string "input.txt")))

(defun any-overlap-p (section1 section2)
  (not (null (intersection section1 section2))))

(defun count-overlaps (f sections)
  (count-if (s:op (apply f _)) sections))

(print (count-overlaps #'complete-overlap-p *sections*))
(print (count-overlaps #'any-overlap-p *sections*))
