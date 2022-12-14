(ql:quickload '(:alexandria :serapeum :fiveam :cl-ppcre :str))

(defpackage :aoc2022.day5
  (:use :cl)
  (:local-nicknames (:a :alexandria)
                    (:s :serapeum))
  (:import-from :serapeum :~> :~>>))

(in-package :aoc2022.day5)

(defparameter *test-input* "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defun get-stack-indices (stacks-input)
  (loop for c across (a:last-elt (str:lines stacks-input))
        for i from 0
        when (string/= c #\space)
          collect (list (parse-integer (s:string+ c)) i)))

(defun parse-stacks (stacks-input)
  (let ((stack-indices (get-stack-indices stacks-input))
        (stacks (make-hash-table)))
    (loop for (stack index) in stack-indices do
      (loop for line in (reverse (butlast (str:lines stacks-input))) do
        (when (< index (length line))
          (let ((crate (elt line index)))
            (when (char/= crate #\space)
              (push crate (gethash stack stacks)))))))
    stacks))

(defstruct movement count from to)

(defun parse-movements (movements-input)
  (mapcar (lambda (line)
            (let ((words (str:words line)))
              (make-movement :count (parse-integer (elt words 1))
                             :from (parse-integer (elt words 3))
                             :to (parse-integer (elt words 5)))))
          (str:lines movements-input)))

(defun parse-input (input)
  (destructuring-bind (stacks-input movements-input)
      (str:split (s:string+ #\newline #\newline)
                 (str:replace-all (s:string+ #\return) "" input))
    (let ((stacks (parse-stacks stacks-input))
          (movements (parse-movements movements-input)))
      (list stacks movements))))

(defun old-process-movement (movement stacks)
  (dotimes (i (movement-count movement))
    (let ((crate (pop (gethash (movement-from movement) stacks))))
      (push crate (gethash (movement-to movement) stacks))))
  stacks)

(defun new-process-movement (movement stacks)
  (let* ((from-stack (gethash (movement-from movement) stacks))
         (movement-count (movement-count movement))
         (move-crates (subseq from-stack 0 movement-count))
         (stay-crates (subseq from-stack movement-count)))
    (setf (gethash (movement-to movement) stacks) (append move-crates (gethash (movement-to movement) stacks)))
    (setf (gethash (movement-from movement) stacks) stay-crates)
    stacks))

(defun process-movements (f movements stacks)
  (dolist (movement movements)
    (funcall f movement stacks))
  stacks)

(defun get-top-crates (stacks)
  (format nil "~{~c~}"
          (loop for i from 1
                for stack = (gethash i stacks)
                while stack
                collect (first stack))))

(defparameter *input* (a:read-file-into-string "input.txt"))

(print (destructuring-bind (stacks movements)
           (parse-input *input*)
         (get-top-crates (process-movements #'old-process-movement movements stacks))))

(print (destructuring-bind (stacks movements)
           (parse-input *input*)
         (get-top-crates (process-movements #'new-process-movement movements stacks))))

(fiveam:def-suite :aoc2022.day5)
(fiveam:in-suite :aoc2022.day5)

(fiveam:def-test test-part-1 ()
  (fiveam:is (string= (destructuring-bind (stacks movements)
                          (parse-input *test-input*)
                        (get-top-crates (process-movements #'old-process-movement movements stacks)))
                      "CMZ")))

(fiveam:def-test test-part-2 ()
  (fiveam:is (string= (destructuring-bind (stacks movements)
                          (parse-input *test-input*)
                        (get-top-crates (process-movements #'new-process-movement movements stacks)))
                      "MCD")))

(fiveam:def-test test-get-stack-indices ()
  (fiveam:is (equal (get-stack-indices "
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3") '((1 1) (2 5) (3 9)))))
