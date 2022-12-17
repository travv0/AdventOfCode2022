(ql:quickload :alexandria)

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

(defun run (&optional (input "input.txt"))
  (let ((input (a:read-file-into-string input)))
    (print (find-marker input 4))
    (print (find-marker input 14))))

