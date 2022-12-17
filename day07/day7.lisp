(ql:quickload '(:alexandria :serapeum :str))

(defpackage :aoc2022.day7
  (:use :cl)
  (:local-nicknames (:a :alexandria)
                    (:s :serapeum))
  (:import-from :s :~> :~>>))

(in-package :aoc2022.day7)

(defstruct (dir (:print-function (lambda (d s k)
                                   (declare (ignore k))
                                   (format s "#<DIR ~a, PARENT ~a, CHILDREN ~a>"
                                           (dir-name d)
                                           (a:when-let ((parent (dir-parent d))) (dir-name parent))
                                           (dir-children d)))))
  name parent children)

(defstruct file name size)

(defclass file-system ()
  ((root-dir :initform (make-dir :name "/") :reader root-dir)
   (current-dir :accessor current-dir)))

(defmethod initialize-instance :after ((file-system file-system) &key)
  (setf (current-dir file-system) (root-dir file-system)))

(defmethod print-object ((file-system file-system) stream)
  (format stream "#<FILE-SYSTEM (current: ~a>) ~a"
          (dir-name (current-dir file-system))
          (root-dir file-system)))

(defmethod cd ((file-system file-system) dir-name)
  (s:string-case dir-name
    (".." (a:if-let ((parent (dir-parent (current-dir file-system))))
            (setf (current-dir file-system) parent)
            (error "already at root directory")))
    ("/" (setf (current-dir file-system) (root-dir file-system)))
    (otherwise (setf (current-dir file-system) (find-dir file-system dir-name)))))

(defmethod find-child ((file-system file-system) name)
  (find-if (lambda (child)
             (etypecase child
               (dir (string= (dir-name child) name))
               (file (string= (file-name child) name))))
           (dir-children (current-dir file-system))))

(defmethod mkdir ((file-system file-system) dir-name)
  (let ((new-dir (make-dir :name dir-name :parent (current-dir file-system))))
    (push new-dir (dir-children (current-dir file-system)))
    new-dir))

(defmethod mkfile ((file-system file-system) file-name file-size)
  (let ((new-file (make-file :name file-name :size file-size)))
    (push new-file (dir-children (current-dir file-system)))
    new-file))

(defun size (path)
  (etypecase path
    (file (file-size path))
    (dir (reduce #'+ (mapcar #'size (dir-children path))))))

(defmethod run-command ((file-system file-system) line)
  (cond ((char= (char line 0) #\$)
         (let ((command (str:words (subseq line 2))))
           (s:string-ecase (first command)
             ("cd" (cd file-system (second command)))
             ("ls" t))))

        ((digit-char-p (char line 0))
         (let* ((words (str:words line))
                (size (parse-integer (first words)))
                (file-name (second words)))
           (mkfile file-system file-name size)))

        ((string= (subseq line 0 3) "dir")
         (mkdir file-system (subseq line 4)))

        (t
         (error "invalid command ~a" line))))

(defun dir-sizes (dir)
  (apply #'append
         (list (cons (dir-name dir) (size dir)))
         (~>> dir
              dir-children
              (remove-if-not (s:op (typep _ 'dir)))
              (mapcar #'dir-sizes))))

(defun parse (input)
  (let ((lines (str:lines input))
        (file-system (make-instance 'file-system)))
    (dolist (line lines)
      (run-command file-system line))
    file-system))

(let* ((file-system (parse (a:read-file-into-string "input.txt")))
       (dir-sizes (dir-sizes (root-dir file-system))))
  (print
   (~>> dir-sizes
        (remove-if (s:op (> (cdr _) 100000)))
        (reduce (s:op (+ (cdr _2) _1)) _ :initial-value 0)))

  (let* ((disk-space 70000000)
         (update-size 30000000)
         (used-space (a:assoc-value dir-sizes "/" :test 'string=))
         (unused-space (- disk-space used-space))
         (needs-deleted (- update-size unused-space)))
    (print
     (~>> dir-sizes
          (remove-if-not (s:op (>= (cdr _) needs-deleted)))
          copy-list
          (sort _ (s:op (< (cdr _1) (cdr _2))))
          first
          cdr))))
