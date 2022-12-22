(in-package :aoc2022.day10)

(defclass cpu ()
  ((x-register :initform 1 :reader x-register)
   (cycle :initform 0 :reader cycle)))

(define-condition cycle () ())

(defmethod addx ((cpu cpu) v)
  (dotimes (i 2)
    (incf (slot-value cpu 'cycle))
    (signal 'cycle))
  (incf (slot-value cpu 'x-register) v)
  cpu)

(defmethod noop ((cpu cpu))
  (incf (slot-value cpu 'cycle))
  (signal 'cycle)
  cpu)

(defmethod signal-strength ((cpu cpu))
  (* (cycle cpu) (x-register cpu)))

(defun parse (s)
  (~>> s
       str:lines
       (mapcar #'parse-line)))

(defun parse-line (line)
  (let ((words (str:words line)))
    (s:string-ecase (first words)
      ("noop" '(noop))
      ("addx" (list 'addx (parse-integer (second words)))))))

(defmethod execute ((cpu cpu) instructions)
  (loop with signal-strengths = ()
        for (instruction x) in instructions do
          (handler-bind ((cycle (lambda (c)
                                  (declare (ignore c))
                                  (when (= 0 (- (mod (cycle cpu) 40) 20))
                                    (push (signal-strength cpu) signal-strengths)))))
            (ecase instruction
              (noop (noop cpu))
              (addx (addx cpu x))))
        finally (return (reverse signal-strengths))))

(let ((instructions (parse (a:read-file-into-string "input.txt"))))
  (print (reduce #'+ (execute (make-instance 'cpu) instructions))))
