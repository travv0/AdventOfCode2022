(in-package :aoc2022.day10)

(defclass cpu ()
  ((x-register :initform 1 :reader x-register)
   (cycle :initform 0 :reader cycle)))

(defclass display ()
  ((sprite :initform '(0 1 2) :reader sprite)
   (pixels :initform (make-array '(6 40)) :reader pixels)
   (current-pixel :initform 0 :reader current-pixel)
   (current-row :initform 0 :reader current-row)))

(defmethod update-sprite ((display display) position)
  (setf (slot-value display 'sprite)
        (loop for i from (1- position) to (1+ position) collect i)))

(defmethod draw-pixel ((display display))
  (with-slots (sprite pixels current-pixel current-row) display
    (setf (aref pixels current-row current-pixel)
          (not (not (member current-pixel sprite))))
    (when (= current-pixel 39)
      (incf current-row))
    (setf current-pixel (mod (1+ current-pixel) 40))))

(defmethod print-pixels ((display display))
  (loop for y from 0 to 5 do
    (loop for x from 0 to 39 do
      (format t "~c" (if (aref (pixels display) y x) #\# #\.)))
    (terpri)))

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

(defmethod execute ((cpu cpu) (display display) instructions)
  (loop with signal-strengths = ()
        for (instruction x) in instructions do
          (handler-bind ((cycle (lambda (c)
                                  (declare (ignore c))
                                  (draw-pixel display)
                                  (when (= 0 (- (mod (cycle cpu) 40) 20))
                                    (push (signal-strength cpu) signal-strengths)))))
            (update-sprite display (x-register cpu))
            (ecase instruction
              (noop (noop cpu))
              (addx (addx cpu x))))
        finally (return (reverse signal-strengths))))

(let ((instructions (parse (a:read-file-into-string "input.txt")))
      (cpu (make-instance 'cpu))
      (display (make-instance 'display)))
  (print (reduce #'+ (execute cpu display instructions)))
  (terpri)
  (print-pixels display))
