(in-package :aoc2022.day8)

(coalton-toplevel
  (define-type Trees (Trees Ufix Ufix (Hashtable (Tuple Ufix Ufix) Ufix)))

  (declare char->integer (Char -> Optional Ufix))
  (define (char->integer c)
    (lisp (Optional Ufix) (c)
      (cl:let* ((s (cl:string c))
                (z (cl:ignore-errors (cl:parse-integer s))))
        (cl:if (cl:null z)
               None
               (Some z)))))

  (declare string->vector (String -> Vector Char))
  (define (string->vector s)
    (lisp (Vector Char) (s)
      (cl:coerce s 'cl:vector)))

  (declare lines (String -> Vector String))
  (define (lines s)
    (lisp (Vector String) (s)
      (cl:coerce (str:lines s) 'cl:vector)))

  (declare parse (String -> Trees))
  (define (parse input)
    (let ls = (lines input))
    (let ts = (ht:new))
    (vec:foreach-index
     (fn (y line)
       (vec:foreach-index
        (fn (x c)
          (let (Some i) = (char->integer c))
          (ht:set! ts (tuple x y) i))
        (string->vector line)))
     ls)
    (Trees (string:length (vec:head-unsafe ls)) (vec:length ls) ts))

  (declare visible? (Ufix -> Ufix -> Trees -> Boolean))
  (define (visible? x y ts)
    (let (Trees w h ht) = ts)
    (let treeHeight = (ht:get ht (tuple x y)))
    (or (== x 0)
        (== y 0)
        (== x (- w 1))
        (== y (- h 1))
        (all (fn (otherX)
               (< (ht:get ht (tuple otherX y)) treeHeight))
             (range 0 (- x 1)))
        (all (fn (otherX)
               (< (ht:get ht (tuple otherX y)) treeHeight))
             (range (+ x 1) (- w 1)))
        (all (fn (otherY)
               (< (ht:get ht (tuple x otherY)) treeHeight))
             (range 0 (- y 1)))
        (all (fn (otherY)
               (< (ht:get ht (tuple x otherY)) treeHeight))
             (range (+ y 1) (- h 1)))))

  (declare count-visible (Trees -> Ufix))
  (define (count-visible ts)
    (let (Trees w h _) = ts)
    (sum (map (fn (y)
                (list:countby (fn (x)
                                (visible? x y ts))
                              (range 0 (- w 1))))
              (range 0 (- h 1)))))

  (declare get-scenic-score (Ufix -> Ufix -> Trees -> Ufix))
  (define (get-scenic-score x y ts)
    (let (Trees w h ht) = ts)
    (let (Some treeHeight) = (ht:get ht (tuple x y)))
    (lisp Ufix (x y w h ht treeHeight)
      (cl:*
       (cl:loop
          for i from 0
          for other-x from (cl:1- x) downto 0
          when (cl:>= (cl:gethash (tuple other-x y) ht) treeHeight)
            return (cl:1+ i)
          finally (cl:return i))

       (cl:loop
          for i from 0
          for other-x from (cl:1+ x) below w
          when (cl:>= (cl:gethash (tuple other-x y) ht) treeHeight)
            return (cl:1+ i)
          finally (cl:return i))

       (cl:loop
          for i from 0
          for other-y from (cl:1- y) downto 0
          when (cl:>= (cl:gethash (tuple x other-y) ht) treeHeight)
            return (cl:1+ i)
          finally (cl:return i))

       (cl:loop
          for i from 0
          for other-y from (cl:1+ y) below h
          when (cl:>= (cl:gethash (tuple x other-y) ht) treeHeight)
            return (cl:1+ i)
          finally (cl:return i)))))

  (declare get-max-scenic-score (Trees -> Ufix))
  (define (get-max-scenic-score ts)
    (let (Trees w h _) = ts)
    (lisp Ufix (w h ts)
      (cl:loop for x from 0 below w maximize
        (cl:loop for y from 0 below h maximize
           (get-scenic-score x y ts))))))
