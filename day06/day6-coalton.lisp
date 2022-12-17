(in-package :aoc2022.day6-coalton)

(coalton-toplevel
  (declare has-duplicate? ((Eq :a) => (List :a) -> Boolean))
  (define (has-duplicate? list)
    (match list
      ((Nil) False)
      ((Cons h t)
       (or (list:member h t) (has-duplicate? t)))))

  (declare find-marker* (Ufix -> Ufix -> (List Char) -> Ufix))
  (define (find-marker* pos len buffer)
    (if (has-duplicate? (take len buffer))
        (find-marker* (1+ pos) len (list:cdr buffer))
        pos))

  (declare find-marker (Ufix -> String -> Integer))
  (define (find-marker len buffer)
    (into (find-marker* len len (into buffer))))

  (declare run (String -> Unit))
  (define (run filename)
    (lisp Unit (filename)
      (cl:let ((input (a:read-file-into-string filename)))
        (cl:print (find-marker 4 input))
        (cl:print (find-marker 14 input))
        (coalton Unit)))))
