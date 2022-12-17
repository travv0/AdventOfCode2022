(defpackage :aoc2022.day6
  (:use :cl)
  (:local-nicknames (:a :alexandria))
  (:export :run))

(defpackage :aoc2022.day6-coalton
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames (:a :alexandria)
                    (:list :coalton-library/list))
  (:export :run))
