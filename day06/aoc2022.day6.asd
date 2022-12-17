(defsystem #:aoc2022.day6
  :depends-on (#:alexandria #:coalton)
  :serial t
  :components ((:file "packages")
               (:file "day6")
               (:file "day6-coalton")))
