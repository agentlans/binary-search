(asdf:load-system "binary-search")

;; Create a package that uses :binary-search.
(defpackage :binary-search-example
  (:use :cl :binary-search))
(in-package :binary-search-example)

;; Find 2 in the sorted list.
;; 2 is in the sorted list from index 3 to index 5, inclusive.
(infimum-array '(0 1 1.5 2 2 2 3 5) #'<= 2) ; => 5, 2
(supremum-array '(0 1 1.5 2 2 2 3 5) #'<= 2) ; => 3, 2

;; Compare characters.
;; This list doesn't contain #\c but #\c would be after #\b but before #\d.
(infimum-array '(#\a #\b #\d) #'char<= #\c 0 2) ;=> 1, #\b
(supremum-array '(#\a #\b #\d) #'char<= #\c 0 2) ;=> 2, #\d

(infimum-array #(#\a #\b #\c) #'char<= #\z) ; => 2, #\c
(supremum-array #(#\a #\b #\c) #'char<= #\z) ; => out of bounds error

;; Estimate square root of 3 from below.
;; Search [0,2] with x error < 0.01.
(infimum
 (lambda (x) (* x x))
 #'<= 3.0 0.0 2.0 0.01)

;; Estimate pi from above.
;; -sin(x) is monotone increasing from [2,4].
;; Search that interval with x error < 0.001.
(supremum
 (lambda (x) (- (sin x)))
 #'<= 0.0 2.0 4.0 0.001)
