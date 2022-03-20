#|
MIT License

Copyright (c) 2021-2022 Alan Tseng

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|#

(defpackage :binary-search
  (:use :cl)
  (:export :out-of-bounds-error
	   :infimum
	   :supremum
	   :infimum-array
	   :supremum-array
           :find-index<
           :find-index>))
(in-package :binary-search)

(defun mid-point (a b)
  (/ (+ a b) 2))

(define-condition out-of-bounds-error (error)
  ((text :initarg :text :reader text)))

(defun infimum (f <= y a b epsilon &optional (mid-point #'mid-point))
  (assert (and (< a b) (> epsilon 0)))
  ;; Ensure that f(a) <= y < f(b).
  ;; In other words, f(a) <= y and not f(b) <= y.
  (unless (funcall <= (funcall f a) y)
    (error 'out-of-bounds-error
	   :text "Value not in range. Try a lesser lower bound"))
  (unless (not (funcall <= (funcall f b) y))
    (error 'out-of-bounds-error
	   :text "Value not in range. Try a greater upper bound."))
  ;; Bisection method
  (labels ((looper (a b)
		   (if (<= (- b a) epsilon)
		       (values a (funcall f a))
		     (let* ((c (funcall mid-point a b))
			    (ac (funcall f c)))
		       (if (funcall <= ac y)
			   (looper c b)
			 (looper a c))))))
	  (looper a b)))

(defun supremum (f <= y a b epsilon &optional (mid-point #'mid-point))
  (assert (and (< a b) (> epsilon 0)))
  ;; Ensure that f(a) < y <= f(b).
  ;; In other words, not y <= f(a) and f(b) <= y.
  (unless (not (funcall <= y (funcall f a)))
    (error 'out-of-bounds-error
	   :text "Value not in range. Try a lesser lower bound."))
  (unless (funcall <= y (funcall f b))
    (error 'out-of-bounds-error
	   :text "Value not in range. Try a greater upper bound."))
  ;; Bisection method
  (labels ((looper (a b)
		   (if (<= (- b a) epsilon)
		       (values b (funcall f b))
		     (let* ((c (funcall mid-point a b))
			    (ac (funcall f c)))
		       (if (funcall <= y ac)
			   (looper a c)
			 (looper c b))))))
	  (looper a b)))

(defun int-mid-point (a b)
  (declare (type integer a b))
  (floor (mid-point a b)))

(defun make-getter (arr)
  (lambda (i)
    (elt arr i)))

(defun last-index (arr)
  (- (length arr) 1))

(defun last-item (arr)
  (elt arr (last-index arr)))

(defun infimum-array (arr <= y &optional (a 0) (b (last-index arr)))
  (declare (type integer a b))
  (assert (< a b))
  (assert (and (<= 0 a) (< b (length arr))))
  (if (funcall <= (last-item arr) y)
      ;; Case where y is after the last element
      (values (last-index arr) (last-item arr))
    ;; Case where y is in the array
    (infimum (make-getter arr)
	     <= y a b 1
	     #'int-mid-point)))

(defun supremum-array (arr <= y &optional (a 0) (b (last-index arr)))
  (declare (type integer a b))
  (assert (< a b))
  (assert (and (<= 0 a) (< b (length arr))))
  (if (funcall <= y (elt arr 0))
      ;; Case where y is before the first element
      (values 0 (elt arr 0))
    ;; Case where y is in the array
    (supremum (make-getter arr)
	      <= y a b 1
	      #'int-mid-point)))

(defun find-index< (arr x &key (<= #'<=) (inclusive t))
  "Returns the greatest i such that arr[i] < x or arr[i] <= x. O(log n) time."
  (if inclusive
      (infimum-array arr <= x)
      (- (supremum-array arr <= x) 1)))

(defun find-index> (arr x &key (<= #'<=) (inclusive t))
  "Returns the least i such that arr[i] > x or arr[i] >= x. O(log n) time."
  (if inclusive
      (+ (infimum-array arr <= x) 1)
      (supremum-array arr <= x)))

#|
(let ((foo #(0 1 1 2 2 2 2 3)))
  ;; The number 2 is in (2,6] and [3,7) of foo
  (list (find-index< foo 2 :inclusive nil) ; 2
	(find-index< foo 2 :inclusive t) ; 6
	(find-index> foo 2 :inclusive nil) ; 3
	(find-index> foo 2 :inclusive t))) ; 7
|#
