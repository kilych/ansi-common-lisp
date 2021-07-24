;; 2.4 List Operations

(third '(a b c d))

;; 2.5 Truth

(listp '(1 2 3))

(null nil)

(not nil)

;; Special operators (different order of evaluation): if

(if (listp '(1 2 3))
    (+ 1 2)
    (+ 5 6))

(if (listp 27)
    (+ 1 2)
    (+ 5 6))

;; Macros: and, or

(and t (+ 1 2))

(or nil 27)

;; 2.6 Functions

(defun our-third (lst)
  (car (cdr (cdr lst))))

(> (+ 1 4) 3)
;; |
;; V
(defun sum-greater (x y z)
  (> (+ x y) z))

(sum-greater 1 4 3)

;; 2.7 Recursion

(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
          lst
          (our-member obj (cdr lst)))))

(our-member 2 '(1 3 2 a 4))

;; 2.9 Input and Output

(format t "~A plus ~A equals ~A.~%" 2 3 (+ 2 3))

(defun askem (string)
  (format t "~A" string)
  (read))

;; Seems to be broken. Different behavior: lispy "e" vs C-x C-e
;; Works when called from repl.
(askem "How old are you? ")

;; 2.10 Variables

(let ((x 1) (y 2))
  (+ x y))

(defun ask-number ()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
        val
        (ask-number))))

(defparameter *glob* 99)

(defconstant +limit+ (+ *glob* 1))

(boundp '*glob*)

;; 2.11 Assignment

(setf *glob* 98)

(let ((n 10))
  (setf n 2)
  n)

;; implicit declaration (bad)
(setf x (list 'a 'b 'c))

(setf (car x) 'n)

(setf a 'b
      c 'd
      e 'f)

;; 2.12 Functional Programming

(setf lst '(c a r a t))

(remove 'a lst)                         ; immutable

(setf x '(c a r a t))

(setf x (remove 'a x))

;; 2.13 Iteration

(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(show-squares 2 5)

(defun show-squares (start end)
  (if (> start end)
      'done
      (progn
        (format t "~A ~A~%" start (* start start))
        (show-squares (+ start 1) end))))

(show-squares 2 5)

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

(our-length '(a b c))

(defun our-length (lst)
  (if (null lst)
      0
      (+ (our-length (cdr lst)) 1)))

(our-length '(a b c))

;; 2.14 Functions as Objects

(function +)

(apply (function +) '(1 2 3))

(apply #'+ '(1 2 3))

(+ 1 2 3)

(apply #'+ 1 2 '(3 4 5))

(funcall #'+ 1 2 3)

(lambda (x y)
  (+ x y))

((lambda (x) (+ x 100)) 1)

(funcall #'(lambda (x) (+ x 100))
         1)

;; 2.15 Types

(typep 27 'integer)
