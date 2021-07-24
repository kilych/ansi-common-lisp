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

;; Exercises

;; 2.x.1

;; Describe what happens when the following expressions are evaluated:

;; 2.x.1a

(+ (- 5 1) (+ 3 7))
(+ 4 (+ 3 7))
(+ 4 10)
14

;; 2.x.1b

(list 1 (+ 2 3))
(list 1 5)
;; => (1 5)

;; 2.x.1c

(if (listp 1) (+ 1 2) ( + 3 4))
(if NIL (+ 1 2) (+ 3 4))
(+ 3 4)
7

;; 2.x.1d

(list (and (listp 3) t) (+ 1 2))
(list (and NIL t) (+ 1 2))
(list NIL 3)
;; => (NIL 3)

;; 2.x.2

;; Give three distinct cons expressions that return (a b c).

(cons 'a (cons 'b (cons 'c nil)))
;; => (A B C)

(cons 'a (cons 'b '(c)))
;; => (A B C)

(cons 'a '(b c))
;; => (A B C)

;; 2.x.3

;; Using car and cdr, define a function to return the fourth element
;; of a list.

(defun our-fourth (lst)
  (car (cdr (cdr (cdr lst)))))

(our-fourth '(a b c d e))
;; => D

;; 2.x.4

;; Define a function that takes two arguments and returns the greater
;; of the two.

(defun our-max (x y)
  (if (> x y) x y))

(our-max 2 5)
;; => 5

;; 2.x.5

;; What do these functions do?

;; 2.x.5a

(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))

(enigma '(a b nil d))
T

(enigma '(a b c d))
NIL

(enigma '())
NIL

(enigma '(nil))
T

;; The function named enigma answers the question, "Does the list
;; contain nil?".

;; 2.x.5b

(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
          0
          (let ((z (mystery x (cdr y))))
            (and z (+ z 1))))))

(mystery 'c '(a b c d))
2

(mystery 'e '(a b c d))
NIL

(mystery 'e '())
NIL

(mystery 'a '(c a r a t))
1

;; The function named mystery evaluates to the position of the first
;; occurence of x in the list y relative to the beginning of y. List
;; positions start at 0.
;; Evaluates to NIL if x was not found.

;; 2.x.6

;; What could occur in place of the x in each of the following
;; exchanges?

;; 2.x.6a

;; (car (x   (cdr '(a (b c) d))))
;;       |
;;       v
   (car (car (cdr '(a (b c) d))))
;; => B

;; 2.x.6b

;; (x  13 (/ 1 0))
;;  |
;;  v
   (or 13 (/ 1 0))
13

;; 2.x.6c

;; (x     #'list 1 nil)
;;  |
;;  v
   (apply #'list 1 nil)
(1)

;; 2.x.7

;; Using only operators introduced in this chapter, define a function
;; that takes a list as an argument and returns true if one of its
;; elements is a list.

(defun contains-list-p (lst)
  (and (not (null lst))
       (or (typep (car lst) 'list)
           (contains-list-p (cdr lst)))))

(contains-list-p '(a '(b c) d))
T

(contains-list-p '(a b c d))
NIL

(contains-list-p '(a nil c d))
T

(contains-list-p '())
NIL

;; 2.x.8

;; Give iterative and recursive definitions of a function that

;; 2.x.8a

;; takes a positive integer and prints that many dots.

(defun print-dotes (n)
  (if (zerop n)
      nil
      (progn
        (format t ".")
        (print-dotes (- n 1)))))

(defun print-dotes-iter (n)
  (do ((i n (- i 1)))
      ((zerop i))
    (format t ".")))

;; 2.x.8b

;; takes a list and returns the number of times the symbol a occurs in it.

(defun count-occurences (elem lst)
  (if (null lst)
      0
      (+ (count-occurences elem (cdr lst))
         (if (eql elem (car lst))
             1
             0))))

(defun count-occurences-iter (elem lst)
  (let ((count 0))
    (dolist (current lst)
      (if (eql elem current)
          (setf count (+ count 1))))
    count))

;; 2.x.9

;; A friend is trying to write a function that returns the sum of all
;; the non-nil elements in a list. He has written two versions of this
;; function, and neither of them work. Explain what's wrong with each,
;; and give a correct version:

;; 2.x.9a

(defun summit (lst)
  (remove nil lst)                      ; Remains lst the same.
  (apply #'+ lst))

(defun summit (lst)
  (apply #'+
         (remove nil lst)))

;; 2.x.9b

;; Doesn't check lst is not nil.

(car nil)
NIL

(cdr nil)
NIL

;; => infinite recursion

(defun summit (lst)
  (let ((x (car lst)))
    (if (null x)
        (summit (cdr lst))
        (+ x (summit (cdr lst))))))

(defun summit (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
        (if (null x)
            (summit (cdr lst))
            (+ x (summit (cdr lst)))))))
