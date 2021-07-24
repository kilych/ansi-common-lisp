;; 3.1 Conses

(defun our-listp (x)
  (or (null x) (consp x)))

(defparameter *c* (cons 1 2))

(our-listp *c*)
T

;; Why not recursive?

(defun my-listp (lst)
  (or (null lst)
      (and (consp lst)
           (my-listp (cdr lst)))))

(my-listp *c*)
NIL

(listp *c*)
T

(defun our-atom (x) (not (consp x)))

;; Note that nil is both an atom and a list.

(atom nil)
T

(listp nil)
T

;; 3.2 Equality

;; eql returns true only if its arguments are the same object.

(eql (cons 'a nil) (cons 'a nil))
NIL

(setf x (cons 'a nil))

(eql x x)
T

;; equal, essentially, returns true if its arguments would print the
;; same:

(equal x (cons 'a nil))
T

;; Works only for lists of symbols.
(defun our-equal (x y)
  (or (eql x y)
      (and (consp x)
           (consp y)
           (our-equal (car x) (car y))
           (our-equal (cdr x) (cdr y)))))

(our-equal x (cons 'a nil))
T

;; 3.3 Why Lisp Has No Pointers

(setf x '(a b c))
(setf y x)

(eql x y)
T

;; The reason Lisp has no pointers is that every value is conceptually
;; a pointer.

;; 3.4 Building Lists

(setf x '(a b c)
      y (copy-list x))
;; (A B C)

(equal x y)
T

(eql x y)
NIL

(eql nil (copy-list nil))
T

(defun our-copy-list (lst)
  (if (atom lst)
      lst
      (cons (car lst) (our-copy-list (cdr lst)))))

(append '(a b) '(c d) '(e))
;; (A B C D E)

(append '(a b))
;; (A B)

(eql '(a b) (append '(a b)))
NIL

;; 3.5 Example: Compression

;; run-length encoding

;; The function compress takes a list of atoms and returns a
;; compressed representation of it:

;; My guess. I prefer explicit state, but there could be too many ifs
;; without cond.
(defun my-compress (atoms)
  (let ((compressed nil)
        (prev nil)
        (atoms-in-row-count 0))
    (dolist (current atoms)
      (if (or (zerop atoms-in-row-count)
              (eql current prev))
          (incf atoms-in-row-count)
          (progn
            (setf compressed (update-compressed compressed atoms-in-row-count prev))
            (setf atoms-in-row-count 1)))
      (setf prev current))
    (update-compressed compressed atoms-in-row-count prev)))

(defun update-compressed (compressed atoms-in-row-count atom)
  (let ((record (if (>= atoms-in-row-count 2)
                    (list atoms-in-row-count atom)
                    atom)))
    (append compressed (list record))))

(my-compress '(1 1 1 0 1 0 0 0 0 1))
;; ((3 1) 0 1 (4 0) 1)

(my-compress '(1 1 1 0 1 0 0 0 0))
;; ((3 1) 0 1 (4 0))

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql next elt)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

(compress '(1 1 1 0 1 0 0 0 0 1))
;; ((3 1) 0 1 (4 0) 1)

(defun my-uncompress (compressed)
  (if (null compressed)
      nil
      (let ((current (car compressed)))
        (let ((uncompressed (if (atom current)
                                (list current)
                                (my-list-of (car current) (second current)))))
          (append uncompressed (my-uncompress (cdr compressed)))))))

(defun my-list-of (n elem)
  (if (zerop n)
      nil
      (cons elem (my-list-of (- n 1) elem))))

(my-uncompress '((3 1) 0 1 (4 0) 1))
;; (1 1 1 0 1 0 0 0 0 1)

(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
            (rest (uncompress (cdr lst))))
        (if (consp elt)
            (append (apply #'list-of elt)
                    rest)
            (cons elt rest)))))

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

(uncompress '((3 1) 0 1 (4 0) 1))
;; (1 1 1 0 1 0 0 0 0 1)

;; 3.6 Access

(nth 0 '(a b c))
A

(nthcdr 2 '(a b c))
;; (C)

(nthcdr 1 '(a b c))
;; (B C)

(last '(a b c))
;; (C)

(tenth '(1 2 3 4 5 6 7 8 9 10 11))
10

;; 3.7 Mapping Functions

(mapcar #'(lambda (x) (+ x 10))
        '(1 2 3))
;; (11 12 13)

(mapcar #'list
        '(a b c)
        '(1 2 3 4))
;; ((A 1) (B 2) (C 3))

;; The related maplist takes the same arguments, but calls the
;; function on successive cdrs of the lists:
(maplist #'(lambda (x) x)
         '(a b c))
;; ((A B C) (B C) (C))
