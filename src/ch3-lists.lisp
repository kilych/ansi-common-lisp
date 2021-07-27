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
;; A

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

;; 3.8 Trees

(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
            (our-copy-tree (cdr tr)))))

(our-copy-tree '(a (b c (d)) e))
;; (A (B C (D)) E)

(our-copy-tree 12)
12

(substitute 'y 'x '(and (integerp x) (zerop (mod x 2))))
;; (AND
;;  (INTEGERP X)
;;  (ZEROP (MOD X 2)))

;; works with trees
(subst 'y 'x '(and (integerp x) (zerop (mod x 2))))
;; (AND
;;  (INTEGERP Y)
;;  (ZEROP (MOD Y 2)))

(defun our-subst (new old tree)
  (if (eql tree old)
      new
      (if (atom tree)
          tree
          (cons (our-subst new old (car tree))
                (our-subst new old (cdr tree))))))

;; works with trees
(our-subst 'y 'x '(and (integerp x) (zerop (mod x 2))))
;; (AND
;;  (INTEGERP Y)
;;  (ZEROP (MOD Y 2)))

;; 3.9 Understanding Recursion

(defun len (lst)
  (if (null lst)
      0
      (+ (len (cdr lst)) 1)))

;; We can assures ourselves that this function is correctby verifying
;; two things:
;; 1. That it works for lists of length 0. [Base case.]
;; 2. Given that it works ofr lists of length n, that it also works
;; for lists of length n + 1.

;; 3.10 Sets

(member 'b '(a b c))
;; (B C)

(member '(a) '((a) (z)))
NIL

(member '(a) '((a) (z)) :test #'equal)
;; ((A) (Z))

(member 'a '((a b) (c d)))
NIL

(member 'a '((a b) (c d)) :key #'car)
;; ((A B) (C D))

(member-if #'oddp '(2 3 4))
;; (3 4)

;; my guess
(defun my-member-if (predicate lst)
  (if (null lst)
      nil
      (if (funcall predicate (car lst))
          lst
          (my-member-if predicate (cdr lst)))))

(my-member-if #'oddp '(2 3 4))
;; (3 4)

(defun our-member-if (fn lst)
  (and (consp lst)
       (if (funcall fn (car lst))
           lst
           (our-member-if fn (cdr lst)))))

(our-member-if #'oddp '(2 3 4))
;; (3 4)

(adjoin 'b '(a b c))
;; (A B C)

(adjoin 'z '(a b c))
;; (Z A B C)

(union '(a b c) '(c b s))
;; (A C B S)

(intersection '(a b c) '(b b c))
;; (C B)

(set-difference '(a b c d e) '(b e))
;; (D C A)

;; Since there is no notion of ordering in a set, these functions do
;; not necessarily bother to preservethe order of elements found in
;; the original lists. The call to set-difference might just as well
;; have returned (d c a), for example.

;; 3.11 Sequences

;; In Common Lisp, sequences include both lists and vectors.

(length '(a b c))
3

(subseq '(a b c d) 1 2)
;; (B)

(subseq '(a b c d) 1)
;; (B C D)

(reverse '(a b c))
;; (C B A)

;; destructive
(sort '(0 2 1 3 8) #'>)
;; (8 3 2 1 0)

(defun nthmost (n lst)
  (nth (- n 1)
       (sort (copy-list lst) #'>)))

(nthmost 2 '(0 2 1 3 8))
3

(every #'oddp '(1 3 5))
T

(some #'evenp '(1 2 3))
T

(every #'> '(1 3 5) '(0 2 4))
T

;; 3.12 Stacks

;; Macros push and pop are defined in terms of setf. It's easy to
;; translate calls if the arguments are constants or variables.

;; (push obj lst) => (setf lst (car obj lst)

;; (pop lst) =>
;; (let ((x (car lst)))
;;   (setf lst (cdr lst))
;;   x)

(setf x '(b))
;; (B)
(push 'a x)
;; (A B)
(setf y x)
;; (A B)
(pop x)
;; A
x
;; (B)
y
;; (A B) [changes not shared because of macro implementation with var
;; name rebinding]

(defun our-reverse (lst)
  (let ((acc nil))
    (dolist (elem lst)
      (push elem acc))
    acc))

(our-reverse '(a b c d e))
;; (E D C B A)

;; non-destructive
(let ((lst '(1 2 3 4)))
  (our-reverse lst)
  lst)
;; (1 2 3 4)

;; pushnew uses adjoin instead of cons
(let ((x '(a b)))
  (pushnew 'c x)
  (pushnew 'a x)
  x)
;; (C A B)

;; 3.13 Dotted Lists

;; not works for cyclic lists
(defun proper-list? (lst)
  (or (null lst)
      (and (consp lst)
           (proper-list? (cdr lst)))))

'(a . (b . (c . nil)))
;; (A B C)

(cons 'a (cons 'b (cons 'c 'd)))
;; (A B C . D)

;; 3.14 Assoc-lists

(setf trans '((+ . "add") (- . "subtract")))
;; ((+ . "add") (- . "subtract"))

(assoc '+ trans)
;; (+ . "add")

(assoc '* trans)
NIL

;; Graham's implementation doesn't check pair is cons.
(defun our-assoc (key alist)
  (and (consp alist)
       (let ((pair (car alist)))
         (if (and (consp pair)
                  (eql key (car pair)))
             pair
             (our-assoc key (cdr alist))))))

(our-assoc '+ trans)
;; (+ . "add")

(our-assoc '* trans)
NIL

;; Like member, the real assoc takes keyword arguments, including :
;; test and :key. Common Lisp also defines an assoc-if, which is to
;; assoc what member-if is to member.

(assoc '(a) '(((a) . b) (z)))
NIL

(assoc '(a) '(((a) . b) (z)) :test #'equal)
;; ((A) . B)

(assoc 'a '(((a) . b) ((c) . d)))
NIL

(assoc 'a '(((a) . b) ((c) . d)) :key #'car)
;; ((A) . B)

(assoc-if #'oddp '((2 a) (3 b) (4 c)))
;; (3 B)

;; 3.15 Example: Shortest Path

;; 3.16 Garbage

;; Where does garbage come from? Let's create some:
(setf lst (list 'a 'b 'c))
;; (A B C)

(setf lst nil)
NIL

;; Actually, we do have a way of reaching the list, for a bit. The
;; globals *, **, and *** are always set to the the last three values
;; returned to the toplevel. These variables are useful in debugging.

;; Consing is ok in prototypes and experiments, at least. And if you
;; take advantage of the flexibility that lists give you in the early
;; stages of a program, you're more likely to produce something that
;; survives to the later stages.
