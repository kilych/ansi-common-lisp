;; 1.1 New Tools

(defun sum (n)
  (let ((s 0))
    (dotimes (i n s)
      (incf s i))))

(sum 5)
;; => 10

(defun addn (n)
  #'(lambda (x) (+ x n)))

(funcall (addn 4) 2)
;; => 6

;; optional

(defun addn-times (n times)
  (let ((s 0))
    (dotimes (i times s)
      (setf s (funcall (addn n) s)))))

(addn-times 2 10)
;; => 20
