(defun clgisc-2.22 (a b c d)
  (list (list a b) (list c d)))

(defun duo-cons (a b l)
  (cons a (cons b l)))

(defun two-deeper (a)
  (list (list a)))

(defun two-deeper (a)
  (cons (cons a nil) nil))

(defun pythag (x y)
  (sqrt (+ (* x x) (* y y))))
