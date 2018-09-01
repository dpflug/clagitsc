(defpackage :clgisc
  (:use :common-lisp :sdraw))
(in-package :clgisc)

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

(defun my-abs (x)
  (if (< x 0) (- x) x))

(defun throw-die ()
  "Roll 1d6 and return result."
  (1+ (random 6)))

(defun throw-dice ()
  "Roll 2d6, return list of the results."
  (list (throw-die) (throw-die)))

(defun snake-eyes-p (throw)
  "Takes a throw. Determines if the throw was snake eyes."
  (equal (list 1 1) throw))

(defun boxcars-p (throw)
  "Takes a throw. Determines if the throw was boxcars."
  (equal (list 6 6) throw))

(defun throw-value (throw)
  "Takes a throw. Returns the sum."
  (+ (first throw) (second throw)))

(defun instant-win-p (throw)
  "Takes a throw. Determines if it was an instant win."
  (member (throw-value throw) '(7 11)))

(defun instant-loss-p (throw)
  "Takes a throw. Determines if it was an instant loss."
  (member (throw-value throw) '(2 3 12)))

(defun say-throw (throw)
  (cond ((snake-eyes-p throw) :snake-eyes)
        ((boxcars-p throw) :boxcars)
        (t (throw-value throw))))

(defun throw-result-msg (throw)
  (cond ((instant-loss-p throw) "You lose. :(")
        ((instant-win-p throw) "You win!")
        (t (format t "Your point is ~A" (apply #'+ throw)))))

(defun score (throw)
  (list 'throw (first throw)
        'and (second throw)
        '--
        (say-throw throw)
        '--))

(defun craps ()
  (let ((throw (throw-dice)))
    (append (score throw)
            (cond ((instant-win-p throw) '(you win))
                  ((instant-loss-p throw) '(you lose))
                  (t (list 'your 'point 'is
                           (throw-value throw)))))))

;; page 164
(defun try-for-point (point)
  (let ((throw (throw-dice)))
    (append (score throw)
            (cond ((or (instant-win-p throw)
                       (= point (throw-value throw)))
                   '(you win))
                  ((instant-loss-p throw) '(you lose))
                  (t (list '(throw again)))))))
