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
  "Takes a throw. Returns a number or name for the roll."
  (cond ((snake-eyes-p throw) :snake-eyes)
        ((boxcars-p throw) :boxcars)
        (t (throw-value throw))))

(defun score (throw)
  "Takes a throw. Returns the first part of the scoring message."
  (list 'throw (first throw)
        'and (second throw)
        '--
        (say-throw throw)
        '--))

(defun craps ()
  "Plays the first roll of Craps."
  (let ((throw (throw-dice)))
    (append (score throw)
            (cond ((instant-win-p throw) '(you win))
                  ((instant-loss-p throw) '(you lose))
                  (t (list 'your 'point 'is
                           (throw-value throw)))))))

(defun try-for-point (point)
  "Takes a point and plays Craps against it."
  (let ((throw (throw-dice)))
    (append (score throw)
            (cond ((or (instant-win-p throw)
                       (= point (throw-value throw)))
                   '(you win))
                  ((instant-loss-p throw) '(you lose))
                  (t (list '(throw again)))))))

                                        ; 6.26
(defun right-side (desc)
  "Takes a description of 2 objects, delimited by '-vs-'. Return the second
  object."
  (cdr (member '-vs- desc)))

(defun left-side (desc)
  "Takes a description of 2 objects,
  delimited by '-vs-'. Return the first object."
  (reverse (cdr (member '-vs- (reverse desc)))))

(defun count-common (desc)
  "Takes a description of 2 objects, delimited by '-vs'. Return a count of their
  similarities."
  (length (intersection (right-side desc) (left-side desc))))

(defun compare (desc)
  "Takes a description of 2 objects, delimited by '-vs-'. Return a count of
  their similarities, with flavor text."
  (cons (count-common desc) '(common features)))

(defvar *nerd-states*)
(setf *nerd-states*
'((sleeping eating)
  (eating waiting-for-a-computer)
  (waiting-for-a-computer programming)
  (programming debugging)
  (debugging sleeping)))

(defun nerdus (state)
  "Takes a nerd-state. Returns the next state to happen."
  (cadr (assoc state *nerd-states*)))

(defun sleepless-nerdus (state)
  "Takes a nerd-state. Returns the next state to happen when the nerd is
  sleepless."
  (let ((maybe (nerdus state)))
    (if (eq maybe 'sleeping)
        (nerdus maybe)
        maybe)))

(defun nerd-on-caffeine (state)
  "Takes a nerd-state. Returns the next state to happen when the nerd is on
  caffeine."
  (nerdus (nerdus state)))

; 7.8
(defun roughly-equal (x k)
  "Finds the first item of a list (x) that's +- 10 a target (k)."
  (find-if #'(lambda (xs)
               (if (and (> (+ k 10) xs)
                        (< (- k 10) xs))
                   t
                   nil))
           x))

(defun find-nested (l)
  "Finds the first item of a list that is, itself, a non-nil list."
  (find-if #'(lambda (ls)
               (if (equal (type-of ls) 'cons)
                   ls
                   nil))
           l))

(defvar *note-table*)
(setf *note-table*
  '((c 1)
    (c-sharp 2)
    (d 3)
    (d-sharp 4)
    (e 5)
    (f 6)
    (f-sharp 7)
    (g 8)
    (8-sharp 9)
    (a 10)
    (a-sharp 11)
    (b 12)))

(defun numbers (notes)
  "Takes a list of notes and returns a numbers."
  (mapcar #'(lambda (note)
              (cadr (assoc note *note-table*)))
          notes))

(defun notes (numbers)
  "Takes a list of numbers and returns a note."
  (mapcar #'(lambda (number)
              (car (rassoc (list number) *note-table* :test #'equal)))
          numbers))

(defun raise (n numbers)
  "Raise each number in the list by a value."
  (mapcar #'(lambda (number)
              (+ number n))
          numbers))

(defun normalize (numbers)
  "Normalizes all note-numbers to the same octave"
  (mapcar #'(lambda (number)
              (let ((new-note (mod number 12)))
                (if (= new-note 0)
                    12
                    new-note)))
          numbers))

(defun transpose (n song)
  "Takes a value to transpose by and a list of notes. Returns transposed notes."
  (notes
   (normalize
    (raise n (numbers song)))))

; 7.15
(defun rank (card)
  "Returns the rank of a given card."
  (car card))

(defun suit (card)
  "Returns the suit of a given card"
  (cadr card))

(defvar my-hand)
(setf my-hand
  '((3 hearts)
    (5 clubs)
    (2 diamonds)
    (4 diamonds)
    (ace spades)))

(defun count-suit (suit hand)
  "Takes a suit and a hand of cards. Returns the number of cards belonging to
  that suit."
  (length (remove-if-not
           #'(lambda (card) (eql suit (suit card)))
           hand)))

(defvar colors)
(setf colors
      '((clubs black)
        (diamonds red)
        (hearts red)
        (spades black)))

(defun color-of (card)
  "Takes a card and returns the color of its suit."
  (cadr (assoc (cadr card) colors)))

(defun first-red (hand)
  "Find the first red card in a hand."
  (find-if #'(lambda (card)
               (eql 'red (color-of card)))
           hand))

(defun black-cards (hand)
  "Return a hand with all the red cards removed."
  (remove-if #'(lambda (card)
                 (eql 'red (color-of card)))
             hand))

(defun what-ranks (suit hand)
  "Return the ranks of all cards matching suit."
  (mapcar #'rank
          (remove-if-not #'(lambda (card)
                             (eql suit (suit card)))
                         hand)))

(defvar *all-ranks*)
(setf *all-ranks*
      '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun higher-rank-p (card1 card2)
  "Determines if the first card given is higher than the second."
  (member card1 (member card2 *all-ranks*)))

(defun beforep (x y l)
  "Returns true if X appears before Y in L."
  (member y (member x l)))

(defun high-card (hand)
  "Returns the highest card in a hand."
  (reduce #'(lambda (card1 card2)
              (if (higher-rank-p card1 card2)
                  card1
                  card2))
          hand))

(defvar *database*)
(setf *database*
      '((b1 shape brick)
        (b1 color green)
        (b1 material wood)
        (b1 size small)
        (b1 supported-by b2)
        (b1 supported-by b3)
        (b2 shape brick)
        (b2 color red)
        (b2 material plastic)
        (b2 size small)
        (b2 supports b1)
        (b2 left-of b3)
        (b3 shape brick)
        (b3 color red)
        (b3 size small)
        (b3 supports b1)
        (b3 right-of b2)
        (b4 shape pyramid)
        (b4 color blue)
        (b4 size large)
        (b4 supported-by b5)
        (b5 shape cube)
        (b5 color green)
        (b5 size large)
        (b5 supports b4)
        (b6 shape brick)
        (b6 color purple)
        (b6 size large)))

; 7.29
(defun match-element (e1 e2)
  "Takes 2 elements and returns t if they're equal, or the second is a question mark."
  (or (eql e2 '?)
       (eql e1 e2)))

(defun match-triple (assertion pattern)
  "Takes an assertion and a pattern and returns t if the assertion matches the
  pattern."
  (every #'match-element assertion pattern))

(defun fetch (pattern)
  "Searches the database for facts matching pattern."
  (remove-if-not #'(lambda (option)
                     (match-triple option pattern))
                 *database*))

(defun ask-color (brick)
  "Generates a pattern to check color of a brick."
  (list brick 'color '?))

(defun supporters (block)
  "Return a list of blocks that support given block."
  (mapcar #'car
          (fetch (list '? 'supports block))))

(defun supp-cube (block)
  "Returns true if given block is supported by a cube"
    (find-if #'(lambda (supp)
                 (fetch (list supp 'shape 'cube)))
             (supporters block)))

(defun desc1 (block)
  "Returns a list of facts about block."
  (fetch (list block '? '?)))

(defun desc2 (block)
  "Returns all facts about block with block name removed."
  (mapcar #'cdr (desc1 block)))

(defun description (block)
  "Returns a list of attributes block has."
  (reduce #'append (desc2 block)))
