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

(defparameter *nerd-states*
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

(defparameter *note-table*
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

(defparameter my-hand
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

(defparameter colors
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

(defparameter *all-ranks*
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

(defparameter *database*
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

                                        ; 7.30
(defparameter *words*
      '((one un)
        (two deux)
        (three trois)
        (four quatre)
        (five cinq)))

(mapcar #'(lambda (w s)
            (append w (list s)))
        *words*
        '(uno dos tres quatro cinco))

                                        ; 8.4
(defun laugh (n)
  "Return a list containing given number of \"HA\"s."
  (cond ((= n 0) '())
        (t (cons 'ha (laugh (1- n))))))

(defun add-up (l)
  "Sum a list of numbers."
  (cond ((null l) 0)
        (t (+ (car l) (add-up (cdr l))))))

(defun alloddp (l)
  "Returns a t if the provided list is all odd numbers."
  (cond ((null l) t)
        ((evenp (car l)) nil)
        (t (alloddp (cdr l)))))

(defun rec-member (el lst)
  "Returns a t if the provided element is a member of the provided list."
  (cond ((null lst) nil)
        ((eql el (car lst)) t)
        (t (rec-member el (cdr lst)))))

(defun rec-assoc (el lst)
  "Returns the pair whose car matches the provided element."
  (cond ((null lst) nil)
        ((eql el (car (car lst))) (car lst))
        (t (rec-assoc el (cdr lst)))))

(defun rec-nth (n lst)
  "Return the nth element of list."
  (cond ((null lst) nil)
        ((zerop n) (car lst))
        (t (rec-nth (1- n) (cdr lst)))))

(defun rec-plus (x y)
  "Adds 2 numbers, recursively."
  (cond ((< y 0) nil)
        ((zerop y) x)
        (t (rec-plus (1+ x) (1- y)))))

(defun fib (n)
  "Compute a Fibonacci number"
  (cond ((or (= n 0) (= n 1)) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))

                                        ; 8.14
(defun inf ()
  "Smallest infinite recursive."
  (inf))

(defun anyoddp (l)
  "Checks if any item in a list is odd."
  (cond ((null l) nil)
        ((oddp (car l)) t)
        (t (anyoddp (cdr l)))))

                                        ; 8.17
(defun find-first-odd (l)
  "Returns the first odd number in a list."
  (cond ((null l) nil)
        ((oddp (car l)) (car l))
        (t (find-first-odd (cdr l)))))

(defun last-element (l)
  "Return the last element of a list."
  (cond ((null l) nil)
        ((null (cdr l)) (car l))
        (t (last-element (cdr l)))))

(defun add-nums (n)
  "Add N, N-1, N-2, ... to 0."
  (cond ((zerop n) 0)
        (t (+ n (add-nums (1- n))))))

(defun all-equal (l)
  "Return t if all the items in a list are equal."
  (cond ((null (cdr l)) t)
        ((eql (car l) (cadr l)) (all-equal (cdr l)))
        (t nil)))

(defun count-down (n)
  "Creates a list counting down from n."
  (cond ((zerop n) nil)
        (t (cons n (count-down (1- n))))))

(defun applicative-fact (n)
  "An applicative version of the factorial function."
  (reduce #'* (count-down n)))

(defun count-down-b (n)
  "count-down, but it ends in 0 instead"
  (cond ((zerop n) '(0))
        (t (cons n (count-down (1- n))))))

(defun count-down-c (n)
  "count-down, but it ends in 0 instead"
  (cond ((= -1 n) nil)
        (t (cons n (count-down (1- n))))))

(defun square-list (l)
  "Takes a list of numbers. Returns a list of their squares."
  (cond ((null l) l)
        (t (let ((n (car l)))
             (cons (* n n) (square-list (cdr l)))))))

                                        ; 8.28
(defun my-nth (n x)
  "Return the nth item of a list, recursively."
  (cond ((null x) nil)
        ((zerop n) (car x))
        (t (my-nth (1- n) (cdr x)))))

(defun my-member (el lst)
  "Return a matching item from a list, recursively."
  (cond ((null lst) nil)
        ((eql el (car lst)) (car lst))
        (t (my-member el (cdr lst)))))

(defun my-assoc (el lst)
  "Return a matching pair from an a-list, recursively."
  (cond ((null lst) nil)
        ((eql el (car (car lst))) (car lst))
        (t (my-assoc el (cdr lst)))))

(defun compare-lengths (l1 l2)
  "Takes 2 lists. Returns \"same-length\", \"first-is-longer\", or
  \"second-is-longer\", depending on their relationship."
  (cond ((and (null l1) (null l2)) 'same-length)
        ((null l2) 'first-is-longer)
        ((null l1) 'second-is-longer)
        (t (compare-lengths (cdr l1) (cdr l2)))))

                                        ; 8.32
(defun sum-numeric-elements (l)
  "Sum only the numeric elements of a list."
  (cond ((null l) 0)
        ((numberp (car l))
         (+ (car l) (sum-numeric-elements (cdr l))))
        (t (sum-numeric-elements (cdr l)))))

(defun my-remove (el lst)
  "Recursive REMOVE."
  (cond ((null lst) nil)
        ((eql el (car lst)) (my-remove el (cdr lst)))
        (t (cons (car lst) (my-remove el (cdr lst))))))

(defun my-intersection (l1 l2)
  "Recursive INTERSECTION."
  (cond ((or (null l1) (null l2)) nil)
        ((member (car l1) l2)
         (cons (car l1) (my-intersection (cdr l1) l2)))
        (t (my-intersection (cdr l1) l2))))

(defun my-set-difference (l1 l2)
  "Recursive SET-DIFFERENCE."
  (let ((cand (car l1)))
    (cond ((and (null l1) (null l2)) nil)
          ((null l1) l2)
          ((null l2) l1)
          ((member cand l2)
           (my-set-difference (remove cand l1) (remove cand l2)))
          (t (cons cand (my-set-difference (cdr l1) l2))))))

(defun count-odd-condaug (l)
  "Counts the number of odd items in a list using conditional augmentation."
  (cond ((null l) 0)
        ((oddp (car l)) (1+ (count-odd-condaug (cdr l))))
        (t (count-odd-condaug (cdr l)))))

(defun count-odd-reg (l)
  "Counts the number of odd items in a list using regular augmentation."
  (cond ((null l) 0)
        (t (+ (if (oddp (car l))
                  1
                  0)
              (count-odd-reg (cdr l))))))

(defun count-atoms (l)
  "Counts the number of atoms in a tree."
  (cond ((atom l) 1)
        (t (+ (count-atoms (car l))
              (count-atoms (cdr l))))))

(defun count-cons (l)
  "Count the number of cons cells in a tree."
  (cond ((atom l) 0)
        (t (+ 1
              (count-cons (car l))
              (count-cons (cdr l))))))

(defun sum-tree (l)
  "Sums all numbers in a tree. Non-numbers ignored."
  (cond ((numberp l) l)
        ((atom l) 0)
        (t (+ (sum-tree (car l))
              (sum-tree (cdr l))))))

(defun my-subst (new old l)
  "Recursive SUBST."
  (cond ((eql l old) new)
        ((atom l) l)
        (t (cons (my-subst new old (car l))
                 (my-subst new old (cdr l))))))

                                        ; 8.43 -- Why doesn't this work? It even matches the example...
(defun flatten (l)
  "Recursively flatten a tree into a list."
  (cond ((atom l) (list l))
        (t (append (flatten (car l))
                   (flatten (cdr l))))))

(defun tree-depth (l)
  "Returns maximum depth of a binary tree."
  (cond ((atom l) 0)
        (t (+ 1 (max (tree-depth (car l))
                     (tree-depth (cdr l)))))))

(defun paren-depth (l)
  "Returns the maximum depth of nested parens."
  (cond ((atom l) 0)
        (t (+ (max (1+ (paren-depth (car l)))
                   (paren-depth (cdr l)))))))

(defun count-up (n)
  "Count up without helper function."
  (cond ((zerop n) nil)
        (t (append (count-up (1- n)) (list n)))))

(defun make-loaf (n)
  "Makes a loaf of size n."
  (if (zerop n)
      nil
      (cons 'x (make-loaf (1- n)))))

(defun bury (el n)
  "Buries an item in a number of parens."
  (cond ((zerop n) el)
        (t (list (bury el (1- n))))))

(defun pairings (l1 l2)
  "Zips 2 lists."
  (cond ((null l1) nil)
        (t (cons (list (car l1) (car l2))
                 (pairings (cdr l1) (cdr l2))))))

(defun sublists (l)
  "Returns all sublists of l."
  (cond ((null l) nil)
        (t (cons l (sublists (cdr l))))))

(defun my-reverse (l)
  "Recursive REVERSE."
  (labels ((helper (l acc)
             (cond ((null l) acc)
                   (t (helper (cdr l) (cons (car l) acc))))))
    (helper l nil)))

(defun my-union (l1 l2)
  "Recursive UNION"
  (cond ((null l1) l2)
        ((null l2) l1)
        (t (cons (car l1)
                 (my-union (cdr l1) (remove (car l1) l2))))))

(defun largest-even (l)
  "Returns the largest even number in a list of nonnegative integers."
  (labels ((helper (l max)
             (let ((cand (car l)))
               (cond ((null l) max)
                     ((and (> cand max)
                           (evenp cand))
                      (helper (cdr l) cand))
                     (t (helper (cdr l) max))))))
    (helper l 0)))

(defun huge (n)
  "Raises a number to its own power."
  (labels ((helper (n power)
             (cond ((= power 1) n)
                   (t (* n (helper n (1- power)))))))
    (helper n n)))

(defun every-other (l)
  "Returns every other element of a list."
  (labels ((helper (l pick)
             (cond ((null l) nil)
                   (pick (cons (car l) (helper (cdr l) nil)))
                   (t (helper (cdr l) t)))))
    (helper l t)))

(defun left-half (l)
  "Returns the left half of a given list."
  (labels ((helper (l take)
             (cond ((null l) nil)
                   ((<= take 0) nil)
                   (t (cons (car l) (helper (cdr l) (1- take)))))))
    (helper l (/ (length l) 2))))

(defun merge-lists (l1 l2)
  "Takes 2 sorted lists. Merges them in order."
  (cond ((null l1) l2)
        ((null l2) l1)
        ((< (car l1) (car l2))
         (cons (car l1) (merge-lists (cdr l1) l2)))
        (t (cons (car l2) (merge-lists l1 (cdr l2))))))

(defparameter *family*
      '((colin nil nil)
        (deirdre nil nil)
        (arthur nil nil)
        (kate nil nil)
        (frank nil nil)
        (linda nil nil)
        (suzanne colin deirdre)
        (bruce arthur kate)
        (charles arthur kate)
        (david arthur kate)
        (ellen arthur kate)
        (george frank linda)
        (hillary frank linda)
        (andre nil nil)
        (tamara bruce suzanne)
        (vincent bruce suzanne)
        (wanda nil nil)
        (ivan george ellen)
        (julie george ellen)
        (marie george ellen)
        (nigel andre hillary)
        (frederick nil tamara)
        (zelda vincent wanda)
        (joshua ivan wanda)
        (quentin nil nil)
        (robert quentin julie)
        (olivia nigel marie)
        (peter nigel marie)
        (erica nil nil)
        (yvette robert zelda)
        (diane peter erica)))

(defun father (person)
  "Gets a person's father."
  (cadr (assoc person *family*)))

(defun mother (person)
  "Gets a person's mother."
  (caddr (assoc person *family*)))

(defun parents (person)
  "Gets a person's parents."
  (union (and (father person) (list (father person)))
         (and (mother person) (list (mother person)))))

(defun children (person)
  "Gets a person's children."
  (mapcar #'car
          (remove-if-not #'(lambda (family)
                             (or (eql person (cadr family))
                                 (eql person (caddr family))))
                         *family*)))

; 8.60b
(defun siblings (person)
  "Gets a person's siblings"
  (set-difference
   (union (children (father person))
          (children (mother person)))
   (list person)))

(defun mapunion (fn ls)
  "Maps a function over a list of lists, then unions them all."
  (reduce #'union (mapcar fn ls)))

(defun grandparents (person)
  "Gets a person's grandparents"
  (mapunion #'parents (parents person)))

(defun cousins (person)
  "Gets a person's first cousins."
  (mapunion #'children
            (mapunion #'siblings
                      (parents person))))

(defun descended-from (descendent ancestor)
  "Determines if the first person is descended from the second."
  (cond ((null descendent) nil)
        ((member ancestor (parents descendent)) t)
        (t (or (descended-from (father descendent)
                               ancestor)
               (descended-from (mother descendent)
                               ancestor)))))

(defun ancestors (person)
  "Return a person's set of ancestors."
  (cond ((null person) nil)
        (t (union (parents person)
                  (union (ancestors (father person))
                         (ancestors (mother person)))))))

(defun generation-gap (p1 p2)
  "Returns the number of generations between 2 people."
  (labels ((helper (person acc)
             (cond ((null person) nil)
                   ((eql p2 person) acc)
                   (t (or (helper (father person) (1+ acc))
                          (helper (mother person) (1+ acc)))))))
    (helper p1 0)))

; 8.61
(defun count-up-tail (n)
  "Tail-recursive count-up"
  (labels ((helper (n acc)
             (cond ((<= n 0) nil)
                   ((eql (car (last acc)) n) acc)
                   (t (helper n (append acc (list (1+ (car (last acc))))))))))
    (helper n '(1))))

(defun fact-tail (n)
  "Tail-recursive factorial"
  (labels ((helper (n acc)
             (cond ((zerop n) acc)
                   (t (helper (1- n) (* acc n))))))
    (helper n 1)))

(defun union-tail (l1 l2)
  "Tail-recursive union"
  (labels ((helper (l1 l2 acc)
             (let ((cand (car l1)))
               (cond ((null l1) (append l2 acc))
                     ((null l2) (append l1 acc))
                     ((member cand l2)
                      (helper (cdr l1)
                              (remove-if #'(lambda (i)
                                             (eql cand i))
                                         l2)
                              (cons cand acc)))
                     (t (helper (cdr l1) l2 (cons cand acc)))))))
    (helper l1 l2 '())))

(defun intersection-tail (l1 l2)
  "Tail-recursive intersection"
  (labels ((helper (l1 l2 acc)
             (let ((cand (car l1)))
               (cond ((or (null l1) (null l2)) acc)
                     ((member cand l2)
                      (helper (cdr l1) (remove-if #'(lambda (i)
                                                      (eql cand i))
                                                  l2)
                              (cons cand acc)))
                     (t (helper (cdr l1) l2 acc))))))
    (helper l1 l2 '())))

(defun set-difference-tail (l1 l2)
  "Tail-recursive set-difference"
  (labels ((helper (l1 l2 acc)
             (let ((cand (car l1)))
               (flet ((remove-helper (i) (eql i cand)))
                 (cond ((null l1) (append l2 acc))
                       ((null l2) (append l1 acc))
                       ((member cand l2)
                        (helper (remove-if #'remove-helper l1)
                                (remove-if #'remove-helper l2)
                                acc))
                       (t (helper (remove-if #'remove-helper l1)
                                  l2
                                  (cons cand acc))))))))
    (helper l1 l2 '())))

; 8.64
(defun tree-find-if (fn tree)
  "Return the first leaf of a tree that fulfills a predicate."
  (cond ((and tree
              (atom tree)
              (funcall fn tree))
         tree)
        ((atom tree) nil)
        (t (or (tree-find-if fn (car tree))
               (tree-find-if fn (cdr tree))))))

(defun tr-count-slices (l)
  "Tail recursive counting slices"
  (labels ((helper (l acc)
             (if l (helper (cdr l) (1+ acc)) acc)))
    (helper l 0)))

(defun tr-reverse (ls)
  "Tail recursive list reverse"
  (labels ((helper (l acc)
             (if l (helper (cdr l) (cons (car l) acc)) acc)))
    (helper ls '())))

(defun arith-eval (exp)
  "Evaluates an infix arithmetic expression."
  (if (numberp exp)
      exp
      (funcall (cadr exp)
               (arith-eval (car exp))
               (arith-eval (caddr exp)))))

(defun legalp (exp)
  "Determines if a given list is a proper arith expression."
  (cond ((numberp exp) t)
        ((atom exp) nil)
        (t (and (= (length exp) 3)
                (legalp (car exp))
                (member (cadr exp) '(+ - * /))
                (legalp (caddr exp))))))

(defun factor-tree (n)
  "Produce a factorization tree for a number."
  (labels ((helper (n p)
             (cond ((= n 1) nil)
                   ((= n p) (list n p))
                   ((zerop (rem n p))
                    (list n p (helper (/ n p) p)))
                   (t (helper n (+ p 1))))))
    (helper n 2)))
