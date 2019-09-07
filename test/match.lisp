
(assert-equal (parse-repeater '...) #t)
(assert-equal (parse-repeater '___) #t)
(assert-equal (parse-repeater '__55) 55)
(assert-equal (parse-repeater '__0) 0)
(assert-equal (parse-repeater '__abc) #f)
(assert-equal (parse-repeater 10) #f)

(assert-equal
  (let ((ht1 (make-hash-table))
       (ht2 (make-hash-table)))
    (hash-table-set! ht1 'abc '(1 2))
    (hash-table-set! ht2 'abc '("abc"))
    (hash-table-set! ht2 'def 10)
    
    (copy-env-into-ht ht1 (list (make-hash-table) ht2 (make-hash-table)))
    (list
      (hash-table-ref ht1 'abc)
      (hash-table-ref ht1 'def)))
  '((("abc") 1 2) (10)))

; These test cases are taken from 
; https://docs.racket-lang.org/reference/match.html

(assert-equal
  (match 1 
    (_ 5))
  5)

(assert-equal
  (match 10
    (10 1 2 3))
  3)

(assert-equal
  (match "abc"
    ("abc" 1))
  1)

(assert-equal
  (match '(+ 1 1)
    ('(+ 1 1) 1))
  1)

(assert-equal
  (match '(1 2 3)
    ((list a b a) (list a b))
    ((list a b c) (list c b a)))
  '(3 2 1))

(assert-equal
  (match '(1 (x y z) 1)
    ((list a b a) (list a b))
    ((list a b c) (list c b a)))
  '(1 (x y z)))

(assert-equal
  (match '(1 2 3)
    ((list _ _ a) a))
  3)

(assert-equal
  (match '(1 2 3)
    ((list 1 a ...) a))
  '(2 3))

(assert-equal
  (match '(1 2 3)
    ((list 1 a ..3) a)
    (_ 'else))
  'else)

(assert-equal
  (match '(1 2 3 4)
    ((list 1 a ..3) a)
    (_ 'else))
  '(2 3 4))

(assert-equal
  (match '(1 2 3 4 5)
    ((list 1 a ..3 5) a)
    (_ 'else))
  '(2 3 4))

(assert-equal
  (match '(1 (2) (2) (2) 5)
    ((list 1 (list a) ..3 5) a)
    (_ 'else))
  '(2 2 2))

(assert-equal
  (match "yes"
    ("no" #f)
    ("yes" #t))
  #t)

(assert-equal
  (match '(1 2 3)
    ((list a b c) (list c b a)))
  '(3 2 1))

(assert-equal
  (match '(1 2 . 3)
    ((list-rest a b c d) a))
  #f)

(assert-equal
  (match '(1 2 3 . 4)
    ((list-rest a b c d) d))
  4)


(assert-equal
  (match (cons 1 2)
    ((cons a b) (+ a b)))
  3)

(assert-equal
  (match '(1 (2 3) 4)
    ((list _ (and a (list _ _)) _) a))
  '(2 3))

(assert-equal
  (match '(1 2)
    ((and (list a b) (list b a)) 10)
    (else 20))
  20)

(assert-equal
  (match 5
    ((not 4) 'yes))
  'yes)

(assert-equal
  (match '(1 2)
   ((or (list a 1) (list a 2)) a))
  1)

(assert-equal
  (match '(for 5)
    (`(,(or 'for 'generating) ,x) x))
  5)

(assert-equal
  (match '(1 2 3)
   ((list (not 4) (not 4) (not 4)) 'yes)
   (_ 'no))
  'yes)

(assert-equal
  (match '(1 4 3)
    ((list (not 4) (not 4) (not 4)) 'yes)
    (_ 'no))
  'no)

(assert-equal
  (match '(1 2 3)
    (`,a a))
  '(1 2 3))

(assert-equal
  (match '5 (`,a a))
  5)

(assert-equal
  (match '(1 2 3)
    (`(1 ,a 3) a))
  2)

