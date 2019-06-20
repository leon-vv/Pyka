(load "src/base.lisp")

(def-macro loop cmnds
  `(do ()
       (#f)
       ,@cmnds))

(def-d-fun literal? (val)
  (or (boolean? val)
      (string? val)
      (number? val)))

; Returns true if infinite repeater (... or ___)
; Returns a number if k-repeater (..k or __k)
; Returns false if not a repeater
(def-d-fun parse-repeater (symbol)
  (lets str (symbol->string symbol)
    (case (substring str 0 2)
      ((".." "__")
        (case (substring str 2 3)
          (("." "_") #t)
          (else (string->number (substring str 2)))))
      (else #f))))

(assert-equal (parse-repeater '...) #t)
(assert-equal (parse-repeater '___) #t)
(assert-equal (parse-repeater '__55) 55)
(assert-equal (parse-repeater '__0) 0)
(assert-equal (parse-repeater '__abc) #f)

; Let's define a 'matcher' as a function which
; takes a value to match, a pattern and a hash table.
; A matcher should return #f if the pattern fails to match
; and - this is important - not mutate the passed hash table.
; If the passed value does match the pattern however, the matcher
; should return a hash table which possilby contains new matched
; values. It's also allowed in this case to update
; the passed hash table and simply return it.

(def-d-fun match-qp (val pat ht)
  (debug (cond
    ((literal? pat)
      (match-literal val pat ht))
    ((symbol? pat)
      (match-symbol val pat ht))
    ((and (list? pat) (not (null? pat)))
      (if (equal? (car pat) 'unquote)
        (match-pat val (car (cdr pat)) ht)
        (match-list val pat ht match-qp))))))

(def-d-fun match-list (val pats ht matcher)
  (if (and (null? val) (null? pats))
    ht
    (if (or (null? val) (null? pats))
      #f 
      (lets res (matcher (car val) (car pats) (hash-table-copy ht))
        (if (hash-table? res)
          (match-list (cdr val) (cdr pats) res matcher)
          #f)))))

(def-d-fun match-list-rest (val pats ht)
  (cond
    ((or (null? val) (null? pats)) #f)
    ((null? (cdr pats))
      (match-pat val (car pats) ht))
    (else
      (lets res (match-pat (car val) (car pats) ht)
        (if (hash-table? res)
          (match-list-rest (cdr val) (cdr pats) res)
          #f)))))

(def-d-fun match-cons (val pat ht)
  (lets res (match-pat (car val) (car pat) (hash-table-copy ht))
    (if (hash-table? res)
      (match-pat (cdr val) (car (cdr pat)) res)
      #f)))

(def-d-fun match-literal (val pat ht)
  (if (equal? pat val) ht #f))

(def-d-fun match-symbol (val pat ht)
  (if (hash-table-exists? ht pat)
    (if (equal? (hash-table-ref ht pat) val) ht #f)
    (begin
      (hash-table-set! ht pat val)
      ht)))

(def-d-fun match-pat (val pat ht)
  (cond
    ((equal? pat '_) ht)
    
    ((symbol? pat) (match-symbol val pat ht))
   
    ((literal? pat) (match-literal val pat ht))
    
    ((and (list? pat) (not (null? pat)))
      
      (lets f (car pat)
        (case f
          ((quote)
            (if (equal? (car (cdr pat)) val) ht #f))
          ((quasiquote)
            (match-qp val (cdr pat) ht))
          ((list)
            (if (list? val) (match-list val (cdr pat) ht match-pat) #f))
          ((list-rest)
            (if (pair? val) (match-list-rest val (cdr pat) ht) #f))
          ((cons)
            (if (pair? val) (match-cons val (cdr pat) ht) #f))
          ((and)
            (do
              ((p (cdr pat) (cdr p))
               (ht (hash-table-copy ht)))
              ((or (not (hash-table? ht))
                   (null? p))
               (if (and (hash-table? ht) (null? p)) ht #f))
              (set! ht (match-pat val (car p) ht))))
          ((or)
            (do
              ((p (cdr pat) (cdr p))
                (h #f))
              ((or (hash-table? h)
                   (null? p))
               (if (hash-table? h) h #f))
              (set! h (match-pat val (car p) ht))))
          ((not)
            (do
              ((p (cdr pat) (cdr p))
               (ht (hash-table-copy ht))
               (h #f))
              ((or (hash-table? h)
                   (null? p))
               (if (hash-table? h) #f ht))
              (set! h (match-pat val (car p) ht))))
          (else #f))))

    (else #f)))
      

(def-d-fexpr match (val . clauses)
  
    (def-d-fun match-env (val clauses env)
      (if (null? clauses)
        #f
        (lets res (match-pat val (car (car clauses)) (make-hash-table))
          (if (hash-table? res)
            (eval (car (cdr (car clauses))) (cons res env))
            (match-env val (cdr clauses) env)))))

    (match-env (eval-prev val) clauses (env-ref 1)))
      
; These test cases are taken from 
; https://docs.racket-lang.org/reference/match.html

(assert-equal
  (match 1 
    (_ 5))
  5)

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
  (match "yes"
    ("no" #f)
    ("yes" #t))
  #t)

(assert-equal
  (match '(1 2 3)
    ((list a b c) (list c b a)))
  '(3 2 1))

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
  (match '(1 2)
   ((or (list a 1) (list a 2)) a))
  1)

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


#|
(assert-equal
  (match '(1 2 3)
    (`,a a))
  '(1 2 3))

|#












