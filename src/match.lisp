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

(def-d-fun match-list (val pats ht)
  (if (and (null? val) (null? pats))
    ht
    (if (or (null? val) (null? pats))
      #f 
      (lets res (match-pat (car val) (car pats) ht)
        (if (hash-table? res)
          (match-list (cdr val) (cdr pats) ht)
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
  (lets res (match-pat (car val) (car pat) ht)
    (if (hash-table? res)
      (match-pat (cdr val) (car (cdr pat)) ht)
      #f)))

(def-d-fun match-pat (val pat ht)
  (cond
    ((equal? pat '_) ht)
    
    ((symbol? pat)
    
      (if (hash-table-exists? ht pat)
        (if (equal? (hash-table-ref ht pat) val) ht #f)
        (begin
          (hash-table-set! ht pat val)
          ht)))
    
    ((literal? pat)
      (if (equal? pat val) ht #f))    

    ((and (list? pat) (not (null? pat)))
      
      (lets f (car pat)
        (case f
          ((quote)
            (if (equal? (car (cdr pat)) val) ht #f))
          ((list)
            (if (list? val) (match-list val (cdr pat) ht) #f))
          ((list-rest)
            (if (pair? val) (match-list-rest val (cdr pat) ht) #f))
          ((cons)
            (if (pair? val) (match-cons val (cdr pat) ht) #f))
          ((and)
            (do
              ((p (cdr pat) (cdr p)))
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














