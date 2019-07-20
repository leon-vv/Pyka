(load "src/base.lisp")

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
; takes a value to match, a pattern and an environment.
; A matcher should return #f if the pattern fails to match
; and not mutate the passed environment.
; If the passed value does match the pattern however, the matcher
; should return an environment with new bindings.
; Note that this means that a matcher that matches successfully but
; does not create bindings returns the empty list (which in Scheme
; is not the same as #f).

; Unsuitable other definitions of a 'matcher':
; Just passing in a hash table and letting the matcher
; mutate it doesn't work with patterns of the fornm
; (list a ...)
; Just returning the new bindings without passing in anything
; doesn't work with
; (not a b c)

(def-d-fun match-qp (val pat env)
  (cond
    ((literal? pat)
      (match-literal val pat env))
    ((symbol? pat)
      (if (equal? val pat) '() #f))
    ((and (list? pat) (not (null? pat)))
      (if (equal? (car pat) 'unquote)
        (match-pat val (car (cdr pat)) env)
        (match-list val pat env match-qp)))))

(def-d-fun match-list (val pats env matcher)
  (if (and (null? val) (null? pats))
    '()
    (if (or (null? val) (null? pats))
      #f 
      (lets bind (matcher (car val) (car pats) env)
        (and bind
          (lets other-bind (match-list (cdr val) (cdr pats) (append bind env) matcher)
            (and other-bind (append bind other-bind))))))))

(def-d-fun match-list-rest (val pats env)
  (cond
    ((or (null? val) (null? pats)) #f)
    ((null? (cdr pats))
      (match-pat val (car pats) env))
    (else
      (lets bind (match-pat (car val) (car pats) env)
        (and bind
          (lets other-bind (match-list-rest (cdr val) (cdr pats) (append bind env))
            (and other-bind (append bind other-bind))))))))

(def-d-fun match-cons (val pat env)
  (lets res (match-pat (car val) (car pat) env)
    (and res
      (lets other (match-pat (cdr val) (car (cdr pat)) (append res env))
        (and other (append res other))))))

(def-d-fun match-literal (val pat env)
  (if (equal? pat val) '() #f))

(def-d-fun match-pat (val pat env)
  (cond
    ((equal? pat '_) '())
    
    ((symbol? pat)
      (if (env-exists? env pat)
        (if (equal? (env-ref env pat) val) '() #f)
        (lets h (make-hash-table)
          (hash-table-set! h pat val)
          (list h))))
     
    ((literal? pat)
      (match-literal val pat env))
     
    ((and (list? pat) (not (null? pat)))
      
      (lets f (car pat)
        (case f
          ((quote)
            (if (equal? (car (cdr pat)) val) '() #f))
          ((quasiquote)
            (match-qp val (car (cdr pat)) env))
          ((list)
            (if (list? val) (match-list val (cdr pat) env match-pat) #f))
          ((list-rest)
            (if (pair? val) (match-list-rest val (cdr pat) env) #f))
          ((cons)
            (if (pair? val) (match-cons val (cdr pat) env) #f))
          ((and)
            (do
                ((p (cdr pat) (cdr p))
                 (bind '()))
                ((or (null? p)
                     (not bind)) bind)
              (lets res (match-pat val (car p) env)
                (set! bind (and res (append res bind)))
                (set! env (and res (append res env))))))
          ((or)
            (do 
                ((p (cdr pat) (cdr p))
                 (bind #f))
                ((or (null? p)
                     bind) bind)
              (set! bind (match-pat val (car p) env))))
          ((not)
            (do
              ((p (cdr pat) (cdr p))
               (bind #f))
              ((or (null? p)
                   bind) (and (not bind) '()))
              (set! bind (match-pat val (car p) env))))
          (else (error "Unrecognized pattern")))))

    (else (error "Unrecognized pattern"))))
      

(def-d-fexpr match (expr . clauses)
  
    (let ((val (eval-prev expr))
          (env (get-env-tail 1)))

      (with/cc return
        (do ((clauses clauses (cdr clauses)))
            ((null? clauses) #f)
            
          (lets res (match-pat val (car (car clauses)) '())
            (if res
              (return (eval-all (cdr (car clauses)) (append res env)))))))))
 
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
  (match 5
    ((not 4) 'yes))
  'yes)


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











