
(define print
  (d-fun msgs

    (define print-one
      (d-fun (v)
        (if (string? v)
            (write-string v)
            (write v))))
    
    (map print-one msgs)
    (newline)))

(define error
  (d-fun msgs
    (apply print-string msgs)
    (exit 1)))

(define quote (d-fexpr (x) x))

(define append
  (d-fun lsts
      (if (null? lsts)
        '()
        (if (null? (cdr lsts)) ; Optimisation
          (car lsts)
          (if (null? (car lsts))
              (apply append (cdr lsts))
              (cons (car (car lsts))
                    (apply append
                      (cons (cdr (car lsts))
                            (cdr lsts)))))))))

(define list-skip-n
  (d-fun (lst k)
    (if (or (null? lst) (equal? k 0))
      lst
      (list-skip-n (cdr lst) (- k 1)))))

(define env-ref
  (d-fun nth
    (list-skip-n (get-env)
      (if (null? nth) 1 (+ (car nth) 1)))))

   
(define last
  (d-fun (lst)
    (if (null? (cdr lst))
        (car lst)
        (last (cdr lst)))))

(define begin
  (d-fun stmts
    (last stmts)))

(define eval-nth
	(d-fun (expr nth)
		(eval expr (env-ref (+ nth 1)))))

(define eval-prev
  (d-fun (expr)
    (eval-nth expr 2)))


(define pretty-string
  (d-fun (val indent)

    (define at-indent
      (d-fun (indent string)
        (string-append (make-string indent "\t") string)))
     
    (define pretty-list
      (d-fun (lst indent)
        (string-append
          "("
          (pretty-string (car lst) 0)  "\n"
          (string-join
            "\n"
            (map 
              (l-fun (v) (at-indent indent (pretty-string v (+ indent 1))))
              (cdr lst)))
          ")")))
     
    (define pretty-string-aux  
      (d-fun (val indent)
        (if (not (pair? val))
          (any->string val)
          (if (equal? (car val) 'unquote)
            (string-append "," (pretty-string (car (cdr val)) (+ indent 1)))
            (if (equal? (car val) 'unquote-splicing)
              (string-append ",@" (pretty-string (car (cdr val)) (+ indent 1)))
              (pretty-list val indent))))))

    (at-indent indent (pretty-string-aux val indent))))

(define debug
  (d-fexpr (v)
    (let ((res (eval-prev v)))
      (print "======= DEBUG ====== "
             "\nCalled with:\t " v
             "\nResult is:\t " res "\n")
      res)))

(define quasiquote
  (d-fexpr (val)
    
    (define quasiquote-cons-env
      (d-fun (c env)
        (if (null? c)
          '()
          (let ((first (car c))
                (rest (quasiquote-cons-env (cdr c) env)))
            (if (and (pair? first)
                     (not (null? first))
                     (equal? (car first) 'unquote-splicing))
                (append (eval (car (cdr first)) env) rest)
                (cons (quasiquote-env first env) rest))))))
    
    (define quasiquote-env
      (d-fun (c env)
        (if (not (pair? c))
          c
          (if (equal? (car c) 'unquote)
              (eval (car (cdr c)) env)
              (quasiquote-cons-env c env)))))
      
    (quasiquote-env val (env-ref 1))))
 
(define assert-equal
  (d-fexpr (code res)
    (let ((code-ev (eval-prev code))
          (res-ev (eval-prev res)))
      (if (equal? code-ev res-ev)
          '()
          (begin
            (print "Failed test: " code " not equal to " res
                   "\nFirst argument evaluated to: " code-ev
                   "\nSecond argument evaluated to: " res-ev " \n"))))))
 
(assert-equal (+ 1 2) 3)
(assert-equal (last '(1 2 3)) 3)

(assert-equal (last '(1)) 1)
(assert-equal (begin 1 2 3) 3)

(assert-equal (list-skip-n '(1 2 3) 0) '(1 2 3))
(assert-equal (list-skip-n '() 5) '())
(assert-equal (list-skip-n '(1 2 3) 1) '(2 3))

(assert-equal (quote (1 2 3)) '(1 2 3))
(assert-equal `(1 2 ,@(list 3 4)) (list 1 2 3 4))
(assert-equal `(1 2 ,(+ 1 2)) (list 1 2 3))
(assert-equal 
  (let ((name 'x))
    (quasiquote (let (((unquote name) 10)) (unquote name))))
    '(let ((x 10)) x))
(assert-equal
  (let ((c 1) (env 2) (val 3))
    `(,c ,env ,val))
  '(1 2 3))

(define l-fun-nth
  (d-fun (args body nth)
    (cons
      (eval-prev `(d-fun ,args ,@body))
      (env-ref (+ nth 1)))))

(define l-fun
  (d-fexpr args
    (l-fun-nth (car args) (cdr args) 1)))

(define set!
  (d-fexpr (var val)
     
    (define set!-env
      (d-fun (var val env)
          (if (null? env)
              (error "set! cannot find " var " in environment ")
              
              (let ((ht (car env)))
                (if (hash-table-exists? ht var)
                    (hash-table-set! ht var val)
                    (set!-env var val (cdr env)))))))
    
    (set!-env var (eval-prev val) (env-ref 1))))

(define counter
  (d-fun (n)
    (l-fun ()
      (set! n (+ n 1))
      (- n 1))))

(assert-equal ((counter 5)) 5)
(assert-equal (let ((c (counter 0)))
                (c)
                (c)
                (c)) 2)

(define eval-all
  (d-fun (es env)
    (eval `(begin ,@es) env)))

(define cond
  (d-fexpr clauses

    (define cond-env
      (d-fun (clauses env)
        (if (null? clauses)
          #f
          (let* ((c (car clauses))
                (res (if (equal? (car c) 'else)
                          #t
                          (eval (car c) env))))
            (if res
                (if (null? (cdr c))
                    res
                    (eval-all (cdr c) env))
                (cond-env (cdr clauses) env))))))
    
    (cond-env clauses (env-ref 1))))

(assert-equal
  (cond (#t 10)) 10)
(assert-equal
  (cond (#f) (else (+ 1 2))) 3)

(define case
  (d-fexpr (key . clauses)
    
    (define any-equal
      (d-fun (val lst)
        (if (null? lst)
          #f
          (if (equal? val (car lst))
            #t
            (any-equal val (cdr lst))))))
     
    (define case-env
      (d-fun (key clauses env)
        (if (null? clauses)
          #f
          (let* ((c (car clauses))
                (res (if (equal? (car c) 'else)
                        #t
                        (any-equal key (car c)))))
            (if res
              (eval-all (cdr c) env)
              (case-env key (cdr clauses) env))))))
    
    (case-env (eval-prev key) clauses (env-ref 1))))

(assert-equal
  (case 10
    ((10) 5)) 5)
(assert-equal
  (case 'abc
    ((def abc) 15)) 15)
(assert-equal
  (case 'abc
    ((abc) 5)
    (else 30)) 5)
 
(define macro
  (d-fexpr (args . code)
    (eval-prev
      `(d-fexpr ,args
          (eval-prev (begin ,@code))))))
 
; Let single
(define lets
  (macro (name val . cmnds)
    `(let ((,name ,val)) ,@cmnds)))

 
 
 
 
 
 
  
