
; Two deep
(define caar (d-fun (l) (car (car l))))
(define cadr (d-fun (l) (car (cdr l))))
(define cdar (d-fun (l) (cdr (car l))))
(define cddr (d-fun (l) (cdr (cdr l))))
; Three deep
(define caaar (d-fun (l) (car (car (car l)))))
(define caadr (d-fun (l) (car (car (cdr l)))))
(define cadar (d-fun (l) (car (cdr (car l)))))
(define caddr (d-fun (l) (car (cdr (cdr l)))))
(define cdaar (d-fun (l) (cdr (car (car l)))))
(define cdadr (d-fun (l) (cdr (car (cdr l)))))
(define cddar (d-fun (l) (cdr (cdr (car l)))))
(define cdddr (d-fun (l) (cdr (cdr (cdr l)))))

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
    (apply print msgs)
    (exit 1)))

(define call/cc call-with-current-continuation)
(define quote (d-fexpr (x) x))

(define env-copy
  (d-fun (env)
    (map hash-table-copy env)))

(define current-env-tail
  (d-fun (nth)
    (list-tail (current-env) (+ nth 1))))

(define last
  (d-fun (lst)
    (if (null? (cdr lst))
        (car lst)
        (last (cdr lst)))))

(define begin
  (d-fun stmts
    (if (null? stmts) #f (last stmts))))

(define eval-nth
	(d-fun (expr nth)
		(eval expr (current-env-tail (+ nth 1)))))

(define eval-prev
  (d-fun (expr)
    (eval-nth expr 2)))

(define pretty-string
  (d-fun (val indent)

    (define at-indent
      (d-fun (indent string)
        (string-append (make-string indent "  ") string)))
     
    (define pretty-list
      (d-fun (lst indent)
        (string-append
          "("
          (pretty-string (car lst) 0)
          (if (null? (cdr lst))
            ")"
            (string-append
              "\n" 
              (string-join
                "\n"
                (map 
                  (d-fun (v) (at-indent indent (pretty-string v (+ indent 1))))
                  (cdr lst)))
              ")")))))
     
    (define pretty-string-aux  
      (d-fun (val indent)
        (if (not (pair? val))
          (any->string val)
          (if (equal? (car val) 'unquote)
            (string-append "," (pretty-string (cadr val) 0))
            (if (equal? (car val) 'unquote-splicing)
              (string-append ",@" (pretty-string (cadr val) (+ indent 1)))
              (pretty-list val indent))))))

    (at-indent indent (pretty-string-aux val indent))))

(define assert
  (d-fexpr args
    (define expr
      (if (= (length args) 1)
          (car args)
          (list '= (car args) (cadr args))))
    (let ((res (eval-prev expr)))
      (if (not res)
        (error "======= ASSERTION FAILED ====== \n"
              (pretty-string expr 0))))))

(define debug
  (d-fexpr (v)
    (let ((res (eval-prev v)))
      (print "======= DEBUG ====== "
             "\nCalled with:\n " (pretty-string v 0)
             (if (not (equal? v res))
                (string-append "\nResult is:\n " (pretty-string res 0) "\n")
                "\n"))
      res)))

; These special definitions make sure that nested
; unquoting works. For example in `(a b `(,c ,,d))
; c is evaluated at the first evaluation, while d
; is evaluated at the second evaluation.
(define unquote
  (d-fexpr args
    (cons 'unquote args)))

(define unquote-splicing
  (d-fexpr args
    (cons 'unquote-splicing args)))

(define quasiquote
  (d-fexpr (val)
    
    (define quasiquote-cons-env
      (d-fun (c env)
        (if (or (null? c) (not (pair? c)))
          c
          (let ((first (car c))
                (rest (quasiquote-cons-env (cdr c) env)))
            (if (and (pair? first)
                     (not (null? first))
                     (equal? (car first) 'unquote-splicing))
                (append (eval (cadr first) env) rest)
                (cons (quasiquote-env first env) rest))))))
    
    (define quasiquote-env
      (d-fun (c env)
        (if (not (pair? c))
          c
          (if (equal? (car c) 'unquote)
              (eval (cadr c) env)
              (quasiquote-cons-env c env)))))
      
    (quasiquote-env val (current-env-tail 1))))

(define member
  (d-fun (obj list)
    (if (null? list)
      #f
      (if (equal? obj (car list))
        list
        (member obj (cdr list))))))

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
 
(define lambdafy
  (d-fun (env fun wrapper)
    (curry
      (wrapper (env fun . args)
        (eval `(apply ,fun ',args) env))
      env fun)))

(define l-fun
  (d-fexpr (args . code)
    (let ((env (current-env-tail 1)))
      (lambdafy
        env
        (eval (cons 'd-fun (cons args code))
              env)
        d-fun))))

(define l-fun-e
  (d-fexpr (args . code)
    (let ((env (current-env-tail 1)))
      (curry
        (lambdafy
          env
          (eval (cons 'd-fun (cons args code)) env)
          d-fun)
        env))))

(define l-fexpr
  (d-fexpr (args . body)
    (let ((env (current-env-tail 1)))
      (lambdafy
        env
        (eval (cons 'd-fexpr (cons args body)) env)
        d-fexpr))))

(define l-fexpr-e
  (d-fexpr (args . body)
    (let ((env (current-env-tail 1)))
      (curry
        (lambdafy
          env
          (eval (cons 'd-fexpr (cons args body)) env)
          d-fexpr)
        env))))

(define set!
  (d-fexpr (var val)
     
    (define set!-env
      (d-fun (var val env)
          (if (null? env)
              (error "set! cannot find " var " in environment ")
              
              (let ((ht (car env)))
                (if (hash-table-exists? ht var)
                    (begin (hash-table-set! ht var val) val)
                    (set!-env var val (cdr env)))))))
    
    (set!-env var (eval-prev val) (current-env-tail 1))))

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
    
    (cond-env clauses (current-env-tail 1))))

(define flatten
  (d-fun (x)
    (cond ((null? x) '())
          ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
          (else (list x)))))

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
    
    (case-env (eval-prev key) clauses (current-env-tail 1))))

(define macro
  (d-fexpr (args . code)
    (eval-prev
      `(d-fexpr ,args
        (eval-prev (begin ,@code))))))
 
(define fun-shorthand
  (macro (sym-name fun)
    `(define ,sym-name
      (macro (name args . code)
        `(define ,,name
          (,fun ,,args
          ,,@code))))))

(fun-shorthand def-d-fun d-fun)
(fun-shorthand def-d-fexpr d-fexpr)

(fun-shorthand def-l-fun l-fun)
(fun-shorthand def-l-fexpr l-fexpr)

(fun-shorthand def-l-fun-e l-fun-e)
(fun-shorthand def-l-fexpr-e l-fexpr-e)

(fun-shorthand def-macro macro)

(def-macro with/cc (name . code)
  `(call/cc (d-fun (,name) ,@code)))

 ; Let single
(def-macro lets (name val . cmnds)
    `(let ((,name ,val)) ,@cmnds))

(def-macro when (test . code)
  `(if ,test
    (begin ,@code)))

(def-d-fun return-first (a b) a)

(def-macro unless (test . code)
  `(if (not ,test)
    (begin ,@code)))

(def-macro prepend-to! (variable value)
  `(set! ,variable (cons ,value ,variable)))

(def-d-fun index (val lst)
  (let ((t (member val lst)))
    (assert t)
    (- (length lst) (length t))))

; Todo: do we want to support predicates?
; Currently records are represented as vectors
(def-macro define-record-type (name constructor pred . fields)
  (list* 'begin
    `(define ,(car constructor) ; Constructor function
      (d-fun vals 
        (assert (length vals) ,(length fields))
        (list->vector vals)))
    (map (d-fun (field)
      (let ((name (car field))
            (acc (cadr field))
            (mod (cddr field)))
          `(begin
            (define ,acc ; Accessor functions
              (d-fun (r)
                (vector-ref r ,(index name (cdr constructor)))))
            ,(if (not (null? mod)) ; Modifier functions
              `(define ,(car mod)
                (d-fun (r v)
                  (vector-set! r ,(index name (cdr constructor)) v)))))))
          fields)))
          
      




















