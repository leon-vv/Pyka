
(def-d-fun literal? (val)
  (or (boolean? val)
      (string? val)
      (number? val)))

; Returns true if infinite repeater (... or ___)
; Returns a number if k-repeater (..k or __k)
; Returns false if not a repeater
(def-d-fun parse-repeater (val)
  (if (symbol? val)
    (lets str (symbol->string val)
      (case (substring str 0 2)
        ((".." "__")
          (case (substring str 2 3)
            (("." "_") #t)
            (else (string->number (substring str 2)))))
        (else #f)))
    #f))

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
        (match-list val pat env #f match-qp)))))

; Copy the bindings in source-env 
; over to the LISTS in ht
(def-d-fun copy-env-into-ht (ht source-env)
  (do ((keys (env-keys source-env) (cdr keys)))
       ((null? keys) ht)
    (lets val (env-ref source-env (car keys)) ; car keys lol
      (if (hash-table-exists? ht (car keys))
        (hash-table-set! ht (car keys) (cons val (hash-table-ref ht (car keys))))
        (hash-table-set! ht (car keys) (list val))))))

(def-d-fun reverse-ht (ht)
  (do ((keys (hash-table-keys ht) (cdr keys)))
      ((null? keys) ht)
    (hash-table-set! ht (car keys) (reverse (hash-table-ref ht (car keys))))))

(def-d-fun match-lvp (val-list pat env repeater matcher)
  (with/cc return
    (lets bind (make-hash-table)
      (do ((i 0 (+ i 1))
          (val-list val-list (cdr val-list))) (#f)

        (if (and (number? repeater) (equal? i repeater))
          (return (list val-list (reverse-ht bind))))
        (if (or (not (pair? val-list)) (null? val-list))
          (return (and (not (number? repeater))
                       (list val-list (reverse-ht bind)))))

        (lets newbind (matcher (car val-list) pat env)
          (if newbind
            (copy-env-into-ht bind newbind)
            (return #f)))))))

(def-d-fun match-list (val pats env support-lvp matcher)
  (cond
    ((and (null? val) (null? pats)) '())
    ((or (null? val) (null? pats)) #f)
    ((and support-lvp
          (not (null? (cdr pats)))
          (define r (parse-repeater (car (cdr pats)))))
      (lets result (match-lvp val (car pats) env r matcher)
        (and result (lets other-bind (match-list (car result)
                                               (cdr (cdr pats))
                                               (append (cdr result) env)
                                               support-lvp
                                               matcher)
                        (append (cdr result) other-bind)))))
    (else
      (lets bind (matcher (car val) (car pats) env)
        (and bind
          (lets other-bind (match-list (cdr val) 
                                       (cdr pats)
                                       (append bind env)
                                       support-lvp
                                       matcher)
            (and other-bind (append bind other-bind))))))))

(def-d-fun match-list-rest (val pats env)
  (cond
    ((not (or (pair? val) (null? (cdr pats)))) #f) ; more pats than val remaining
    ((null? pats) #f) ; more val than pats remaining
    ((null? (cdr pats)) ; last pat
      (match-pat val (car pats) env))
    ((define r (parse-repeater (car (cdr pats))))
       (lets result (match-lvp val (car pats) env r match-qp)
        (and result
             (match-list-rest (car result) (cdr (cdr pats)) (append (cdr result) env)))))
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
            (if (list? val) (match-list val (cdr pat) env #t match-pat) #f))
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



