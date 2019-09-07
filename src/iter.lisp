(def-d-fexpr hash-table-set-symbol! (ht sym)
  (hash-table-set! (eval-prev ht) sym (eval-prev sym)))
  
; A 'generator' is a function that returns the next
; value of a sequence at each invocation. When there
; are no more values in the sequence to return, the
; generator invokes the 'finish' function, which is
; passed in as an argument.
(def-d-fun make-list-gen (list)
  (l-fun (finish)
    (if (null? list)
      (finish)
      (lets val (car list)
        (set! list (cdr list))
        val))))

   
(def-d-fun compute-step (keyword start . by)
  (lets
      direction
      (case seq-kw
        ((from upfrom) 1)
        ((downfrom) -1))
    (if (null? by)
      direction
      (* direction (car by)))))

(def-d-fun compute-end (keyword end)
  (case keyword
    ((to) (+ end 1)) ; 'to' and 'downto' are inclusive
    ((downto) (- end 1))
    ((below above) end)))

(def-d-fun numer-gen (start end step)
  (l-fun (finish)
    (if (and end (or
            (and (< step 0) (<= start end))
            (and (> step 0) (>= start end))))
      (finish)
      (lets s start (set! start (+ start step)) s))))
    
(def-d-fun create-gathering (fun val control-ht base-env already-called set-return)
  (l-fexpr args
    (if (even (length args)) (error "Wrong number of arguments to gathering clause"))
    
    (let* ((args-ht (make-hash-table))
           (expr-result (eval (car args) (cons control-ht base-env))))
      
      (do ((a (cdr args) (cdr (cdr a)))) ; Turn args into hash table
          ((null? a))
        (hash-table-set! args-ht (car a) (eval (car (cdr a)) (cons control-ht base-env))))
      
      (if (and (not already-called) (hash-table-exists? args-ht 'initial-value))
        (set! val (hash-table-ref args-ht 'initial-value)))
      (set! already-called #t)
      
      (if (hash-table-exists? args-ht 'into) ; 'into' references value in control hash table
        (let* ((var (hash-table-ref args-ht 'into))
               (next-val (if (hash-table-exists? control-ht var)
                              (fun (hash-table-ref control-ht var) expr-result args-ht)
                              (fun val expr-result args-ht))))
               (hash-table-set! control-ht var next-val)
               next-val) ; make sure to return 'next-val'
        (lets ret (set! val (fun val expr-result args-ht))
          (set-return ret)
          ret)))))
            
        
(def-d-fun add-gatherings (control-ht base-env set-return)
  (def-l-fun add (symbol fun init)
    (hash-table-set! control-ht symbol (create-gathering fun init control-ht base-env #f set-return)))
  
  ; 'a' is current value, 'b' is the value passed as argument
  ; 'k' is a hash table with Keyword arguments ('by', 'at', 'initial-value' etc)
  (add 'sum (d-fun (a b k) (+ a b)) 0)
  (add 'multiply (d-fun (a b k) (* a b)) 1)
  (add 'counting (d-fun (a b k) (if b (+ a 1) a)) 0)
  (add 'maximixe (d-fun (a b k) (if a (max a b) b)) #f)
  (add 'minimize (d-fun (a b k) (if val (min a b) b)) #f)
  (add 'reducing (d-fun (a b k) ((hash-table-ref 'by k) a b)) #f)
  (add 'collect (d-fun (a b k)
    (lets place (hash-table-ref/default k 'at 'end)
      (case place
        ((beginning start) (cons b a))
        ((end) (append a (list b))))))
    '())
  (add 'appending (d-fun (a b k)
    (lets place (hash-table-ref/default k 'at 'end)
      (case place
        ((beginning start) (append b a))
        ((end) (append a b)))))
    '()))

(def-d-fun set-is-for  (clause is-for var)
  (if (equal? (car (car clause)) 'for)
    (hash-table-set! is-for var #t)))

; Use of dynamic environment. Bindings of caller site
; leak to this function.
(def-d-fun match-clause ()
  (match (car clauses)
      (`(repeat ,times)
        (if (equal? repeat #f)
          (set! repeat times)
          (set! repeat (min times repeat))))
        
      (`(,(or 'for 'generating) ,var in ,list)
        (hash-table-set! gen-ht var (make-list-gen (eval list base-env)))
        (set-is-for clauses is-for var))
      
      ((list (or 'for 'generating) var seq-kw start)
        (hash-table-set! gen-ht var (numer-gen start (compute-step seq-kw start) #f))
        (set-is-for clauses is-for var))
      
      ((list (or 'for 'generating) var seq-kw start 'by by)
        (hash-table-set! gen-ht var (numer-gen start (compute-step seq-kw start by) #f))
        (set-is-for clauses is-for var))

      ((list (or 'for 'generating) var seq-kw1 start seq-kw2 end)
        (hash-table-set! gen-ht var
          (numer-gen start (compute-step seq-kw start)
                            (compute-end seq-kw2 end)))
        (set-is-for clauses is-for var))
    
      ((list-rest 'initially clauses)
        (set! initially (append initially clauses)))
      ((list-rest 'finally clauses)
        (set! finally (append finally clauses)))
      
      (c (set! code (append code (list c))))))

(def-d-fexpr iter clauses
 
  (if (null? clauses) (do () (#f))) ; Infinite loop
    
  (let
    ((base-env (get-env-tail 1))

     ; Will contain 'finish', 'leave', 'next', etc.
     ; But also any variable names used in reductions and accumulations
     (control-flow-ht (make-hash-table))      
    
     (initially '()) ; Code placement
     (finally '())
     (ret-val #f) ; Return value of iter
     
     (gen-ht (make-hash-table)) ; Generator hash table used by 'next' function
     (is-for (make-hash-table)) ; Which of the generators is produced by a for statement
      
     (repeat #f) ; Wheter there is a bound on the loop repetitions
     (code '())) ; User code (i.e. non driver code)
     
    (def-l-fun set-return (val) (set! ret-val val)) 

    (do ((clauses clauses (cdr clauses)))
        ((null? clauses) #f)
      (match-clause)) 
     
    (with/cc leave
      (with/cc finish-with
      
        (let* ((finish (l-fun () (finish-with #f)))
               (set-return (l-fun (val) (set! ret-val val)))
               (next (l-fun (sym) 
                (hash-table-set! control-flow-ht sym ((hash-table-ref gen-ht sym) finish)))))
          (hash-table-set-symbol! control-flow-ht leave)
          (hash-table-set-symbol! control-flow-ht finish)
          (hash-table-set-symbol! control-flow-ht next)
          (add-gatherings control-flow-ht base-env set-return)
          
          (map next (hash-table-keys gen-ht)) ; Initialise variables
          (eval-all initially (cons control-flow-ht base-env))
          
          (do () (#f)
          
            (if (and repeat (< (set! repeat (- repeat 1)) 0)) (finish))
            (map next (hash-table-keys is-for))

            (eval-all code (cons control-flow-ht base-env)))))

      (if (null? finally) ; Return implicit ret-val used by gatherings if there's no 'finally' block
        ret-val
        (eval-all finally (cons control-flow-ht base-env))))))

(assert-equal
  (lets g (numer-gen 1 3 1)
    (g #f) (g #f))
  2)

(assert-equal
  (lets g (numer-gen 1 -10 -3)
    (g #f) (g #f) (g #f) (g #f) (g (d-fun () #f)))
  #f)

(assert-equal
  (lets g (make-list-gen '(1 2 3))
    (g (d-fun () '())) (g (d-fun () '())))
  2)

(assert-equal
  (let ((x 5) (g (make-list-gen '())))
    (g (l-fun () (set! x 15)))
    x)
  15)
 
(assert-equal
  (iter (for x in '(1 2 3)) (if (equal? x 2) (leave x)))
  2)

(assert-equal
  (iter (generating x in '(1 2 3))
    (leave (next 'x)))
  2)

(assert-equal
  (lets x 3
    (iter (repeat 3) (set! x (+ x 1)))
    x)
  6)

(assert-equal
  (lets x 5
    (iter (for y in '(1 2 3 4 5))
      (initially (set! x 10))
      (if (equal? y 3) (finish))
      (finally x)))
  10)

