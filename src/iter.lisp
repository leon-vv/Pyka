(load "src/match.lisp")

(def-d-fexpr hash-table-set-symbol! (ht sym)
  (hash-table-set! ht sym (eval-prev sym)))
  
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

(assert-equal
  (lets g (make-list-gen '(1 2 3))
    (g (d-fun () '())) (g (d-fun () '())))
  2)

(assert-equal
  (lets g (make-list-gen '())
    (with/cc abc (g abc) 10)
    5)
  5)

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
    
(assert-equal
  (lets g (numer-gen 1 3 1)
    (g #f) (g #f))
  2)

(assert-equal
  (lets g (numer-gen 1 -10 -3)
    (g #f) (g #f) (g #f) (g #f) (g (d-fun () #f)))
  #f)

#|
(def-d-fexpr iter clauses
 
  (if (null? clauses) (do () (#f))) ; Infinite loop
  
  (let*
    ((base-env (env-ref 2))
     (control-flow-ht (make-hash-table)) ; Will contain 'finish', 'leave', 'next', etc.
     
     (initially #f) ; Code placement
     (finally #f)
      
     (gen-ht (make-hash-table)) ; Generator hash table used by 'next' function
     (is-for (make-hash-table)) ; Which of the generators is produced by a for statement
      
     (repeat #f) ; Wheter there is a bound on the loop repetitions
     (code '()) ; User code (i.e. non driver code)
     
    (do ((c (car clauses) (cdr c)))
        ((null? c) #f)
      
     (match c
       (`(repeat ,times)
        (if (equal? repeat #f)
          (set! repeat times)
          (set! repeat (min times repeat))))
          
       (`(for ,var in ,list)
        (hash-table-set! gen-ht var (list-gen (eval list base-env))))
      
       ((list (or 'for 'generating') var seq-kw start)
        (hash-table-set! gen-ht var (numer-gen start (compute-step seq-kw start) #f))
        (if (equal? (car c) 'for) (hash-table-set! for-ht var #f)))
       
       ((list (or 'for 'generating) var seq-kw start 'by by)
        (hash-table-set! gen-ht var (numer-gen start (compute-step seq-kw start by) #f))
        (if (equal? (car c) 'for) (hash-table-set! for-ht var #f)))

       ((list (or 'for 'generating') var seq-kw1 start seq-kw2 end)
        (hash-table-set! gen-ht var
          (numer-gen start (compute-step seq-kw start)
                           (compute-end seq-kw2 end)))
        (if (equal? (car c) 'for) (hash-table-set! for-ht var #f)))
      
        ((list-rest 'initially clauses)
          (set! initially (append initially clauses)))
        ((list-rest 'finally clauses)
          (set! finally (append initally finally)))
        ((list-rest 'after-each clauses)
          (set! after-each (append after-each clauses)))
        
        (c (set! code (append code c)))))

    (with/cc leave
      (with/cc finish
        
          (hash-table-set-symbol! control-flow-ht leave)
          (hash-table-set-symbol! control-flow-ht finish)
          (hash-table-set! control-flow-ht
                          'next
                          (l-fun g 
                            (if (null? g) ; Handle repeat
                              (lets r repeat (set! repeat (- repeat 1)) r)
                              (hash-table-ref gen-ht g) finish)))
           
          (eval
            (begin
                ,initially
                (do ,(map (d-fun (k)
                              (if (hash-table-exists? for-ht k)
                                `(,k (next ,k) (next ,k))
                                `(,k (next ,k)))) (hash-table-keys gen-ht))
                  ,(if repeat '((next)) #f)
                  ,code)))
            (cons control-flow-ht base-env))))
      
      (eval final-code base-env))

|#

 
