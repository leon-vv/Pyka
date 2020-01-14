
(def-d-fun empty-iter-context ()
  (define auto-update '())
  (define next-funs (make-hash-table))
  (define finally #f)
  (define after-each #f)
  (define return-value #f)
  (define repeat #f)
  (define gathering-values (make-hash-table))
  (define first-iteration #t)
  (define first-time #t)
  ; Control flow Hash Table, used to store references 
  ; to 'for', 'collect', 'next', etc. and in which scope
  ; the iter statements are evaluated.
  (define ht (make-hash-table))
  
  (def-l-fexpr next args
    (if (null? args)
      (if (and repeat (equal? repeat 1))
          (finish)
          (set! repeat (- repeat 1)))
      (hash-table-set! ht (car args)
        ((hash-table-ref/default next-funs (car args)
          (l-fun () (error "While calling (next ...); variable "
                            (car args) " is not a valid driver")))))))

  (car (current-env)))

(def-d-fun list-gen (finish list)
  (l-fun ()
    (if (null? list)
      (finish)
      (return-first (car list) (set! list (cdr list))))))

(def-d-fun flat-assoc-exists? (lst key)
  (with/cc exit
    (do ((l lst (cdr (cdr l))))
        ((or (null? l) (null? (cdr l))) #f)
      (if (equal? (car l) key) (exit #t)))))

(def-d-fun flat-assoc-ref (lst key)
  (with/cc exit
    (do ((l lst (cdr (cdr l))))
        (#f)
      (if (equal? (car l) key) (exit (cadr l))))))

; fun takes current state, new value, key list, env
; and returns the new state
(def-d-fexpr gathering (iter-context name default-initial fun v . keys)
    (let* 
        ((env (current-env-tail 1))
        (v (eval v env))
        (initial-val (if (flat-assoc-exists? keys 'initial-val)
                         (flat-assoc-ref keys 'initial-val)
                         default-initial)))
      
      (if (flat-assoc-exists? keys 'into)
        (lets var (flat-assoc-ref keys 'into)
          (env-define var (fun (if (env-exists? env var)
                                   (eval var env)
                                   initial-val) v keys env))
        
        (let* ((gv (hash-table-ref ic 'gathering-values))
               (val (hash-table-ref/default gv name initial-val)))
          (hash-table-set! gv name (+ (eval v env) val)))))))
      
(def-l-fexpr-env iter (env . forms)
  (let (empty-iter-context)
    (with/cc leave
      (with/cc finish-with
        
        (define finish (l-fun () (finish-with #f)))
      
        ; Drivers
        (hash-table-set! ht 'next (curry next (car (current-env))))
        
        ; Todo: evaluated in correct env?
        (def-d-fun set-driver (name args-to-generator)
          (hash-table-set! ht name (l-fexpr args
            (when first-iteration
              (lets var-and-gen (args-to-generator args)
                (hash-table-set! next-funs
                  (car var-and-gen)
                  (curry (cadr var-and-gen) finish)))))))
             
        (set-driver 'for
          (l-fun (var in list)
            (cons
              var
              (make-list-gen (eval lst env)))))
            
        (def-d-fun set-gathering (name init fun)
          (hash-table-set! ht name (curry gathering ic name init fun)))
        
        ; Reductions
        (set-gathering 'sum 0 (d-fun (s v _ _) (+ s v)))
        (set-gathering 'multiply 1 (d-fun (s v _ _) (* s v)))
        (set-gathering 'counting 0 (d-fun (s v _ _) (if v (+ s 1) s)))
        (set-gathering 'maximize #f (d-fun (s v _ _) (if s (max s v) v)))
        (set-gathering 'minimize #f (d-fun (s v _ _) (if s (min s v) v)))
        (set-gathering 'reducing #f (d-fun (s v k env) ((eval (flat-assoc-ref k 'by) env) s v)))
        
        ; Accumulations
        (set-gathering 'collect '() (d-fun (s v k _)
            (lets place (if (flat-assoc-exists? k 'at) (flat-assoc-ref k 'at) 'end)
              (case place
                ((beginning start) (cons v s))
                ((end) (append a (list b)))))))
        (set-gathering 'appending '() (d-fun (s v k _)
            (lets place (if (flat-assoc-exists? k 'at) (flat-assoc-ref k 'at) 'end)
              (case place
                ((beggining start) (append s v))
                ((end) (append v s))))))
        
        ; Aggregated boolean tests
        (hash-table-set! ht 'always (l-fun (e) (if (not e) (leave #f)))) ; Todo: return value?
        (hash-table-set! ht 'never (l-fun (e) (if e (leave #f))))
        (hash-table-set! ht 'thereis (l-fun (e) (if e (leave e))))
        
        ; Control flow
        (hash-table-set! ht 'finish finish)
        (hash-table-set! ht 'leave (l-fun args (if (null? args) (leave #f) (leave (car args)))))
        (hash-table-set! ht 'while (l-fun (e) (if (not e) (finish))))
        (hash-table-set! ht 'until (l-fun (e) (if e (finish))))
        
        ; Code placement
        (hash-table-set! ht 'initially (l-fexpr-env (env . code)
          (when first-iteration
            (eval code env)))) ; Todo: not completely standards compliant?
         
        (hash-table-set! ht 'finally
          (l-fexpr-env (env . code) (set! finally (l-fun () (eval code env)))))
        (hash-table-set! ht 'after-each (l-fexpr code (set! after-each code)))
        
        ; Predicates
        (hash-table-set! ht 'first-iteration-p (l-fun () first-iteration))
        (hash-table-set! ht 'first-time-p
          (l-fun () (return-first first-time (set! first-time #f))))
         
        (do ((f forms (cdr forms)))
            ((null? f))
         
          (with/cc next-iteration
            (hash-table-set! ht 'next-iteration (l-fun () (next-iteration #f)))
            (eval (car f) (cons ht env)))
           
          (set! first-iteration #f)
          
          ; Update bindings created with for
          (do ((u auto-update (cdr u)))
              ((null? u))
            (force-eval-args next (car u)))
           
          ; Update repeat counter
          (next)))
      
      (if finally (finally)))))

