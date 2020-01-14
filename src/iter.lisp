
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
        ((env (current-env-tail 2)) ; Todo: should this be considered idomatic?
        (v (eval v env))
        (initial-val (if (flat-assoc-exists? keys 'initial-val)
                         (flat-assoc-ref keys 'initial-val)
                         default-initial)))
      
      (if (flat-assoc-exists? keys 'into)
      
        (lets var (flat-assoc-ref keys 'into)
          (env-define var (fun (if (env-exists? env var)
                                   (env-ref env var)
                                   initial-val) v keys env)))
        
        ; Todo: return value?
        (let* ((gv (hash-table-ref iter-context 'gathering-values))
               (val (hash-table-ref/default gv name initial-val)))
          (hash-table-set! gv name (fun val v keys env))))))
      
(def-l-fexpr-e iter (env . forms)
  (let-ht (empty-iter-context)
    (with/cc leave
      (with/cc finish-with
        
        (define finish (l-fun () (finish-with #f)))
      
        ; Drivers
        
        (def-l-fexpr next args
          (if (null? args)
            (begin
              (assert (number? repeat))
              (if (equal? repeat 1) (finish) (set! repeat (- repeat 1))))
            (hash-table-set! ht (car args)
              ((hash-table-ref/default next-funs (car args)
                (l-fun () (error "While calling (next ...); variable "
                                  (car args) " is not a valid driver")))))))
         
        (hash-table-set! ht 'next next)

        (hash-table-set! ht 'repeat
          (l-fun (count)
            (when first-iteration (set! repeat count))))
         
        (hash-table-set! ht 'for
          (l-fexpr-e (inner-env var in lst)
            (when first-iteration
              (lets gen (list-gen finish (eval lst inner-env))
                (env-define env var (gen))
                (set! auto-update (cons var auto-update))
                (hash-table-set! next-funs var gen)))))
         
        ; Gatherings
        (def-d-fun set-gathering (name init fun)
          (hash-table-set! ht name (curry gathering (car (current-env)) name init fun)))
        
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
        (hash-table-set! ht 'initially (l-fexpr-e (env . code)
          (when first-iteration
            (eval-all code env)))) ; Todo: this is not standards compliant?
         
        (hash-table-set! ht 'finally ; Todo: support multiple finally blocks
          (l-fexpr-e (env . code) (set! finally (l-fun () (eval-all code env)))))
        (hash-table-set! ht 'after-each
          (l-fexpr-e (env . code) (set! after-each (l-fun () (eval code env)))))
        
        ; Predicates
        (hash-table-set! ht 'first-iteration-p (l-fun () first-iteration))
        (hash-table-set! ht 'first-time-p
          (l-fun () (return-first first-time (set! first-time #f))))
         
        (do () (#f)
        
          (do ((f forms (cdr f)))
              ((null? f))
            (with/cc next-iteration
              (hash-table-set! ht 'next-iteration (l-fun () (next-iteration #f)))
              (eval (car f) (cons ht env))))
            
          (set! first-iteration #f)
          
          ; Update bindings created with for
          (do ((u auto-update (cdr u)))
              ((null? u))
            (force-eval-args next (car u)))
            
          ; Update repeat counter
          (if repeat (next))))
      
      (if finally (finally)))))
