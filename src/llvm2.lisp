
(def-d-fun instr (ins)
  (curry list ins))


(def-d-fun type->string (t)
  (cond
    ((symbol? t)
      (symbol->string t))
    ((and (list? t) (equal? (car t) 'pointer))
      (string-append (type->string (cadr t)) "*"))))
      
      
(def-d-fun args->string (args)
  (lets as (map (d-fun (a)
                    (string-append "i32 " (any->string a)))
                    args)
    (string-append "(" (string-join ", " as) ")")))

(def-d-fun statement->string (stmt)
  (if (list? stmt)
    (case (car stmt)
      ((=)
        (string-append
          (statement->string (cadr stmt))
          " = "
          (statement->string (caddr stmt))))
      
      ((fun)
        (string-append
          "define i32 "
          (any->string (cadr stmt))
          (args->string (caddr stmt)) " {\n"
          (program->string (cdddr stmt)) 
          "\n}"))

      ((ret)
        (string-append "ret i32 " (statement->string (cadr stmt))))
      
      ((call)
        (string-append "call i32 " (any->string (cadr stmt))
                                   (args->string (caddr stmt))))
      ((declare)
        (string-append "declare " (type->string (cadr stmt)) " "
          (symbol->string (caddr stmt))
          "(" (string-join ", " (map type->string (list-ref stmt 3))) ")"))
          
      (else
        (string-join " " (map any->string stmt))))
    (any->string stmt)))

(def-d-fun fun. (name args . body)
  (append (list 'fun name args) body))
 
(def-d-fun program->string (program)
  (string-join "\n"
    (map statement->string program)))

(def-d-fun print-program (program)
  (print (program->string program)))
 
(def-d-fun llvm-header. ()
  '(declare i32 @printf ((pointer i32) ...)))

(def-d-fun with-header. program
  (cons
    (llvm-header.)
    program))
    
(print-program
  (list
    (fun. '@main '()
      '(ret 10))))

























