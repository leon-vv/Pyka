
(def-d-fun instr (ins)
  (curry list ins))


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
        (string-append "ret " (statement->string (cadr stmt))))
      ((call)
        (string-append "call i32 " (any->string (cadr stmt))
                                   (args->string (caddr stmt))))
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
 
(print-program
  (list
    (fun. 'times2 '(a)
      '(ret (mult a 2)))
    '(call times2 (10 20))
    '(= x (call times2 (50)))))
