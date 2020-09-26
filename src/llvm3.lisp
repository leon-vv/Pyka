(load "llvm-context.lisp")

(def-d-fun type->string (t)
  (cond
    ((symbol? t)
      (symbol->string t))
    ((list? t)
      (case (car t)
        ((pointer) (string-append (type->string (cadr t)) "*"))
        ((->)
          (string-append
            (type->string (cadr t)) " "
            (type-list->string (cddr t) "(" ")")))
        ((array)
          (string-append
            "[" (number->string (cadr t)) " x " (type->string (caddr t)) "]"))
        ((struct)
          (type-list->string (cdr t) "{" "}"))))))

(def-l-fun type-list->string (t start end)
  (string-append
    start
    (string-join-map ", " type->string t)
    end))

(def-l-fun arg-list->string (a start end)
  (string-append
    start
    (string-join-map ", " 
      (l-fun (at)
        (string-append
          (type->string (car at)) " "
          (symbol->string (cadr at))))
      a)
    end))


(def-l-fun const. (val)
  (any->string val))

(def-l-fun label. (var)
  (any->string var))

#|(def-l-fun expr->string (ctx ex)
  (cond
    ((symbol? ex)
      ((label. ex) ctx))
    ((procedure? ex)
      (ex ctx))
    (else
      ((const. ex) ctx))))|#

(def-l-fun expr->string (ex)
  (if (string? ex)
    ex
    (any->string ex)))

(def-l-fun ret. (t ex)
  (string-join " " "ret" (type->string t) ex) (expr->string ex))

(def-l-fun add. (t a b)
  (string-append "add " (type->string t) " " (expr->string a) ", " (expr->string b)))

(def-l-fun =. (var ex)
  (string-append (symbol->string var) " = " (expr->string ex)))
    
(def-l-fun declare. (ret name type-lst)
  (string-append
    "declare " (type->string ret) " " (symbol->string name)
      (type-list->string type-lst "(" ")")))

(def-l-fun fun. (ret name arg-list . body)
  (string-append
    (type->string ret) " " (symbol->string name)
    (arg-list->string arg-list "(" ")") " {\n"
    (string-join "\n" body) "\n}"))

"""
(def-l-fun-e fun. (env type name args . body)
  (def-d-fun (ctx)
    (string-append
      declare 
        (type->string type) " "
        (symbol->string name)
     
    (let*
      ((inner-context (llvm-context-push ctx))
       (inner-IR (map (l-fun (stmt)
                        ((eval stmt env) inner-context))
                      body)))
      (string-append
"""


(def-d-fun within-ctx. ctxs
  (lets ctx (make-llvm-context)
    (print (string-join-map "\n" (l-fun (stmt) (stmt ctx)) ctxs))))
  
(define ctx (make-llvm-context))
(llvm-context-define! ctx 'a)
(llvm-context-define! ctx 'b)


#|

(def-l-fexpr-e fun. (env type name args . body)
  (def-d-fun (ctx)
    (let*
      ((inner-context (llvm-context-push ctx))
       (inner-IR (map (l-fun (stmt)
                        ((eval stmt env) inner-context))
                      body)))
      (string-append

(fun. 'i32 'plus ((i32 a) (i32 b))
  (=. i32 'res (add. i32 a b))
  (ret. i32 res))
|#

