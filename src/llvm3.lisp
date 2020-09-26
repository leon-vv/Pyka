(load "llvm-context.lisp")

; We define a LLVM statement as a
; function which accepts a function
; and calls this function with a LLVM IR.

(def-d-fun type-list->string (t start end)
  (string-append
    start
    (string-join-map ", " type->string t)
    end))

(def-d-fun arg-list->string (a start end)
  (string-append
    start
    (string-join-map ", " 
      (l-fun (at)
        (string-append
          (type->string (car at)) " "
          (symbol->string (cadr at))))
      a)
    end) )

(arg-list->string '((i8 this) (i32 a) (i32 b)))
 
(def-d-fun type->string (t)
  (cond
    ((symbol? t)
      (symbol->string t))
    ((list? t)
      (case (car t)
        ((pointer) (string-append (type->string (cadr t)) "*"))
        ((->)
          (string-append
            (type->string (cadr t))
            (type-list->string (caddr t) "(" ")")))
        ((array)
          (string-append
            "[" (number->string (cadr t)) " x " (type->string (caddr t)) "]"))
        ((struct)
          (type-list->string (cdr t) "{" "}"))))))

(def-l-fun const. (val)
  (l-fun (ctx)
    (any->string val)))

(def-l-fun label. (var)
  (l-fun (ctx)
    (llvm-context-variable ctx var)))

(def-d-fun expr->string (ctx ex)
  (cond
    ((symbol? ex)
      ((label. ex) ctx))
    ((procedure? ex)
      (ex ctx))
    (else
      ((const. ex) ctx))))

(def-l-fun ret. (t ex)
  (l-fun (ctx)
    (string-append
      "ret " (type->string t) " " (expr->string ctx ex))))

(def-l-fun add. (t a b)
  (l-fun (ctx)
    (string-append
      "add " (type->string t) " "
             (expr->string ctx a) ", "
             (expr->string ctx b))))

(def-l-fun =. (t var ex)
  (l-fun (ctx)
    (ctx
    (string-append
      (llvm-context-fresh-variable ctx var) " = "
      (expr->string ctx ex))))
      

(def-l-fun declare. (ret name type-lst)
  (l-fun (ctx)
    (string-append
      "declare " (type->string ret) " "
        "@" (symbol->string name)
        (type-list->string type-lst "(" ")"))))

(def-l-fun-e fun. (env type name args . body)
  (def-d-fun (ctx)
    (string-append
      "declare "
        (type->string type) " "
        (symbol->string name)
     
    (let*
      ((inner-context (llvm-context-push ctx))
       (inner-IR (map (l-fun (stmt)
                        ((eval stmt env) inner-context))
                      body)))
      (string-append


(def-d-fun within-llvm-ctx. ctxs
  (lets ctx (make-llvm-context)
    (print (string-join-map "\n" (l-fun (stmt) (stmt ctx)) ctxs))))
  
(within-ctx.
  (declare. '(pointer i8) 'malloc '(i32))
  
  (struct. string
    (buffer (pointer i8))
    (length i32)
    (maxlen i32))

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

