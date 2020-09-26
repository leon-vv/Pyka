(load "llvm-context.lisp")

; We define a LLVM statement as a
; function which accepts a llvm-context
; and returns LLVM IR as a string.

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
            " ("
            (string-join-map ", " type->string (caddr t))
            ")"))))))


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
    (string-append
      (llvm-context-fresh-variable ctx var) " = "
      (expr->string ctx ex))))
        


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

