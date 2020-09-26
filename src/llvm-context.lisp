
(def-d-fun make-llvm-context ()
  (list (make-hash-table)))

(def-d-fun llvm-context-push (ctx)
  (cons (make-hash-table) ctx))

(def-d-fun llvm-context-pop (ctx)
  (cdr ctx))

(def-d-fun llvm-context-variable (ctx var)
  (lets count (env-ref ctx var)
    (string-append
      "%" (symbol->string var) (any->string count))))

(def-d-fun llvm-context-define! (ctx var)
  (env-define! ctx var 0))

(def-d-fun llvm-context-fresh-variable (ctx var)
  (if (env-exists? ctx var)
    (lets count (env-ref ctx var)
      (env-set! ctx var (+ count 1))
      (llvm-context-variable ctx var)))
    (begin
      (llvm-context-define! ctx var)
      (llvm-context-variable ctx var)))

      



