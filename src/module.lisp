; Module system
; Relative path is determined from module name
; by adding '.lisp' at the end. The name of the module
; can be followed by any of
; - The symbol 'all' which imports all exported identifiers,
;   this is the default.
; - A list of the form (except ...) which imports all exported
;   identifiers except the given identifiers.
; - A list of the form (only ...) which imports only the intersection
;   of the given identifiers and the exported identifiers.
; - The symbol 'private' which imports all identifiers including the
;   non-exported ones, and should only be used for testing purposes.
; An export declaration can be any of
; - The symbol 'all' which exports everything.
; - A list of the form (only ...)
; - A list of the form (except ...)

(def-l-fexpr require (name import-spec)
  (let ((path (string-append (symbol->string name) ".lisp"))
        (env (current-env-tail 1))
        (ht (make-hash-table))
        (export-spec #f))
    
    (def-d-fun copy-over (ht-source ht-dest idents)
      (do ((i idents (cdr i)))
          ((null? i))
        (hash-table-set! ht-dest (car i) (hash-table-ref ht-source (car i)))))
    
    (hash-table-set! ht 'export
      (l-fexpr (spec)
        (set! export-spec spec)))
        
    (eval `(load ,path) (cons ht env))
   
    (if (not export-spec)
      (print "Warning: nothing exported by library " name)

      (match import-spec
        ('all (match export-spec
              ('all (hash-table-merge!
                (car env) ht))
              ((list 'only idents ...)
                (copy-over ht (car env) idents))
              ((list 'except idents ...)
                (copy-over ht (car env)
                  (filter (d-fun (i) (not (member i idents)))
                          (hash-table-keys ht))))))
        ((list 'except idents ...)
          (match export-spec
            ('all (copy-over ht (car env)
                  (filter (d-fun (i) (not (member i idents)))
                          (hash-table-keys ht))))
            ((list 'only ex-idents ...)
              (copy-over ht (car env)
                (filter (d-fun (i) (not (member i idents))) ex-idents)))
            ((list 'except ex-idents ...)
              (copy-over ht (car env)
                (filter (d-fun (i) (not (or (member i idents) (member i ex-idents))))
                        (hash-table-keys ht))))))
        ((list 'only idents ...)
          (match export-spec
            ('all (copy-over ht (car env) (filter (d-fun (i) (member i idents)) idents)))
            ((list 'only ex-idents ...)
              (copy-over ht (car env)
                (filter (d-fun (i) (or (member i idents) (member i ex-idents)))
                        (hash-table-keys ht)))) ; Todo: throw error if identifiers is missing
            ((list 'except ex-idents ...)
              (copy-over ht (car env)
                (filter (d-fun (i) (and (not (member i ex-idents)) (member i idents)))
                        (hash-table-keys ht))))))))))
              
            
              



