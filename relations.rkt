#lang racket

(provide
 (except-out (all-from-out racket) #%app #%top #%datum)
 (rename-out [app #%app]
             [top #%top]
             [datum #%datum])
 show make-relation relation project)

(define-syntax-rule (app f arg ...)
  (begin (cond [(procedure? f) (#%app f arg ...)]
               [else (cons 'f (quote (arg ...)))])))

(define-syntax-rule (top . arg) 'arg)

(define-syntax-rule (datum . arg)
  (begin (#%datum . arg)))

; Representation of the relation 
(struct table (cols col-hash data))

(define global-env (make-hash))

(define (displ lst dipslay-func)
  (match lst
    ['() (void)]
    [(list row rest ...)
     (dipslay-func row)
     (displ rest dipslay-func)]))

(define (show-table rel)
  (displ (table-data rel) (lambda(el) (displayln el))))

; Print the given relation's content on the screen
(define (show rel)
  (cond [(table? rel) (show-table rel)]
        [else (show (hash-ref global-env rel))]))

(define (init-col-hash hash cols)
  (let init-next-col  ([cols cols] [iter 0])
    (match cols
      ['() hash]
      [(list first rest ...)
       (hash-set! hash first iter)
       (init-next-col rest (+ 1 iter))])))

(define (conv-data data)
  (reverse
   (let conv-row ([data data] [ndata '()])
     (match data
       ['() ndata]
       [(list first rest ...) (conv-row rest (cons (list->vector first) ndata))]))))

; Creates a new relation
(define (make-relation colnames data)
  (table colnames
         (init-col-hash (make-hash) colnames)
         (conv-data data)))

; Binds the identifier 'relname' to the relation given by 'reldef'
(define (relation relname reldef)
  (hash-set! global-env relname reldef))

(define (cols-numbers col-hash cols)
  (let check-next-col ([cols cols] [number-lst '()])
    (match cols
      ['() number-lst]
      [(list first rest ...)
       (check-next-col rest (cons (hash-ref col-hash first) number-lst))])))

(define (shrink-row row number-lst)
  (reverse (match number-lst
             ['() '()]
             [(list first rest ...) (cons (vector-ref row first) (shrink-row row rest))])))

(define (project-by-numbers rows number-lst)
  (let shrink-next-row ([rows rows] [out-rows '()])
    (match rows
      ['() out-rows]
      [(list first rest ...) (shrink-next-row rest (cons (shrink-row first number-lst) out-rows))])))

(define (project-table rel colnames)
  (table (table-cols rel)
         (init-col-hash (make-hash) colnames)
         (project-by-numbers (table-data rel) (cols-numbers (table-col-hash rel) colnames))))

; Returns the projection of the relation 'rel' on the given 'colnames'
(define (project rel colnames)
  (cond [(table? rel) (project-table rel colnames)]
        [else (project-table (hash-ref global-env rel) colnames)])) 