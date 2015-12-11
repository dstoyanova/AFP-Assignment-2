#lang racket

(define (union-mine l1 l2)
  (remove-duplicates (append l1 l2)))

(define (intersect-mine l1 l2)
  (remove-duplicates
   (cond
     [(null? l1) '()]
     [(null? l2) '()]
     [(member (car l1) l2)
      (cons (car l1) (intersect-mine (remove (car l1) l1) l2))]
     [else (intersect-mine (remove (car l1) l1) l2)])))

(define nth
  (lambda (n l)
    (cond
      [(eq? l null) '()]
      [(= n 0) (car l)]
      [else (nth (- n 1) (cdr l))])))

(define (nextroll rolls p)
  (if (= (length rolls) (+ p 1)) 0 (+ p 1)))

(define (neighbours-helper edges node results cp)
  (cond
    [(= (length edges) cp) results]
    [(= (car (list-ref edges cp)) node)
     (neighbours-helper edges node (cons (list-ref (list-ref edges cp) 1) results) (+ cp 1))]
    [else (neighbours-helper edges node results (+ cp 1))]))

(define (neighbours edges nodes)
  (let ([acc null])
    (for ([x nodes])
      (set! acc (union-mine (neighbours-helper edges x '() 0) acc))) acc))

(define (dice-helper-2 edges nodes stp)
  (cond
    [(= stp 0) '()]
    [(= stp 1) (neighbours edges nodes)]
    [else (dice-helper-2 edges (neighbours edges nodes) (- stp 1))]))

(define (dice-helper-1 N edges rolls nds m cr)
  (let ([nodes (dice-helper-2 edges nds (list-ref rolls cr))])
    (cond
      [(= m 100) -1]
      [(eq? null (intersect-mine (list N) nodes))
       (dice-helper-1 N edges rolls nodes (+ m 1) (nextroll rolls cr))]
      [else (+ m 1)])))

(define (dice N edges rolls)
  (dice-helper-1 N edges rolls '(1) 0 0)) 