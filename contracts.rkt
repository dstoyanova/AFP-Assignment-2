(provide
 (contract-out
  [add_vertex (->i ([digraph any/c]
                    [a (digraph) (and/c integer?
                                        (lambda (v) (not (has_vertex? digraph v))))])
                   any)]
  [add_edge (->i ([digraph any/c]
                  [a (digraph) (is_vertex? digraph)]
                  [b (digraph a) (and/c (is_vertex? digraph)
                                        (lambda (v) (not (has_edge? digraph a v))))]
                  [w integer?])
                 any)]
  [has_vertex? (->i ([digraph any/c]
                     [a (digraph) integer?])
                    [result boolean?])]
  [has_edge? (->i ([digraph any/c]
                   [a (digraph) (is_vertex? digraph)]
                   [b (digraph) (is_vertex? digraph)])
                  [result boolean?])]
  [out_neighbours (->i ([digraph any/c]
                        [a (digraph) (is_vertex? digraph)])
                       [result (digraph a) (and/c list?
                                                  (lambda (res)
                                                    (andmap (lambda (b) (has_edge? digraph a b)) res)))])]
  [weight (->i ([digraph any/c]
                [a (digraph) (is_vertex? digraph)]
                [b (digraph a) (is_edge? digraph a)])
               [result integer?])]))

(define (is_vertex? g) (lambda (v) (and (integer? v) (has_vertex? g v))))
(define (is_edge? g u) (lambda (v) (and (has_vertex? g u) (has_vertex? g v) (has_edge? g u v))))