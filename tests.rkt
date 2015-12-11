#lang play
(require "start.rkt")
(print-only-errors)

;-----------------------------------------------------------
;BASIC TEST
;-----------------------------------------------------------

(test (run-val '(+ 1 2)) 3)
(test (run-val '(< 1 2)) #t)
(test (run-val '(- 2 1)) 1)
(test (run-val '(* 2 3)) 6)
(test (run-val '(= (+ 2 1) (- 4 1))) #t)
(test (run-val '(and #t #f)) #f)
(test (run-val '(or #t #f)) #t)
(test (run-val '(not (not #t))) #t)
(test (run-val '(if (not #f) (+ 2 1) 4)) 3)
(test (run-val '(local ([define x 5])
              (seqn {+ x 1}
                    x))) 5)


;-------------------------
;OTHER TESTS
;-------------------------

(test (run-val '(local
             [(define c (class
                            (field x 1)
                          (field y 2)
                          (method sum (z) (+ (get this x) (+ (get this y) z)))
                          (method set-x (val) (set this x val))))
              (define o (new c))]
             (seqn
              (send o set-x (+ 1 3))
              (+ (send o sum 3) (get o y)))))
      11)