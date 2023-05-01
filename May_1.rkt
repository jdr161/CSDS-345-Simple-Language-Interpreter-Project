#lang racket
;call cc
(define Mstate
  (lambda (condition body state return)
    (call/cc (lambda (break) (letrec ((loop (lambda (s) (if (M_boolean condition state)
                                                            (loop (Mstate body s return break))
                                                            s))
                                            (loop state))))))))
;CPS
(define Mstate
  (lambda (condition body state return next)
    (letrec (loop (lambda (s) (if (M_boolean condition s) ;lambda (s1) (next s1) break continuation (if see break then run the next line)
                                  (M_state body s return (lambda (s1) (next s1))
                                           (lambda (s1) (loop s1))) (next s)))))(loop state)))

