#lang racket
(require "simpleParser.rkt")


; addBinding takes a name, value, and a state
; adds that key-value pair to the state
; replaces any existing binding if one already exists with the same name
; returns the state
(define addBinding
  (lambda (name val state)
    (cond
      [(eq? val 'true)              (addBinding name #t state)]
      [(eq? val 'false)             (addBinding name #f state)]
      [(null? state)                (list(list name val))]
      [(eq? (car (car state)) name) (cons (list name val) (cdr state))]
      (else (cons (car state)       (addBinding name val (cdr state)))))))

; if the state doesn't contain the name of the key-value pair, we give error
; but if (eq?(car(car state) name) cdr state thus removed that pairing
; else (cons (car state) removebinding(cdr state)
;(removeBinding 'x '((y 5) (x 7) (k 19) (b 10)))
;WW
(define removeBinding
  (lambda (name state)
    (cond
      [(null? state) state]
      [(eq?          (car state) name) (cdr state)]
      (else          (cons (car state) (removeBinding name (cdr state)))))))

; findBinding takes a name and a state
; finds the binding with the correct name
; if not binding with the inputted name can be found
; throws an error using (error msg)
; returns the value of the binding
(define findBinding
  (lambda (name state)
    (cond
      [(eq? name 'true)             #t]
      [(eq? name 'false)            #f]
      [(null? state)                (error name "variable used before declaration")]
      [(eq? (car (car state)) name) (if (null? (car (cdr (car state))))
                                        (error name "cannot use variable before it is assigned a value")
                                        (car (cdr (car state))))]
      (else                         (findBinding name (cdr state))))))

; helper function declared? finds if a given var name is in the state or not
 (define declared?
  (lambda (name state)
    (cond
      [(null? state)               #f]
      [(eq? (car(car state)) name) #t]
      [else (declared? name        (cdr state))])))

; 
(define operator
  (lambda (expression)
    (car expression)))

(define leftoperand cadr)
;cadr is (car (cdr expr))

(define rightoperand caddr)
;caddr is (car (cdr (cdr expr)))

; fourthoperand takes 
(define fourthoperand
  (lambda (expr)
    (car (cdr (cdr (cdr expr))))))

; newState returns a blank state, ready to pass into M_state
(define newState
  (lambda ()
    (list (list 'return null))))

; findReturnVal finds the return value to return after everything else is done
(define findReturnVal
  (lambda (state)
    (cond
      [(eq? (findBinding 'return state) #t) 'true]
      [(eq? (findBinding 'return state) #f) 'false]
      (else (findBinding 'return state)))))

; returnDefined? takes a state
; returns #t if return has been assigned a value
; #f otherwise
(define returnDefined?
  (lambda (state)
    (cond
      [(eq? (car (car state)) 'return) (if (null? (car (cdr (car state))))
                                           #f
                                           #t)]
      (else                            (returnDefined? (cdr state))))))
 

; M_value takes an expression and a state
; evaluates the expression
; expressions can be a number, a variable, or an operator with two subexpressions
; returns the value of the expression
(define M_value
  (lambda (expr state)
    (cond
      [(not (list? expr)) (if (number? expr) ; if the expression is just a single number or variable
                              expr
                              (findBinding expr state))]
      [(eq? (operator expr) '+) (+ (M_value (leftoperand expr) state) (M_value (rightoperand expr) state))]
      [(eq? (operator expr) '-) (if (null? (cdr (cdr expr))) ; if the '- is a unary operator
                               (- (M_value (leftoperand expr) state))
                               (- (M_value (leftoperand expr) state) (M_value (rightoperand expr) state)))] 
      [(eq? (operator expr) '*) (* (M_value (leftoperand expr) state) (M_value (rightoperand expr) state))]
      [(eq? (operator expr) '/) (quotient (M_value (leftoperand expr) state) (M_value (rightoperand expr) state))]
      [(eq? (operator expr) '%) (remainder (M_value (leftoperand expr) state) (M_value (rightoperand expr) state))]
      (else (M_boolean expr state)))))

; M_boolean takes a conditional and a state
; if trying to compare an int and a boolean, throws an error
; evaluates the conditional (including dealing with comparison operators)
; returns true or false
(define M_boolean
  (lambda (conditional state)
    (cond
      [(not (list? conditional))   (findBinding conditional state)] ; the case that conditional is a variable or 'true or 'false
      [(and (eq? (operator conditional) '==) (and (number? (M_value (leftoperand conditional) state)) (number? (M_value (rightoperand conditional)  state))))
                 (eq? (M_value (leftoperand conditional) state) (M_value (rightoperand conditional) state))]
      [(and (eq? (operator conditional) '!=) (and (number? (M_value (leftoperand conditional) state)) (number? (M_value (rightoperand conditional)  state))))
                 (not (eq? (M_value (leftoperand conditional) state) (M_value (rightoperand conditional)  state)))]
      [(and (eq? (operator conditional) '<) (and (number? (M_value (leftoperand conditional) state)) (number? (M_value (rightoperand conditional)  state))))
                 (< (M_value (leftoperand conditional) state) (M_value (rightoperand conditional)  state))]
      [(and (eq? (operator conditional) '<=) (and (number? (M_value (leftoperand conditional) state)) (number? (M_value (rightoperand conditional)  state))))
                 (<= (M_value (leftoperand conditional) state) (M_value (rightoperand conditional)  state))]
      [(and (eq? (operator conditional) '>) (and (number? (M_value (leftoperand conditional) state)) (number? (M_value (rightoperand conditional)  state))))
                 (> (M_value (leftoperand conditional) state) (M_value (rightoperand conditional)  state))]
      [(and (eq? (operator conditional) '>=) (and (number? (M_value (leftoperand conditional) state)) (number? (M_value (rightoperand conditional)  state))))
                 (>= (M_value (leftoperand conditional) state) (M_value (rightoperand conditional)  state))]
      [(eq? (operator conditional) '&&) (and (M_boolean (leftoperand conditional) state) (M_boolean (rightoperand conditional)  state))]
      [(eq? (operator conditional) '||) (or (M_boolean (leftoperand conditional) state) (M_boolean (rightoperand conditional)  state))]
      [(eq? (operator conditional) '!)  (not (M_boolean (leftoperand conditional) state))]
      (else                        (error "cannot compare int and bool")))))


; M_declaration takes a declaration statement (in the form (var variable) or (var variable value)) and a state
; checks that the variable name is available and adds the new variable to the state
; if the variable does not have a value assigned in the passed in statement, sets the value to null
; returns the new state
(define M_declaration
  (lambda (statement state)
    (cond
      [(declared? (leftoperand statement) state) (error (leftoperand statement) "variable name is already taken")]
      [(null? (cdr (cdr statement)))           (addBinding (leftoperand statement) null state)]
      (else                                    (addBinding (leftoperand statement) (M_value (rightoperand statement) state) state)))))

; M_assignment takes an assignment statement (in the form (= variable expression)) and a state
; WW
; assigns the value of the expression to the variable in the state
; returns the new state
; expr meaning (= variable expression)
; state is a list of bindings currently
;(M_assignment '(= x 10) '((y 5) (x 5) (z 19) (k 27)))
(define M_assignment
  (lambda (expr state)
    (cond
      [(and (eq? (operator expr) '=) (declared? (leftoperand expr) state)) (addBinding (leftoperand expr) (M_value(rightoperand expr) state) state)]
      (else (error (leftoperand expr) "variable not declared")))))

; M_return takes a return statement (in the form (return expression)) and a state
; If return is already defined, returns the state
; otherwise, it evaluates the expression and 
; returns the value of the expression
(define M_return
  (lambda (statement state)
    (if (returnDefined? state)
        state
        (addBinding 'return (M_value (leftoperand statement) state) state))))

; M_if takes an if statement (in the form (if conditional then-statement optional-else-statement)) and a state
; evaluates the conditional and calls M_state on the correct statement as necessary
; returns the new state
; WW
(define M_if
  (lambda (statements state)
    (cond
      [(null? statements) state] ; i assume if passed in it is not gonna be null though
      [(M_boolean (leftoperand statements) state) (M_state (cons (rightoperand statements) '()) state)]
      [(eq? (cdr (cdr (cdr statements))) '())   state] ; check if the else statemet is null then just return state as it is
      [else                                     (M_state (cons (fourthoperand statements) '()) state)]))) ; return the false statement state


; M_while takes a while statement (in the form 	(while conditional body-statement)) and a state
; recurses if the conditional returns true
; returns the state if the conditional returns false
; Maria
(define M_while
  (lambda (cstatements state)
    (cond
      [(M_boolean (leftoperand cstatements) state) (M_while cstatements (M_state (cons (rightoperand cstatements) '()) state))] ;M_state updates the estate to after the operation and
      (else                                      state)))) ;if returned false then just return the state
    ;also we cons the "() to the end to every M_state call because if we pass in (= x 10) into M _state, it is not a sublist like ((= x 10))
    ;so then the (car (car tree)) won't work because that is intended for the ((var x 10) (= x 12)) syntax tree 

; M_state takes a syntax tree and a state
; checks what kind of statement the first statement in the syntax tree is and calls the correct function on it
; recurses on itself with the the cdr of the syntax tree and the state returned from the the function called on the car of the syntax tree
; if the state is just a single value, returns that value
(define M_state
  (lambda (tree state)
    (cond
      [(null? tree)                   state]
      [(eq? 'var (car (car tree)))    (M_state (cdr tree) (M_declaration (car tree) state))]
      [(eq? '= (car (car tree)))      (M_state (cdr tree) (M_assignment (car tree) state))]
      [(eq? 'return (car (car tree))) (M_return (car tree) state)]
      [(eq? 'if (car (car tree)))     (M_state (cdr tree) (M_if (car tree) state))]
      [(eq? 'while (car (car tree)))  (M_state (cdr tree) (M_while (car tree) state))]
      (else                           (error (car (car tree)) "unrecognized statement type")))))
  

; interpret takes a filename
; calls the parser to get the syntax tree
; calls M_state on that syntax tree
; returns the return value from that syntax tree
(define interpret
  (lambda (filename)
    (findReturnVal (M_state(parser filename) (newState))))) ; () shows returns true for (null? '())

;(parser "t16.txt")



(interpret "t15.txt")
(interpret "t16.txt")
(interpret "t17.txt")
(interpret "t18.txt")
(interpret "t19.txt")
(interpret "t20.txt")
(interpret "t21.txt")
(interpret "t22.txt")
(interpret "t23.txt")

;t1 runs and returns 150
;t2 runs and returns -4 (used (round x ) to make sure we get integers
;t3 runs and returns 10
;t4 runs and returns 16
;t5 runs and returns 220
;t6 runs and returns 5
;t7 runs and returns 6
;t8 runs and returns 10
;t9 runs are returns 5 
;t10 runs and returns -39
;t11 gives correct error
;t12 gives correct error
;t13 gives correct error
;t14 gives correct error
;t15 runs and correctly returns true
;t16 runs and returns 100
;t17 runs and correctly returns false
;t18 runs and correctly returns true
;t19 runs and correctly returns 128
;t20 runs and correctly returns 12
;t21 runs and correctly returns true
;t22 runs and correctly returns 1
;t23 gives correct error
  