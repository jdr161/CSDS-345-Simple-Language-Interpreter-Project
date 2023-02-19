#lang racket
(require "simpleParser.rkt")


; addBinding takes a name, value, and a state
; adds that key-value pair to the state
; returns the state
;1(define addBinding)

; removeBinding takes a name and a state
; removes that key-value pair from the state
; Returns the new state
; if the state doesn't contain the name of the key-value pair, we give error
; but if (eq?(car(car state) name) cdr state thus removed that pairing
; else (cons (car state) removebinding(cdr state)
;(removeBinding 'x '((y 5) (x 7) (k 19) (b 10)))
(define removeBinding
  (lambda (name state)
    (cond
<<<<<<< Updated upstream
      [(null? state) (error "cannot find binding name pair in state")]
=======
      [(null? state) state]
>>>>>>> Stashed changes
      [(eq? (caar state) name) (cdr state)]
      [else (cons (car state) (removeBinding name (cdr state)))])))

; M_boolean takes a conditional and a state
; evaluates the conditional (including dealing with comparison operators)
; returns true or false
;(define M_boolean)

; M_value takes an expression and a state
; evaluates the expression
; returns the value of the expression
;(define M_value)


; M_declaration takes a declaration statement (in the form (var variable) or (var variable value)) and a state
; checks that the variable name is available and adds the new variable to the state
; if the variable name is not available, calls the Scheme (error ...) function
; if the variable does not have a value assigned in the passed in statement, sets the value to null
; returns the new state
;(define M_declaration)

; M_assignment takes an assignment statement (in the form (= variable expression)) and a state
; assigns the value of the expression to the variable in the state
; returns the new state
;(define M_assignment)

; M_return takes a return statement (in the form (return expression)) and a state
; evaluates the expression
; returns the value of the expression
;(define M_return)

; M_if takes an if statement (in the form (if conditional then-statement optional-else-statement)) and a state
; evaluates the conditional and calls M_state on the correct statement as necessary
; returns the new state
;(define M_if)

; M_while takes a while statement (in the form 	(while conditional body-statement)) and a state
; recurses if the conditional returns true
; returns the state if the conditional returns false
;(define M_while)       

; M_state takes a syntax tree and a state
; checks what kind of statement the first statement in the syntax tree is and calls the correct function on it
; recurses on itself with the the cdr of the syntax tree and the state returned from the the function called on the car of the syntax tree
; if the state is just a single value, returns that value
;(define M_state)

; interpret takes a filename
; calls the parser to get the syntax tree
; calls M_state on that syntax tree
; returns the return value from that syntax tree
;(define interpret
  ;(lambda (filename)
    ;(M_state(parser filename))))