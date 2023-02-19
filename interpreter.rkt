#lang racket
(require "simpleParser.rkt")


; addBinding takes a name, value, and a state
; adds that key-value pair to the state
; replaces any existing binding if one already exists with the same name
; returns the state
(define addBinding
  (lambda (name val state)
    (cond
      [(null? state) (list(list name val))]
      [(eq? (car (car state)) name) (cons (list(list name val)) (cdr state))]
      (else (cons (car state) (addBinding name val (cdr state)))))))

; removeBinding takes a name and a state
; removes that key-value pair from the state
; Returns the new state
;(define removeBinding)

; findBinding takes a name and a state
; finds the binding with the correct name
; if not binding with the inputted name can be found
; throws an error using (error msg)
; returns the value of the binding
(define findBinding
  (lambda (name state)
    (cond
      [(null? state) (error name "variable used before declaration")]
      [(eq? (car (car state)) name) (car (cdr (car state)))]
      (else (findBinding name (cdr state))))))

; M_value takes an expression and a state
; evaluates the expression
; expressions can be a number, a variable, or an operator with two subexpressions
; returns the value of the expression
(define M_value
  (lambda (expr state)
    (cond
      [(not (list? expr)) ; if the expression is just a single number or variable
         (if (number? expr)
            expr
            (findBinding expr state))]
      [(eq? (car expr) '+) (+ (M_value (car (cdr expr)) state) (M_value (car (cdr (cdr expr))) state))]
      [(eq? (car expr) '-) (- (M_value (car (cdr expr)) state) (M_value (car (cdr (cdr expr))) state))]
      [(eq? (car expr) '*) (* (M_value (car (cdr expr)) state) (M_value (car (cdr (cdr expr))) state))]
      [(eq? (car expr) '/) (/ (M_value (car (cdr expr)) state) (M_value (car (cdr (cdr expr))) state))]
      [(eq? (car expr) '%) (modulo (M_value (car (cdr expr)) state) (M_value (car (cdr (cdr expr))) state))]
      (else (error "unexpected operator")))))

; M_boolean takes a conditional and a state
; evaluates the conditional (including dealing with comparison operators)
; returns true or false
;(define M_boolean)
    



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