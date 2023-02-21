#lang racket
(require "simpleParser.rkt")


; addBinding takes a name, value, and a state
; adds that key-value pair to the state
; replaces any existing binding if one already exists with the same name
; returns the state
(define addBinding
  (lambda (name val state)
    (cond
      [(eq? val 'true) (addBinding name #t state)]
      [(eq? val 'false) (addBinding name #f state)]
      [(null? state) (list(list name val))]
      [(eq? (car (car state)) name) (cons (list name val) (cdr state))]
      (else (cons (car state) (addBinding name val (cdr state)))))))

; if the state doesn't contain the name of the key-value pair, we give error
; but if (eq?(car(car state) name) cdr state thus removed that pairing
; else (cons (car state) removebinding(cdr state)
;(removeBinding 'x '((y 5) (x 7) (k 19) (b 10)))
;WW
(define removeBinding
  (lambda (name state)
    (cond
      [(null? state) state]
      [(eq? (caar state) name) (cdr state)]
      [else (cons (car state) (removeBinding name (cdr state)))])))

; findBinding takes a name and a state
; finds the binding with the correct name
; if not binding with the inputted name can be found
; throws an error using (error msg)
; returns the value of the binding
(define findBinding
  (lambda (name state)
    (cond
      [(eq? name 'true) #t]
      [(eq? name 'false) #f]
      [(null? state) (error name "variable used before declaration")]
      [(eq? (car (car state)) name) (car (cdr (car state)))]
      (else (findBinding name (cdr state))))))

; helper function declared? finds if a given var name is in the state or not
 (define declared?
  (lambda (name state)
    (cond
      [(null? state) #f]
      [(eq? (car(car state)) name) #t]
      [else (declared? name (cdr state))])))


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
(define M_boolean
  (lambda (conditional state)
    (cond
      ((boolean? (car conditional))    (car conditional))
      ((eq? (car conditional) '==)     (eq? (M_value (car (cdr conditional)) state) (M_value (car (cdr (cdr conditional))) state)))
      ((eq? (car conditional) '!=)     (not (eq? (M_value (car (cdr conditional)) state) (M_value (car (cdr (cdr conditional))) state))))
      ((eq? (car conditional) '<)      (< (M_value (car (cdr conditional)) state) (M_value (car (cdr (cdr conditional))) state)))
      ((eq? (car conditional) '<=)     (<= (M_value (car (cdr conditional)) state) (M_value (car (cdr (cdr conditional))) state)))
      ((eq? (car conditional) '>)      (> (M_value (car (cdr conditional)) state) (M_value (car (cdr (cdr conditional))) state)))
      ((eq? (car conditional) '>=)     (>= (M_value (car (cdr conditional)) state) (M_value (car (cdr (cdr conditional))) state)))
      ((eq? (car conditional) '&&)     (and (M_boolean (car (cdr conditional)) state) (M_boolean (car (cdr (cdr conditional))) state)))
      ((eq? (car conditional) '||)     (or (M_boolean (car (cdr conditional)) state) (M_boolean (car (cdr (cdr conditional))) state)))
      ((eq? (car conditional) '!)      (not (M_boolean (car (cdr conditional)) state)))
      (else (error "unexpected conditional operator")))))


; M_declaration takes a declaration statement (in the form (var variable) or (var variable value)) and a state
; checks that the variable name is available and adds the new variable to the state
; if the variable name is not available, calls the Scheme (error ...) function
; if the variable does not have a value assigned in the passed in statement, sets the value to null
; returns the new state
(define M_declaration
  (lambda (statement state)
    (cond
      [(declared? (car (car statement)) state) (error "variable name is already taken")]
      [(null? (cdr (car (car statement))))     (addBinding (car (car statement)) null state)]
      (else                                    (addBinding (car (car statement)) (car (car (car statement))) state))
    

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
    [(and (eq? (car expr) '=) (declared? (car(cdr expr)) state)) (addBinding (car (cdr expr)) (car(cdr(cdr expr))) state)]
    [else state])))

; M_return takes a return statement (in the form (return expression)) and a state
; evaluates the expression
; returns the value of the expression
;(define M_return)

; M_if takes an if statement (in the form (if conditional then-statement optional-else-statement)) and a state
; evaluates the conditional and calls M_state on the correct statement as necessary
; returns the new state
; WW
(define M_if
  (lambda (statements state)
    (cond
      [(null? statements) state] ; i assume if passed in it is not gonna be null though
      [(M_boolean (car (cdr statements))) (M_state (cons (car (cdr (cdr statements))) '()) state)]
      [(null? (car (cdr (cdr (cdr statements))))) state] ;check if the else statemet is nullthen just return state as it is
      [else (M_state (cons (car (cdr (cdr (cdr statements)))) '()) state)]))) ; return the false statement state


; M_while takes a while statement (in the form 	(while conditional body-statement)) and a state
; recurses if the conditional returns true
; returns the state if the conditional returns false
; Maria
;(define M_while)       

; M_state takes a syntax tree and a state
; checks what kind of statement the first statement in the syntax tree is and calls the correct function on it
; recurses on itself with the the cdr of the syntax tree and the state returned from the the function called on the car of the syntax tree
; if the state is just a single value, returns that value
(define M_state
  (lambda (tree state)
    (cond
      [(null? tree) (error "no return value")]
      [(eq? 'var (car (car tree))) (M_state (cdr tree) (M_declaration (car tree) state))]
      [(eq? '= (car (car tree))) (M_state (cdr tree) (M_assignment (car tree) state))]
      [(eq? 'return (car (car tree))) (M_return (car tree) state)]
      [(eq? 'if (car (car tree))) (M_state (cdr tree) (M_if (car tree) state))]
      [(eq? 'while (car (car tree))) (M_state (cdr tree) (M_while (car tree) state))]
      (else (error "unrecognized statement type")))))
  

; interpret takes a filename
; calls the parser to get the syntax tree
; calls M_state on that syntax tree
; returns the return value from that syntax tree
(define interpret
  (lambda (filename)
    (M_state(parser filename))))






  