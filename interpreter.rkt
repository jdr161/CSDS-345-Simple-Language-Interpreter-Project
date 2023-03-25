#lang racket
(require "simpleParser.rkt")
;Team 12
;James Redding
;Maria Eradze
;Wendy Wu

; helper function declared? finds if a given var name is in the state or not
 (define declared-cc
  (lambda (name state break)
    (cond
      [(null? state)                 #f]
      [(null? (getFirstStatement state))           (declared-cc name (getRestOfStatements state) break)]
      [(list? (getFirstStatementType state))     (if (null? (getRestOfStatements state)) ; case that state is a list of layers
                                         (declared-cc name (getFirstStatement state) break)
                                         (or (declared-cc name (getFirstStatement state) break) (declared-cc name (getRestOfStatements state) break)))]
      [(eq? (getFirstStatementType state) name)  (break #t)] ; case that state is a single layer
      [else                          (declared-cc name (getRestOfStatements state) break)])))
(define declared?
  (lambda (name state)
     (call/cc
      (lambda (k) (declared-cc name state k)))))

; addBinding takes a name, value, and a state
; adds that key-value pair to the state
; replaces any existing binding if one already exists with the same name
; returns the state
(define addBinding
  (lambda (name val state)
    (if (declared? name state)
        (changeBinding name val state)
        (cons (cons (list name val) (getFirstStatement state)) (getRestOfStatements state)))))
(define changeBinding
  (lambda (name val state)
    (cond
      [(null? state)                        (error "something wrong with addBinding")]
      [(null? (getRestOfStatements state))     (list (changeBindingInLayer name val (getFirstStatement state)))]
      (else          (cons (changeBindingInLayer name val (getFirstStatement state)) (changeBinding name val (getRestOfStatements state)))))))
(define changeBindingInLayer
  (lambda (name val layer)
    (cond
      [(null? layer) '()]
      [(eq? (getFirstStatementType layer) name)      (cons (list name val) (getRestOfStatements layer))]
      (else (cons (getFirstStatement layer)          (changeBindingInLayer name val (getRestOfStatements layer)))))))

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
      (else (call/cc (lambda (return)
                       (findBinding-cc name state return))))))) 
;findBinding-cc delegates the findingBinding operation in layers
(define findBinding-cc
  (lambda (name state return)
    (cond
      [(null? state)       state]
      [(null? (getRestOfStatements state)) (findBindingInLayer-cc name (getFirstStatement state) return)]
      [(and (null? (findBindingInLayer-cc name (getFirstStatement state) return)) (null? (findBinding-cc name (getRestOfStatements state) return)))
                   (error "variable used before declaration")];means that the code can't find the corresponding named variable from any layer 
      [else (call/cc (lambda (return) (or (findBindingInLayer-cc name (getFirstStatement state) return) (findBinding-cc name (getRestOfStatements state) return))))])))
;the above else means once we find the value of the named variable we immediately return it (since we are assume every var is named differently
;only one place will have it and therefore we use an or

;findBindingInLayer find Binding value of a named variable in a single layer (traverse through pairs in the layer)
(define findBindingInLayer-cc
  (lambda (name layer return) ;layer is initially ((k 8) (x 7))
    (cond
      [(null? layer) layer]
      [(eq? (getFirstStatementType layer) name) (return (bindingVal layer))]
      (else (findBindingInLayer-cc name (getRestOfStatements layer) return)))))

;bindingVal returns the ((x 10)) value 10 from this binding pair
(define bindingVal
  (lambda (expression)
    (car (cdr (car expression)))))

; operator function
;(> 10 20) basically the operator function gives the operator of the pair/list
(define operator
  (lambda (expression)
    (car expression)))

;leftoperand is the 10 of the above example
;input is the expr
;return the cadr of expr
;cadr is (car (cdr expr))
(define leftoperand cadr)


;rightoperand is the 20 of the above example
;input is the expr
;return the caddr of expr
;caddr is (car (cdr (cdr expr)))
(define rightoperand caddr)


; fourthoperand takes the expression expr
; returns the (cdr (cdr (cdr expr)))
; which is the fourth operand in a list that is the expr
(define fourthoperand
  (lambda (expr)
       (cdr (cdr (cdr expr)))))

; getFirstStatementType returns the type of statement of the first statement for a list of statements
; (getFirstStatementType ((var x) (= x 1))) => 'var
(define getFirstStatementType
  (lambda (tree)
    (car (car tree))))

; getFirstStatement returns the first statement in a list of statements
; (getFirstStatement ((var x) (= x 1))) => (var x)
(define getFirstStatement
  (lambda (tree)
    (car tree)))

; getRestOfStatements returns the first statement in a list of statements
; (getRestOfStatements ((var x) (= x 1))) =>  '((= x 1))
(define getRestOfStatements
  (lambda (tree)
    (cdr tree)))

; newState returns a blank state, ready to pass into M_state
(define newState
  (lambda ()
    (list (list))))

; M_value takes an expression and a state
; evaluates the expression
; expressions can be a number, a variable, or an operator with two subexpressions
; returns the value of the expression
(define M_value
  (lambda (expr state)
    (cond
      [(not (list? expr))    (if (number? expr) ; if the expression is just a single number or variable
                              expr
                              (findBinding expr state))]
      [(eq? (operator expr) '+) (+ (M_value (leftoperand expr) state) (M_value (rightoperand expr) state))]
      [(eq? (operator expr) '-) (if (null? (cdr (cdr expr))) ; if the '- is a unary operator
                                    (- (M_value (leftoperand expr) state))
                                    (- (M_value (leftoperand expr) state) (M_value (rightoperand expr) state)))] 
      [(eq? (operator expr) '*)         (* (M_value (leftoperand expr) state) (M_value (rightoperand expr) state))]
      [(eq? (operator expr) '/)  (quotient (M_value (leftoperand expr) state) (M_value (rightoperand expr) state))]
      [(eq? (operator expr) '%) (remainder (M_value (leftoperand expr) state) (M_value (rightoperand expr) state))]
      (else                                                                              (M_boolean expr state)))))

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
      [(eq? (operator conditional) '&&)                      (and (M_boolean (leftoperand conditional) state) (M_boolean (rightoperand conditional)  state))]
      [(eq? (operator conditional) '||)                      (or (M_boolean (leftoperand conditional) state) (M_boolean (rightoperand conditional)  state))]
      [(eq? (operator conditional) '!)                       (not (M_boolean (leftoperand conditional) state))]
      [(eq? (operator conditional) '==)  (eq? (M_value (leftoperand conditional) state) (M_value (rightoperand conditional) state))] ;fixed the if (true == true) return true case
      [(eq? (operator conditional) '!=)   (not (eq? (M_value (leftoperand conditional) state) (M_value (rightoperand conditional)  state)))] ;similar case as above case resolved
      [(number? (M_value conditional state))   (error "int in if cond")] ; expected error for int in if cond
      (else                                      (error "cannot compare int and bool")))))


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
; assigns the value of the expression to the variable in the state
; returns the new state
(define M_assignment
  (lambda (expr state)
    (cond
      [(and (eq? (operator expr) '=) (declared? (leftoperand expr) state)) (addBinding (leftoperand expr) (M_value(rightoperand expr) state) state)]
      (else                                                                (error (leftoperand expr) "variable not declared")))))

; M_return takes a return statement (in the form (return expression)) and a state
; If return is already defined, returns the state
; otherwise, it evaluates the expression and 
; returns the value of the expression
(define M_return
  (lambda (statement state)
    (cond
      ((not (null? (M_value (leftoperand statement) state)))      (M_value (leftoperand statement) state))
      (else (error "error the return variable doesn't exist")))))

; M_if takes an if statement (in the form (if conditional then-statement optional-else-statement)) and a state
; evaluates the conditional and calls M_state on the correct statement as necessary
; returns the new state
(define M_if
  (lambda (statements state return break continue throw)
    (cond
      [(null? statements) state] ; i assume if passed in it is not gonna be null though
      [(M_boolean (leftoperand statements) state) (M_state (cons (rightoperand statements) '()) state return break continue throw)] ;if conditional is true, we execute thenstatement and update the state
      [(null? (fourthoperand statements))    state] ; check if the else statement is null then just return state as it is
      [else                                     (M_state (fourthoperand statements) state return break continue throw)]))) ; return the else statement state if there exist else 


; M_while takes a while statement (in the form 	(while conditional body-statement)) and a state
; recurses if the conditional returns true
; returns the state if the conditional returns false
;implemented call/cc in case of break from M_state and continue in case of continue in M_state
(define M_while
  (lambda (cstatements state return throw)
    (call/cc (lambda (break)     
      (cond
        [(M_boolean (leftoperand cstatements) state) (M_while cstatements (M_state (cons (rightoperand cstatements) '())
                                                                                   state
                                                                                   return ; return is the same
                                                                                   break  ; break will return a state the call/cc we just set up
                                                                                   (lambda (contState) (break (M_while cstatements contState return throw))) ; continue skips everything else in an iteration and directly begins the next iteration
                                                                                   throw) ; throw stays the same
                                                              return throw)] 
        ;while the condition is true, we will continue running the body statement and update the state using M_while
        (else                                         state)))))) ;if returned false then just return the state


;removeLayer removes the car of the state, or the top layer of the state
(define removeLayer
  (lambda (state)
    (cdr state)))
;addLayer add a single layer in '() to the state
(define addLayer
  (lambda (state)
    (cons '() state)))
;M_block takes care of any code in block form that starts with 'begin
;first delegate the code within the block to M_state and update the state with added layer, then deletes the layer after updated layer
(define M_block
  (lambda (stmt state return break continue throw)
    (removeLayer (M_state (getRestOfStatements stmt) (addLayer state) return break continue throw))))

;M_throw throws an error e to the state
(define M_throw
  (lambda (state e throw)
    (throw (M_value e state) state)))

;getTryBlock gets the body of the try block (try body ....)
(define getTryBlock
  (lambda (tryStmt)
    (cons 'begin (car (cdr tryStmt)))))
;getFinallyBlock gets the body of the finally block (try body () (finally body))
(define getFinallyBlock
  (lambda (tryStmt)
    (if (null? (car (cdr (cdr (cdr tryStmt)))))
        '(begin)  ;just { }
        (cons 'begin (car (cdr (car (cdr (cdr (cdr tryStmt)))))))))) ;{ some code }
;getCatchStmts gets the body of the Catch block (try body (catch (e) body) (finally body))
(define getCatchStmts
  (lambda (tryStmt throw e-val state)
    (if (null? (car (cdr (cdr tryStmt))))
        (throw e-val state) ; if catch is completely empty, throw the error up a level: (try (body) () (finally (body))) => throw error to one level up
        (car (cdr (cdr (car (cdr (cdr tryStmt))))))))) ;'(try (body) (catch (e) (body)) (finally (body))) when we can get catch's body
;getCatchErrorName gets the error name e from (try body (catch (e) body) (finally body))
(define getCatchErrorName
  (lambda (tryStmt)
    (car (car (cdr (car (cdr (cdr tryStmt))))))))  ; get the e from (catch (e) body)

;M_try takes care of a try-catch-finally block and correctly updates the state no matter the structure of the block
(define M_try
  (lambda (stmt state return break continue throw)
    (call/cc (lambda (return-try)
               (M_block (getFinallyBlock stmt)
                        (M_block (getTryBlock stmt)
                                 state
                                (lambda (v) (begin (M_block (getFinallyBlock stmt) state return break continue throw) (return v))) ; return must do finally block before returning
                                (lambda (state) (break (M_block (getFinallyBlock stmt) state return break continue throw))) ; break must do finally block before breaking
                                (lambda (state) (continue (M_block (getFinallyBlock stmt) state return break continue throw))) ; continue must do finally block before calling continue
                                (lambda (e-val state) (return-try (M_block (getFinallyBlock stmt)
                                                                           (removeLayer (M_state ; we call M_state because we create the new layer when we call addBinding on e
                                                                                         (getCatchStmts stmt throw e-val state) ; getCatchStmts throws the error up a level if there is no catch
                                                                                         (addBinding (getCatchErrorName stmt) e-val state)
                                                                                         return
                                                                                         (lambda (state-new) (break (removeLayer state-new)))
                                                                                         (lambda (state-new) (continue (removeLayer state-new)))
                                                                                         (lambda (e-val-new state-new) (throw e-val-new state-new))))
                                                                           return
                                                                           break
                                                                           continue
                                                                           throw))))
               return
               break
               continue
               throw)))))
    
; M_state takes a syntax tree and a state
; checks what kind of statement the first statement in the syntax tree is and calls the correct function on it
; recurses on itself with the the cdr of the syntax tree and the state returned from the the function called on the car of the syntax tree
; if the state is just a single value, returns that value

(define M_state
  (lambda (tree state return break continue throw)
    (cond
      [(null? tree)                                state]
      [(eq? 'var (getFirstStatementType tree))     (M_state (getRestOfStatements tree) (M_declaration (getFirstStatement tree) state) return break continue throw)]
      [(eq? '= (getFirstStatementType tree))       (M_state (getRestOfStatements tree) (M_assignment (getFirstStatement tree) state) return break continue throw)]
      [(eq? 'return (getFirstStatementType tree))  (return (M_return (getFirstStatement tree) state))]
      [(eq? 'if (getFirstStatementType tree))      (M_state (getRestOfStatements tree) (M_if (getFirstStatement tree) state return break continue throw) return break continue throw)]
      [(eq? 'while (getFirstStatementType tree))   (M_state (getRestOfStatements tree) (M_while (getFirstStatement tree) state return throw) return break continue throw)]
      [(eq? 'break (getFirstStatementType tree))   (break (removeLayer state))] ;if you break out of a while loop, you have the remove the top most layer wiht variables declared inside it
      [(eq? 'throw (getFirstStatementType tree))   (M_throw state (leftoperand (getFirstStatement tree)) throw)]
      [(eq? 'try (getFirstStatementType tree))     (M_state (getRestOfStatements tree) (M_try (getFirstStatement tree) state return break continue throw) return break continue throw)]
      [(eq? 'begin (getFirstStatementType tree))   (M_state (getRestOfStatements tree) (M_block (getFirstStatement tree) state return break continue throw) return break continue throw)]
      [(eq? 'continue (getFirstStatementType tree))(continue state)]
      (else                           (error (getFirstStatementType tree) "unrecognized statement type")))))
  

; interpret takes a filename
; calls the parser to get the syntax tree
; calls M_state on that syntax tree
; returns the return value from that syntax tree
(define interpret
  (lambda (filename)
    (call/cc (lambda (return) (M_state(parser filename) (newState) return (lambda (v) error) (lambda (v) error) (lambda (v1) error)))))) ; () shows returns true for (null? '())

(parser "test17.txt")
;(parser "test1.txt")
;(interpret "test1.txt") ; expected: 20
;(interpret "test2.txt") ; expected: 164
;(interpret "test3.txt") ; expected: 32
;(interpret "test4.txt") ; expected: 2
;(interpret "test5.txt") ; expected: error        
;(interpret "test6.txt") ; expected: 25
;(interpret "test7.txt") ; expected: 21
;(interpret "test8.txt") ; expected: 6
;(interpret "test9.txt") ; expected: -1
;(interpret "test10.txt") ; expected: 789
;(interpret "test11.txt") ; expected: error       
;(interpret "test12.txt") ; expected: error
;(interpret "test13.txt") ; expected: error
;(interpret "test14.txt") ; expected: 12
;(interpret "test15.txt") ; expected: 125
;(interpret "test16.txt") ; expected: 110
;(interpret "test17.txt") ; expected: 2000400      (failed, returned error i: variable name is already taken)
;(interpret "test18.txt") ; expected: 101
;(interpret "test19.txt") ; expected: error          (failed, returned arity mismatch, expected: 1, given 2 at throw e-val-new state-new in M_try)
;(interpret "test20.txt") ; expected: ??
;(interpret "test21.txt") ; expected: ??
;(interpret "test22.txt") ; expected: ??
;(interpret "test23.txt") ; expected: ??
;(interpret "test24.txt") ; expected: ??
; to catch errors we make: (with-handlers ([exn:fail? (lambda (exn) 'air-bag)])
;                                 (error "crash!"))

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
  