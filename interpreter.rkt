; Interpreter Part 3 (Code modified from interpreter part 2 prof's call/cc version)
; James Redding
; Maria Eradze
; Wendy Wu

#lang racket
(require "classParser.rkt")
; (load "simpleParser.scm")


; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
(define call/cc call-with-current-continuation)


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file class)
    (scheme->language
     (call-main (interpret-statement-list-outer (parser file) (newenvironment)) class))))

; looks up the main method from the correct class and calls it
(define call-main
  (lambda (environment class)
    (interpret-function (list 'funcall 'main) (get-methods (lookup class environment)) (lambda (v env) (myerror "Uncaught exception thrown")))))

; outer layer function that adds classes to an environment
(define interpret-statement-list-outer
  (lambda (statement-list environment)
    (if (null? statement-list)
        environment
        (interpret-statement-list-outer (restof statement-list) (interpret-statement-outer (outer statement-list) environment)))))

; helper function for outer layer function. 
(define interpret-statement-outer
  (lambda (statement environment)
    (if (eq? 'class (statement-type statement))
        (insert (get-class-name statement) (make-class-closure (get-class-name statement) (get-super statement) (get-class-body statement) environment) environment)
        (myerror "Unknown statement:" (statement-type statement)))))

; adds the function to the state
(define interpret-function-declaration
  (lambda (statement environment compile-time-type instance-closure)
    (insert (get-function-name statement) (make-function-closure (get-function-name statement) (get-formal-params statement) (get-function-body statement) environment compile-time-type instance-closure) environment)))

; class closure
(define make-class-closure
  (lambda (class-name super-class-name class-body class-environment)
    (list super-class-name (get-class-methods class-name class-body class-environment) (get-instance-fields class-body (newenvironment)))))

; helper function
(define get-class-methods
  (lambda (class-name statement-list environment)
    (if (null? statement-list)
        environment
        (get-class-methods (restof statement-list) (get-class-method class-name (outer statement-list) environment)))))

; helper function
(define get-class-method
  (lambda (class-name statement environment)
    (if (eq? 'function (statement-type statement))
        (interpret-function-declaration statement environment) ; TODO: see if not passing in instance fields, causes this to break
        environment)))

; helper function
(define get-instance-fields
  (lambda (statement-list environment)
    (if (null? statement-list)
        environment
        (get-instance-fields (restof statement-list) (get-instance-field (outer statement-list) environment)))))

; helper function
(define get-instance-field
  (lambda (statement environment)
    (if (eq? 'var (statement-type statement))
        (insert (operand1 statement) (operand2 statement) environment)
        environment)))

; make instance closure
(define make-instance-closure
  (lambda (runtime-type environment throw)
    (list runtime-type (compute-initial-values (get-instance-fields-from-class-closure (lookup runtime-type environment)) environment throw))))

(define compute-initial-values
  (lambda (non-computed-environment environment throw) ; non-computed-environment - '(((a b c) ((expra) (exprb) (exprc))))
    (if (null? (caar non-computed-environment))
        environment
        (insert (caaar non-computed-environment) (eval-expression (caadar non-computed-environment) environment throw) (compute-initial-values (list (list (cdaar non-computed-environment) (cdadar non-computed-environment))))))))

; creates the function closure for a given set of formal parameters, function body, and environment
(define make-function-closure
  (lambda (func-name formal-params function-body environment compile-time-type instance-closure)
    (list formal-params function-body (lambda (func-call-env) (insert func-name (lookup func-name func-call-env) (push-frame environment))))))

; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw compile-time-type instance-closure)
    (if (null? statement-list)
        environment
        (interpret-statement-list (restof statement-list) (interpret-statement (outer statement-list) environment return break continue throw compile-time-type instance-closure) return break continue throw compile-time-type instance-closure))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw compile-time-type instance-closure)
    (cond
      ((eq? 'function (statement-type statement)) (interpret-function-declaration statement environment compile-time-type instance-closure))
      ((eq? 'funcall (statement-type statement)) (begin (interpret-function statement environment throw compile-time-type instance-closure) environment))
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return throw compile-time-type instance-closure))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment throw compile-time-type instance-closure)) ;here
      ((eq? '= (statement-type statement)) (interpret-assign statement environment throw compile-time-type instance-closure))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw compile-time-type instance-closure))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw compile-time-type instance-closure))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw compile-time-type instance-closure))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw compile-time-type instance-closure))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw compile-time-type instance-closure))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; interpret a single function given statement of the function call, the current environment, and a throw continuation
; creates the closure and func-env and then analyze whether the number of formal params = the number of actual params
; if equal, then just interprets the code block like in part2 and then returns a value/just does the function call
; returns the environment after done with interpreting the function.
(define interpret-function
  (lambda (statement environment throw compile-time-type instance-closure)    
    (call/cc
     (lambda (return)
       (let* ((closure (lookup (get-function-name statement) environment))
              (func-env (addParams (get-formal-params-from-closure closure) (get-actual-params statement) (call-make-env-from-closure closure environment) environment throw)))
         (if (eq? (length (get-formal-params-from-closure closure)) (length (get-actual-params statement)))
             (interpret-statement-list (get-body-from-closure closure) func-env return
                                       (lambda (env) (myerror "Break used outside of loop"))
                                       (lambda (env) (myerror "Continue used outside of loop"))
                                       (lambda (v env) (throw v environment))
                                       compile-time-type
                                       instance-closure)
             (myerror "Mismatched parameters and arguments number in function call:" (get-function-name statement))))))))

; Inputs are the formal params, actual params, fstate, environment, and throw
; Inserts all the corresponding formal parameter and actual parameter from the function into a fstate (function state) until the parameters runs out
; returns the function environment
(define addParams
  (lambda (formal-params actual-params fstate environment throw)
    (if (null? formal-params)
        fstate
        (addParams (restof formal-params)
                   (restof actual-params)
                   (insert (outer formal-params) (eval-expression (outer actual-params) environment throw) fstate) 
                   environment
                   throw))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return throw compile-time-type instance-closure)
    (return (eval-expression (get-expr statement) environment throw compile-time-type instance-closure))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment throw compile-time-type instance-closure)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment throw compile-time-type instance-closure) environment)
        (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment throw compile-time-type instance-closure)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment throw compile-time-type instance-closure) environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw compile-time-type instance-closure)
    (cond
      ((eval-expression (get-condition statement) environment throw compile-time-type instance-closure) (interpret-statement (get-then statement) environment return break continue throw compile-time-type instance-closure))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw compile-time-type instance-closure))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw compile-time-type instance-closure)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment compile-time-type instance-closure)
                        (if (eval-expression condition environment throw compile-time-type instance-closure)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw compile-time-type instance-closure))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment compile-time-type instance-closure))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw compile-time-type instance-closure)
    (pop-frame (interpret-statement-list (restof statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))
                                         compile-time-type
                                         instance-closure))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw compile-time-type instance-closure)
    (throw (eval-expression (get-expr statement) environment throw) environment compile-time-type instance-closure)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block compile-time-type instance-closure)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw compile-time-type instance-closure)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))
                                                 compile-time-type
                                                 instance-closure))
                                     return break continue throw compile-time-type instance-closure)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw compile-time-type instance-closure)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw compile-time-type instance-closure) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw compile-time-type instance-closure))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw compile-time-type instance-closure))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block compile-time-type instance-closure)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw compile-time-type instance-closure)
                          return break continue throw compile-time-type instance-closure))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment throw compile-time-type instance-closure)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment throw compile-time-type instance-closure)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment throw compile-time-type instance-closure)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment throw compile-time-type instance-closure)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment throw compile-time-type instance-closure)))
      ((eq? 'new (operator expr)) (make-instance-closure (operand1 expr) environment throw))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment throw compile-time-type instance-closure) environment throw compile-time-type instance-closure)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment throw compile-time-type instance-closure)
    (cond
      ((eq? 'funcall (operator expr)) (interpret-function expr environment throw compile-time-type instance-closure))
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment throw compile-time-type instance-closure)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment throw compile-time-type instance-closure)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment throw compile-time-type instance-closure)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment throw compile-time-type instance-closure)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment throw compile-time-type instance-closure)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment throw compile-time-type instance-closure)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment throw compile-time-type instance-closure))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment throw compile-time-type instance-closure)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment throw compile-time-type instance-closure)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment throw compile-time-type instance-closure)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment throw compile-time-type instance-closure)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment throw compile-time-type instance-closure)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment throw compile-time-type instance-closure)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define outer car)
(define restof cdr)
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)
(define get-function-name operand1)
(define get-formal-params operand2)
(define get-function-body operand3)
(define get-formal-params-from-closure car)
(define get-actual-params cddr)
(define call-make-env-from-closure (lambda (closure environment) ((caddr closure) environment)))
(define get-body-from-closure operand1)
(define get-super-class operator)
(define get-methods operand1)
(define get-instance-fields-from-class-closure operand2)
(define get-runtime-type operator)
(define get-instance-fields-from-instance-closure operand1)
(define get-class-name operand1)
(define get-super
  (lambda (statement)
    (if (null? (caddr statement))
        '()
        (car (cdaddr statement)))))
(define get-class-body operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (restof environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (unbox (car l)))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (outer environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (outer environment)) (restof environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (box (scheme->language val)) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (outer environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (outer varlist)) (begin (set-box! (outer vallist) val) vallist)) ;outer is just car (the outermost element or the first element of the list
      (else (cons (outer vallist) (update-in-frame-store var val (restof varlist) (restof vallist))))))) ;restof = cdr

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))


(interpret "newtest2.txt" 'B)



