#lang racket

;; ============================
;; Obliq Interpreter - Starter
;; ============================

;; Este es un esquema inicial para implementar un intérprete para el lenguaje Obliq en Racket.
;; Proporciona las bases para trabajar en el análisis sintáctico, la evaluación de expresiones y el manejo de ambientes.

;; ----------------------------
;; Representación de Ambientes
;; ----------------------------

;; Un ambiente es una lista de asociaciones (variable . valor).
(define empty-env '())

;; Buscar una variable en el ambiente.
(define (lookup var env)
  (cond [(null? env) (error "Variable no encontrada" var)]
        [(eq? (car (car env)) var) (cdr (car env))]
        [else (lookup var (cdr env))]))

;; Extender el ambiente con una nueva asociación.
(define (extend-env var val env)
  (cons (cons var val) env))

;; Actualizar el valor de una variable en el ambiente.
(define (update-env var val env)
  (cond [(null? env) (error "Variable no encontrada para actualizar" var)]
        [(eq? (car (car env)) var) (cons (cons var val) (cdr env))]
        [else (cons (car env) (update-env var val (cdr env)))]))

;; ----------------------------
;; Representación de Expresiones
;; ----------------------------

;; Las expresiones se representan como estructuras.
(struct num (value))
(struct var (name))
(struct add (left right))
(struct sub (left right))
(struct mul (left right))
(struct div (left right))
(struct if (cond then else))
(struct bool (value))
(struct equal (left right)) ; Cambiado de eq a equal
(struct lt (left right))
(struct gt (left right))
(struct concat (left right))
(struct proc (params body))
(struct call (proc args))
(struct seq (exprs))

;; ----------------------------
;; Evaluador de Expresiones (corregido)
;; ----------------------------

(define (eval-exp exp env)
  (cond
    ;; Primitivas numéricas
    [(num? exp) (num-value exp)]
    ;; Variables
    [(var? exp) (lookup (var-name exp) env)]
    ;; Aritmética
    [(add? exp)
     (+ (eval-exp (add-left exp) env)
        (eval-exp (add-right exp) env))]
    [(sub? exp)
     (- (eval-exp (sub-left exp) env)
        (eval-exp (sub-right exp) env))]
    [(mul? exp)
     (* (eval-exp (mul-left exp) env)
        (eval-exp (mul-right exp) env))]
    [(div? exp)
     (/ (eval-exp (div-left exp) env)
        (eval-exp (div-right exp) env))]
    ;; Expresiones booleanas
    [(bool? exp) (bool-value exp)]
    [(equal? exp)
     (= (eval-exp (equal-left exp) env)
        (eval-exp (equal-right exp) env))]
    [(lt? exp)
     (< (eval-exp (lt-left exp) env)
        (eval-exp (lt-right exp) env))]
    [(gt? exp)
     (> (eval-exp (gt-left exp) env)
        (eval-exp (gt-right exp) env))]
    ;; Condicionales
    [(if? exp)
     (if (eval-exp (if-cond exp) env)
         (eval-exp (if-then exp) env)
         (eval-exp (if-else exp) env))]
    ;; Operaciones de cadenas
    [(concat? exp)
     (string-append (number->string (eval-exp (concat-left exp) env))
                    (number->string (eval-exp (concat-right exp) env)))]
    ;; Procedimientos
    [(proc? exp)
     (lambda args
       (let ([new-env (foldl (lambda (param-arg env)
                               (extend-env (car param-arg) (cdr param-arg) env))
                             env
                             (map cons (proc-params exp) args))])
         (eval-exp (proc-body exp) new-env)))]
    [(call? exp)
     (apply (eval-exp (call-proc exp) env)
            (map (lambda (arg) (eval-exp arg env))
                 (call-args exp)))]
    ;; Secuencias
    [(seq? exp)
     (foldl (lambda (e _) (eval-exp e env))
            #f
            (seq-exprs exp))]
    [else (error "Expresión desconocida" exp)]))

;; ----------------------------
;; Programa de Prueba
;; ----------------------------

;; Ambiente inicial
(define test-env (extend-env 'x 10 empty-env))

;; Prueba de primitivas numéricas
(define test-num (add (num 5) (var 'x))) ; Resultado esperado: 15

;; Prueba de cadenas
(define test-str (concat (num 5) (num 3))) ; Resultado esperado: "53"

;; Prueba de expresiones booleanas
(define test-bool (if (lt (num 3) (num 5)) (num 1) (num 0))) ; Resultado esperado: 1

;; Prueba de igualdad
(define test-equal (if (equal (num 3) (num 3)) (num 1) (num 0))) ; Resultado esperado: 1

;; Prueba de procedimientos
(define test-proc (call (proc '(a b) (add (var 'a) (var 'b))) (list (num 5) (num 10)))) ; Resultado esperado: 15

;; Prueba de secuencias
(define test-seq (seq (list (add (num 5) (num 5)) (mul (num 2) (num 3))))) ; Resultado esperado: 6

;; Ejecutar pruebas
(eval-exp test-num test-env)
(eval-exp test-str test-env)
(eval-exp test-bool test-env)
(eval-exp test-equal test-env)
(eval-exp test-proc test-env)
(eval-exp test-seq test-env)