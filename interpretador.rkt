#lang racket
(require racket/list)

;; ============================
;; Obliq Interpreter - Starter
;; ============================

;; Representación de Ambientes
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

;; ============================
;; Evaluación de Expresiones
;; ============================

;; Función principal de evaluación
(define (eval-exp exp env)
  (cond 
    ;; Constantes numéricas
    [(number? exp) exp]

    ;; Operaciones aritméticas
    [(and (list? exp) (member (car exp) '(+ - * %)))
     (let ([args (map (lambda (e) (eval-exp e env)) (cdr exp))])
       (case (car exp)
         [(+) (apply + args)]
         [(-) (apply - args)]
         [(*) (apply * args)]
         [(%) (apply modulo args)]))]

    ;; Operaciones relacionales
    [(and (list? exp) (member (car exp) '(< <= > >= is)))
     (let ([args (map (lambda (e) (eval-exp e env)) (cdr exp))])
       (case (car exp)
         [(<) (< (car args) (cadr args))]
         [(<=) (<= (car args) (cadr args))]
         [(>) (> (car args) (cadr args))]
         [(>=) (>= (car args) (cadr args))]
         [(is) (equal? (car args) (cadr args))]))]

    ;; Condicionales
    [(and (list? exp) (equal? (car exp) 'if))
     (let ([cond (eval-exp (cadr exp) env)])
       (if cond
           (eval-exp (caddr exp) env)
           (eval-exp (cadddr exp) env)))]

    ;; Ciclo for
    [(and (list? exp) (equal? (car exp) 'for))
     (let loop ([i (eval-exp (caddr exp) env)]
                [end (eval-exp (cadddr exp) env)]
                [env env])
       (if (> i end)
           'ok
           (begin
             (eval-exp (car (cddddr exp)) (extend-env (cadr exp) i env))
             (loop (+ i 1) end env))))]

    ;; Definición de procedimientos
    [(and (list? exp) (equal? (car exp) 'proc))
     (let ([params (cadr exp)]
           [body (caddr exp)])
       (lambda args
         (if (not (= (length params) (length args)))
             (error "Número incorrecto de argumentos" params args)
             (let ([new-env (foldr (lambda (param-val env)
                                     (extend-env (car param-val) (cdr param-val) env))
                                   env
                                   (map cons params args))])
               (eval-exp body new-env)))))]

    ;; Aplicación de procedimientos
    [(and (list? exp) (equal? (car exp) 'apply))
     (let ([proc (eval-exp (cadr exp) env)]
           [args (map (lambda (e) (eval-exp e env)) (caddr exp))])
       (apply proc args))]

    ;; Definición de una variable
    [(and (list? exp) (equal? (car exp) 'define))
     (let ([var (cadr exp)]
           [val (eval-exp (caddr exp) env)])
       (extend-env var val env))]

    ;; Manejo de errores y excepciones
    [(and (list? exp) (equal? (car exp) 'try))
     (let ([try-block (cadr exp)]
           [catch-block (caddr exp)])
       (with-handlers ([exn:fail? (lambda (_) (eval-exp catch-block env))])
         (eval-exp try-block env)))]

    ;; Variable
    [(symbol? exp) (lookup exp env)]

    ;; Error por expresión no reconocida
    [else (error "Expresión no reconocida" exp)]))

;; ============================
;; Pruebas Iniciales
;; ============================

;; Ambiente inicial con algunas variables
(define initial-env (extend-env 'x 10 (extend-env 'y 20 empty-env)))

;; Pruebas
(eval-exp 42 initial-env)             ;; Constante numérica
(eval-exp '(+ 1 2 3) initial-env)     ;; Suma
(eval-exp '(* 2 5) initial-env)       ;; Multiplicación
(eval-exp '(< 3 5) initial-env)       ;; Comparación menor que
(eval-exp '(is x 10) initial-env)     ;; Comparación de igualdad con variable
(eval-exp '(if (< x y) x y) initial-env) ;; Condicional: debería devolver 10
(eval-exp '(for i 1 3 (+ i 1)) initial-env) ;; Ciclo for: debería evaluar las expresiones con i=1,2,3

;; Definición y uso de procedimientos
(define extended-env (eval-exp '(define my-proc (proc (a b) (+ a b))) initial-env))
(eval-exp '(apply my-proc (3 4)) extended-env) ;; Debería devolver 7

;; Manejo de errores
;; (eval-exp '(try (/ 1 0) "Error capturado") initial-env) ;; Debería devolver "Error capturado"
