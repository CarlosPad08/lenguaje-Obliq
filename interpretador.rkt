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

    ;; Manejo de objetos
    [(and (list? exp) (equal? (car exp) 'object))
     (let ([fields (map (lambda (pair)
                          (let* ([key (car pair)]
                                 [val (cdr pair)]
                                 [evaluated-val (if (and (list? val) (equal? (car val) 'proc))
                                                    (eval-exp val env)
                                                    (eval-exp val env))]
                                 [normalized-pair (if (equal? key '=>) (cons (cadr pair) (caddr pair)) pair)])
                            (cons (car normalized-pair) evaluated-val)))
                        (cdr exp))])
       (lambda (msg . args)
         (case msg
           [(get) (let ([field (car args)])
                   (if (assoc field fields)
                       (cdr (assoc field fields))
                       (error "Campo no encontrado" field)))]
           [(update) (let ([field (car args)] [new-val (cadr args)])
                      (if (assoc field fields)
                          (set! fields (map (lambda (pair)
                                              (if (eq? (car pair) field)
                                                  (cons (car pair) new-val)
                                                  pair))
                                            fields))
                          (error "Campo no encontrado para actualizar" field))
                      new-val)]
           [(send) (let ([method-name (car args)] [method-args (cdr args)])
                    (if (assoc method-name fields)
                        (apply (cdr (assoc method-name fields)) method-args)
                        (error "Método no encontrado" method-name)))]
           [else (error "Operación no válida en el objeto" msg)])))]

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
(display "Inicio de las pruebas iniciales")
(display "\n")

(eval-exp 42 initial-env)             ;; Constante numérica
(eval-exp '(+ 1 2 3) initial-env)     ;; Suma
(eval-exp '(* 2 5) initial-env)       ;; Multiplicación
(eval-exp '(< 3 5) initial-env)       ;; Comparación menor que
(eval-exp '(is x 10) initial-env)     ;; Comparación de igualdad con variable
(eval-exp '(if (< x y) x y) initial-env) ;; Condicional: debería devolver 10
(eval-exp '(for i 1 3 (+ i 1)) initial-env) ;; Ciclo for: debería evaluar las expresiones con i=1,2,3
(display "Fin de las pruebas iniciales")
(display "\n")
(display "----------------------------------------------------------------------------------------------")
(display "\n")

;; Definición y uso de procedimientos
(display "Definicion y uso de procedimientos")
(display "\n")

(define extended-env (eval-exp '(define my-proc (proc (a b) (+ a b))) initial-env))
(eval-exp '(apply my-proc (3 4)) extended-env) ;; Debería devolver 7

(display "Fin definicion y uso de procedimientos")
(display "\n")
(display "----------------------------------------------------------------------------------------------")
(display "\n")

;; Manejo de errores
;; (eval-exp '(try (/ 1 0) "Error capturado") initial-env) ;; Debería devolver "Error capturado"

;; ============================
;; Pruebas de Objetos
;; ============================

(display "Inicio de manejo de objetos")
(display "\n")

(define obj (eval-exp '(object (x . 10) (y . 20) (add . (proc (a b) (+ a b)))) empty-env))

;; Acceso a campos
(obj 'get 'x) ;; Debería devolver 10

;; Actualización de campos
(obj 'update 'x 64)

;; Invocación de métodos
(obj 'send 'add 3 4) ;; Debería devolver 7
