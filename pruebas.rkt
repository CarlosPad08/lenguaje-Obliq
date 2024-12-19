#lang racket

(require "interpretador.rkt")

;; ============================
;; Pruebas
;; ============================

(display "Inicio de las pruebas desde archivo externo")
(display "\n")

;; Ambiente inicial
(define initial-env (extend-env 'x 10 (extend-env 'y 20 empty-env)))

;; Prueba: Constante numérica
(display "Prueba: Constante numérica\n")
(display (eval-exp 42 initial-env))
(display "\n")

;; Prueba: Suma
(display "Prueba: Suma\n")
(display (eval-exp '(+ 1 2 3) initial-env))
(display "\n")

;; Prueba: Multiplicación
(display "Prueba: Multiplicación\n")
(display (eval-exp '(* 2 5) initial-env))
(display "\n")

;; Prueba: Comparación menor que
(display "Prueba: Comparación menor que\n")
(display (eval-exp '(< 3 5) initial-env))
(display "\n")

;; Prueba: Condicional
(display "Prueba: Condicional\n")
(display (eval-exp '(if (< x y) x y) initial-env)) ;; Debería devolver 10
(display "\n")

;; Prueba: Ciclo for
(display "Prueba: Ciclo for\n")
(eval-exp '(for i 1 3 (display i)) initial-env)
(display "\n")

;; Prueba: Definición y aplicación de procedimientos
(display "Prueba: Definición y aplicación de procedimientos\n")
(define extended-env (eval-exp '(define my-proc (proc (a b) (+ a b))) initial-env))
(display (eval-exp '(apply my-proc (3 4)) extended-env)) ;; Debería devolver 7
(display "\n")

;; Prueba: Manejo de objetos
(display "Prueba: Manejo de objetos\n")

(define obj (eval-exp '(object (x . 10) (y . 20) (add . (proc (a b) (+ a b)))) empty-env))

;; Acceso a campos
(display "Acceso a campos (x): ")
(display (obj 'get 'x)) ;; Debería devolver 10
(display "\n")

;; Actualización de campos
(display "Actualización de campos (x): ")
(obj 'update 'x 64)
(display (obj 'get 'x)) ;; Debería devolver 64
(display "\n")

;; Invocación de métodos
(display "Invocación de métodos (add): ")
(display (obj 'send 'add 3 4)) ;; Debería devolver 7
(display "\n")

(display "Fin de las pruebas desde archivo externo")
(display "\n")
