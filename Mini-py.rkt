#lang eopl
; Mayra Alejandra Sanchez - 2040506
; Laura Daniela Jaimes - 2040430
; Maria Paula Giraldo - 2022411
; Santiago Casañas Tabares - 2025301
; Jesus Adrian Peña - 2025513

; URL github: https://github.com/mayra-Sanchez/ProyectoFinal_LP

;                                                               ----------------------------------- MINI PYTHON ----------------------------------

; ------------ Especificacion lexica --------------------------
(define especificacion-lexica
  '((espacio-blanco (whitespace) skip)
    (comentario ("#" (arbno (or digit letter #\newline whitespace))) skip)  
    (letter("'" letter "'") symbol)
    (identificador ("$" letter (arbno (or letter digit))) symbol) 
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (texto (letter (arbno (or letter ":" "?" "=" "'" "&" "." "," ";" "*" "!" "¡" "¿" "-" "_"))) string)))
;------------ Gramatica --------------------------------------
(define gramatica
'(
  ;programa
  (programa (expresion) un-programa)

  ;Cuerpo
  (cuerpo (expresion (arbno expresion)) cuerpoc)

  ;Expresiones
  (expresion (numero) numero-lit)
  (expresion ("\"" texto "\"") texto-lit)
  (expresion (identificador) id-exp)
  (expresion (boolean) expr-bool)
  (expresion ("(" expresion primitiva-bin-entero expresion ")") primapp-bin-exp)
  (expresion (primitiva-un-entero "(" expresion ")") primapp-un-exp)
  (expresion ("declarar" "(" (separated-list identificador "=" expresion ";") ")" "{" expresion "}" ) variableLocal-exp)
  (expresion ("if" expresion "then" expresion "[" "else" expresion "]" "end") condicional-exp)
  (expresion ("def" "(" (separated-list identificador ",") ")" "{" expresion "}") proc-exp)
  (expresion ("eval" expresion "[" (separated-list expresion ",") "]") app-exp)
  (expresion ("def-rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) defrec-exp)
  
  ; paso por valor y referencia 
  (expresion ("begin" expresion (arbno ";" expresion) "end")
                begin-exp)
  
  (expresion ("set" identificador "=" expresion)
                set-exp)

  ;Variables y constantes
  (expresion ("var" (arbno identificador "=" expresion)"," "in" expresion ";") var-exp) ;Hacer el manejo de los valores mutables
  (expresion ("const" (arbno identificador "=" expresion)"," "in" expresion ";")const-exp)

  ;Control para listas, tuplas y registro
  (expresion (prim-lista "(" (separated-list expresion ",") ")") lista-exp)
  (expresion ("set-lista(" expresion "," expresion "," expresion ")") set-list)
  (expresion ("ref-lista(" expresion "," expresion ")") ref-list)

  ;String
  (expresion (prim-string) string-exp)

  ;Booleanos
  (boolean (bool) trueFalse-exp)
  (expresion (pred-prim "(" expresion "," expresion ")") comparacion-exp)
  (boolean (oper-bin-bool "(" boolean "," boolean ")") op-log-exp)
  (boolean (oper-un-bool "(" boolean ")")  oper-un-bool-exp)

  ; True / False
  (bool ("True") true-exp)
  (bool ("False") false-exp)

  ;Primitiva booleana (comparacion)
  (pred-prim ("<") menor-bool)
  (pred-prim (">") mayor-bool)
  (pred-prim ("<=") menorIgual-bool)
  (pred-prim (">=") mayorIgual-bool)
  (pred-prim ("=") igual-bool)
  (pred-prim ("<>") noIgual-bool)

  ;Primitiva booleana (logicos)
  (oper-bin-bool ("and") and-bool)
  (oper-bin-bool ("or") or-bool)
  (oper-un-bool ("not") not-bool)

  ;Primitiva listas
  (prim-lista ("crear-lista") crea-list-prim)
  (prim-lista ("'") lista-prim)
  (prim-lista ("append") append-prim)
  (prim-lista ("vacio")  vacio-prim)
  (prim-lista ("cabeza")  car-prim)
  (prim-lista ("cola")  cdr-prim)
  (prim-lista ("vacio?") null?-prim)
  (prim-lista ("lista?") list?-prim)

 ;Primitiva binaria
  (primitiva-bin-entero ("+") primitiva-suma)
  (primitiva-bin-entero ("-") primitiva-resta)
  (primitiva-bin-entero ("/") primitiva-div)
  (primitiva-bin-entero ("*") primitiva-multi)
  (primitiva-bin-entero ("%") primitiva-mod)

  ; Primitiva unaria
  (primitiva-un-entero ("++") primitiva-add1)
  (primitiva-un-entero ("--") primitiva-sub1)

  ;Primitiva string
  (prim-string ("concat" "(" expresion "," expresion ")") concat-exp)
  (prim-string ("longitud" "(" expresion ")") longitud-exp)
  
))
;                                                               ----------------------------------- INTERPRETADOR ----------------------------------

; Tipos de datos para la sintaxis abstracta de la gramática construidos automáticamente:

(sllgen:make-define-datatypes especificacion-lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes especificacion-lexica gramatica)))

;Parser, Scanner, Interfaz
;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner especificacion-lexica gramatica))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (programa) (evaluar-programa  programa))
    (sllgen:make-stream-parser 
      especificacion-lexica
      gramatica)))
;                                                               ----------------------------------- EVALUADORES ----------------------------------
; --------------Evaluar-programa------------------------
;Es el procedimiento principal, toma un arbol de sintaxis abstracta y retorna un valor.
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (cuerpo)
                 (evaluar-expresion cuerpo (init-amb))))))
;------------- Evaluar-expresion -------------------------
;evaluar-expresion: <expresion> <ambiente> -> numero
;evalua la expresión en el ambiente de entrada
;Esta funcion usa una expresion y un ambiente y retorna el valor de la expresion usando dicho ambiente para encontrar los valores de las variables
(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (numero-lit (datum) datum)
      
      (id-exp (id) (apply-env amb id))

      (texto-lit (texto) texto)
      
      (primapp-bin-exp (exp1 prim exp2)
                   (apply-prim-bin  exp1 prim exp2 amb))

      (lista-exp (prim rands)
                 (let ((args (eval-rands-list rands amb)))
                   (apply-prim-list prim args)))
      
      (set-list (lista pos dato)
                (let ((lista (evaluar-expresion lista amb))
                      (pos (evaluar-expresion pos amb))
                      (dato (evaluar-expresion dato amb)))
                      (set-position-list lista pos dato)))
      
      (ref-list (lista pos)
                (let ((lista (evaluar-expresion lista amb)))
                  (get-position-list lista (evaluar-expresion pos amb))))
      
      (variableLocal-exp (ids exps cuerpo)
                         (let ((args (eval-let-exp-rands exps amb)))
                           (evaluar-expresion cuerpo
                           (extend-amb ids args amb))))
      
      (condicional-exp (test-exp true-exp false-exp)
                        (if (valor-verdad? (evaluar-expresion test-exp amb))
                            (evaluar-expresion true-exp amb)
                            (evaluar-expresion false-exp amb)))

      (proc-exp (ids cuerpo) (closure ids cuerpo amb))

      (primapp-un-exp (prim exp) (apply-prim-un prim exp amb))
      
      (app-exp (exp exps)
               (let ((proc (evaluar-expresion exp amb))
                     (args (eval-exps exps amb)))

                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'evaluar-expresion
                                 "no es un procedimiento" proc))))
      
      (defrec-exp (proc-names idss bodies letrec-body)
                  (evaluar-expresion letrec-body
                                   (extend-amb-recursively proc-names idss bodies amb)))
      
      (var-exp (ids rands body)
               (let ((args (eval-let-exp-rands rands amb)))
                 (evaluar-expresion body (extend-amb ids args amb))))
      
      (const-exp (ids rands body)
                 (begin
                   (eval-rands rands)
                   (cases expresion body
                     (set-exp (id exp) (eopl:error 'evaluar-expresion
                                 "No es posible modificar una constante" ))                     
                     (else (let ((args (eval-let-exp-rands rands amb)))
                         (evaluar-expresion body (extend-amb ids args amb))))
                   ))
               )
      
      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (apply-env-ref amb id)
                  (evaluar-expresion rhs-exp amb))
                 0))
      
      (begin-exp (exp exps)
                 (let loop ((acc (evaluar-expresion exp amb))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (evaluar-expresion (car exps) 
                                               amb)
                              (cdr exps)))))

     (expr-bool (bool) (eval-bool bool amb))

     (comparacion-exp ( prim exp1 exp2)
                      (apply-comparacion-exp prim exp1 exp2  amb))
     (string-exp (exp)
                 (cases prim-string exp
                   (concat-exp (exp1 exp2) (string-append (evaluar-expresion exp1 amb ) (evaluar-expresion exp2 amb )))
                   (longitud-exp (exp) (string-length (evaluar-expresion exp amb )))))

      )))
;-----------Eval-bool------------------
; Funcion eval-bool que evalua todos los tipos de booleanos del programa
(define eval-bool
  (lambda (expr-bool amb)
    (cases boolean expr-bool
      (trueFalse-exp (valor) (extr-bool-exp valor))
      (op-log-exp (op-log bool1 bool2)
                  (apply-oplog-exp op-log bool1 bool2 amb ))
      (oper-un-bool-exp (un prim)
                        (apply-un-exp un (eval-bool prim amb)))
      )
    )
  )
;                                                               ----------------------------------- LISTAS ----------------------------------
;funcion auxiliar para obtener elemento en una posicion de una lista
(define get-position-list
  (lambda (lista pos)
    (list-ref lista pos)))

;funcion auxiliar para cambiar elemento en una posicion de una lista
(define set-position-list
  (lambda (lista n x)
    (letrec
        (
         (new-list
          (lambda (listaaux pos dato count)
            (cond
              [(eqv? listaaux '()) empty]
              [(eqv? count pos) (cons dato (new-list (cdr listaaux) pos dato (+ 1 count)))]
              [else (cons (car listaaux) (new-list (cdr listaaux) pos dato (+ 1 count)))]
              )
            )
          )
         )
      (new-list lista n x 0))
    )
  )
;                                                               ----------------------------------- REFERENCIAS ----------------------------------

;Definición de tipos para trabajar referencias
(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (list? x))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;Blacos y referencias

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))

;                                                   ----------------------------------- FUNCIONES AUXILIARES EVALUADORAS ----------------------------------
(define eval-rands
  (lambda (rands)
    (cond
      [(null? rands) #true]
      [else
       (cases expresion (car rands)
                     (set-exp (id exp) (eopl:error 'evaluar-expresion
                                 "No es posible modificar una constante" ))
                     (else (eval-rands (cdr rands))))]
      )))

;funcion auxiliar para evaluar los rands de una lista
(define eval-rands-list
  (lambda (exps env)
    (map
      (lambda (exp) (evaluar-expresion exp env))
      exps)))

;Esta funcion auxiliar toma una lista de expresiones y un ambiente y evalua cada exp usando eval-exp
(define eval-exps
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;Esta funcion llama a evaluar-expresion con el ambiente actual para determinar los valores de las variables
(define eval-exp
  (lambda (rand env)
    (cases expresion rand
      (id-exp (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (evaluar-expresion rand env))))))

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (evaluar-expresion x env)) rands)))

(define eval-let-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let-exp-rand x env))
         rands)))

(define eval-let-exp-rand
  (lambda (rand env)
    (direct-target (evaluar-expresion rand env))))
;                                                             ----------------------------------- PRIMITIVAS ----------------------------------
;----------- Aritmeticas -----------
;apply-primitive: <primitiva> <list-of-expression> -> numero
;Realiza la especificacion de aplicación de las primitivas binarias
;Empleada para la suma, resta, multiplicacion y division de los numeros definidos, de forma que su escritura coincida con la
;notacion infija, y la concatenacion de dos expresiones
(define apply-prim-bin
  (lambda (exp1 prim exp2 amb)
    (cases primitiva-bin-entero prim
      (primitiva-suma () (+ (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-resta () (- (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-multi () (* (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-div () (/ (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-mod () (modulo (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      )))

;apply-prim-un
;Realiza la especificacion de aplicación de las primitivas unarias
;Empleada para conocer la longitud de una expresion, ademas de sumar y restar una unidad a un numero definido
(define apply-prim-un
  (lambda (prim arg amb)
    (cases primitiva-un-entero prim
      (primitiva-add1 () (+ (evaluar-expresion arg amb ) 1))
      (primitiva-sub1 () (- (evaluar-expresion arg amb ) 1)))))


;----------- Hexadecimales -----------


;----------- Booleanas -----------
; control de primitivas booleanas apply-comparacion-exp
(define apply-comparacion-exp
  (lambda (prim exp1 exp2 amb)
    (cases pred-prim prim
      (menor-bool () (<= (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (mayor-bool () (>= (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (menorIgual-bool () (< (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (mayorIgual-bool () (> (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (noIgual-bool () (not (= (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb))))
      (igual-bool () (= (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb))))))

; control de primitivas booleanas apply-oplog-exp
(define apply-oplog-exp
  (lambda (op-log bool1 bool2 amb)
    (cases oper-bin-bool op-log
      (and-bool () (and (eval-bool  bool1 amb) (eval-bool  bool2 amb)))
      (or-bool () (or (eval-bool  bool1 amb) (eval-bool  bool2 amb)))
      )))

;extr-bool-exp: <bool-vlue> -> booleano
(define extr-bool-exp
  (lambda (valor)
    (cases bool valor
      (true-exp () #t)
      (else #f)
      )
    )
  )

; apply-un-exp -> negación de una comparación
(define apply-un-exp
  (lambda (un prim)
    (cases oper-un-bool un
      [not-bool () (not prim)]
      )))

;----------- Listas -----------
;apply-prim-list
;Realiza la especificacion de aplicación de las primitivas para listas
;Empleada para crear una lista, crear lista vacia, saber si algo es una lista, cabeza, cola y saber si una lista es vacia
(define apply-prim-list
  (lambda (prims args)
    (cases prim-lista prims
      (crea-list-prim () args)               ;already a list
      (lista-prim () args)
      (vacio-prim () '())
      (car-prim () (car (car args)))
      (cdr-prim () (cdr (car args)))
      (append-prim () (cons (car args) (cadr args)))
      (null?-prim () (if (null? (car args)) 1 0))
      (list?-prim () (list? (car args)))
      )))
;                                                               ----------------------------------- AMBIENTES ----------------------------------
; ---------------Ambiente inicial-------------------------
; Es una funcion cuyo dominio es un conjunto finito de variables y cuyo rango es el conjunto de todos los valores de Scheme, es usado usado para asociar las variables con sus valores en la implementacion
; de un lenguaje de programacion.
;(define init-amb
;  (lambda ()
;    (extend-amb
;     '(@a @b @c @d @e)
;     '(1 2 3 "hola" "FLP")
;     (empty-amb))))
(define init-amb
  (lambda ()
    (extend-amb
     '(x y z)
     (list (direct-target 1)
           (direct-target 5)
           (direct-target 10))
     (empty-amb))))

;definición del tipo de dato ambiente
; Es una herramienta de Scheme para construir e implementar interfaces para tipos de dato ambiente (ambiente vacio, ambiente extendido y ambiente extendido recursivo)
(define-datatype ambiente ambiente?
  (empty-amb-record)
  (extended-amb-record
   (syms (list-of symbol?))
   (vec vector?)
   (env ambiente?)))

(define scheme-value? (lambda (v) #t))

;empty-amb:  -> ambiente
;función que crea un ambiente vacío
(define empty-amb  
  (lambda ()
    (empty-amb-record)))       ;llamado al constructor de ambiente vacío 


;extend-amb: <list-of symbols> <list-of numbers> ambiente -> ambiente
;Recibe un ambiente y lo extiene con los nuevos syms y vals con el ambiente
(define extend-amb
  (lambda (syms vals env)
    (extended-amb-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> ambiente -> ambiente
;función que crea un ambiente extendido para procedimientos recursivos
;Recibe el nombre del procedimiento, los idss, los bodies y el viejo ambiente y lo extiende haciendo llamado a recursively-extended-amb-record para poder hacer la extension de un ambiente recursivo
(define extend-amb-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-amb-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (closure ids body env))))
            (iota len) idss bodies)
          env)))))

;Realiza la busqueda de un símbolo en un ambiente
;Empleada en la evaluacion de una expresion, para encontrar una variable dada en un ambiente dado, si no se encuentra la variable, retorna un
;mensaje de error
;(define buscar-variable
;  (lambda (amb sym)
;    (cases ambiente amb
;      (empty-amb-record ()
;                        (eopl:error 'apply-amb "Error, la variable no existe" sym))
;      (extended-amb-record (syms vals amb)
;                           (let ((pos (list-find-position sym syms)))
;                             (if (number? pos)
;                                 (list-ref vals pos)
;                                 (buscar-variable amb sym))))
;      (recursively-extended-amb-record (proc-names idss bodies old-env)
;                                       (let ((pos (list-find-position sym proc-names)))
;                                         (if (number? pos)
;                                             (closure (list-ref idss pos)
;                                                      (list-ref bodies pos)
;                                                      amb)
;                                             (buscar-variable old-env sym)))))))

(define apply-env
  (lambda (env sym)
      (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases ambiente env
      (empty-amb-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-amb-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))



;Procedimientos
;Este es un constructor de los procedimientos, los cuales sirven para asignarle los ids, cuerpo y el ambiente de los procedimientos que usamos en este lenguaje
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (cuerpo expresion?)
   (amb ambiente?)))

;                                                             --------------------------------FUNCIONES AUXILIARES GENERALES ----------------------------

;funciones auxiliares para encontrar la posición de un símbolo en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

;Realiza una busqueda de la posicion de un simbolo
;Empleada para la búsqueda de una variable en funciones usadas en el lenguaje
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

;Realiza la busqueda del indice de un simbolo en una lista
;Empleada para encontrar la posicion del simbolo dado en una lista dada
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

; 7) Extienda la gramática para evaluar procedimientos:
;<expresion> :=  "evaluar" expresion   (expresion ",")*  finEval
;app-exp(exp exps)
;Determina como aplicar un valor de tipo procedimiento
 (define apply-procedure
   (lambda (proc exps)
     (cases procval proc
      (closure (ids cuerpo amb)
               (evaluar-expresion cuerpo (extend-amb ids exps amb))))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

; valor-verdad?
; Esta funcion recibe un argumento y determina si corresponde al valor booelano falso (es igual a cero) o al valor booelano verdadero (cualquier otro valor).
(define valor-verdad?
  (lambda (x)
    (not (zero? x))))


