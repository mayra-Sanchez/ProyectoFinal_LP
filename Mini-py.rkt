#lang eopl
; Mayra Alejandra Sanchez - 2040506
; Laura Daniela Jaimes - 2040430
; Maria Paula Giraldo - 2022411
; Santiago Casañas Tabares - 2025301
; Jesus Adrian Peña - 2025513

; URL github: https://github.com/mayra-Sanchez/interpretador.git

;----------------------------------- INTERPRETADOR ----------------------------------
; Especificacion lexica
(define especificacion-lexica
  '((espacio-blanco (whitespace) skip)
    (comentario ("#" (arbno (or digit letter #\newline whitespace))) skip)  ;Los comentarios inician con #
    (letter("'" letter "'") symbol)
    (identificador ("$" letter (arbno (or letter digit))) symbol) ;Con letter se identifica que puede iniciar con mayuscula o minuscula 
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (texto (letter (arbno (or letter ":" "?" "=" "'" "&" "." "," ";" "*" "!" "¡" "¿" "-" "_"))) string)))

; Gramatica
(define gramatica
'(
  ;programa
  (programa (expresion) un-programa)

  ;Cuerpo
  (cuerpo (expresion (arbno expresion)) cuerpoc)

  ;Expresiones

  ;Datos
  (expresion (numero) numero-lit)
  (expresion ("\"" texto "\"") texto-lit)
  (expression (expr-bool) exp-bool)
  (expression ("[]") vacio-exp)
  (expression ("x16" "(" (arbno numero) ")" ) hexa-exp)
  (bool ("true") true-exp)
  (bool ("false") false-exp)

  ;Identificador
  (expresion (identificador) var-exp)

  ; Definiciones
  (expresion ("var" (arbno identificador "=" (expresion))"," "in" expresion ";") varLet-exp) ;Hacer el manejo de los valores mutables
  (expresion ("const" (arbno identificador "=" (expresion))"," "in" expresion ";")const-exp)
  (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) rec-exp)

  ;Constructores de datos predefinidos:
  (expresion ("[" (separated-list expresion ";") "]") list-exp)
  (expresion ("tupla" "[" (separated-list expresion ";") "]") tupla-exp) ;preguntar
  (expresion ("{" (identificador "=" expresion (arbno ";" identificador "=" expresion)) "}") registro-exp) ;preguntar
  (expr-bool (pred-prim "(" expresion "," expresion ")") pred-exp)
  (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") oper-bin-exp)
  (expr-bool (bool) bool-exp)
  (expr-bool (oper-un-bool "(" expr-bool ")" ) oper-un-exp)
  
   ;Estructura de control
  (expression ("begin" expresion (arbno ";" expression) "end") begin-exp)
  (expresion ("if" expr-bool "then" expresion ("[" "else" expresion "]")"end") condicional-exp)
  (expresion ("while" expr-bool "do" expresion "done") while-exp)
  (expresion ("for" identificador "=" expresion (or "to" "downto") expresion "do" expresion "done") for-exp)
  (expression ("set" identificador "=" expresion) set-exp)

  ;Otros
  (expresion ("def" "(" (separated-list identificador ",") ")" "{" expresion "}") def-exp) ;Crear procedimiento
  (expresion ("eval" expresion "(" (separated-list expresion ",") ")") app-exp) ;invocar procedimientos
  (expresion ("(" expresion primitiva-bin expresion ")") primapp-bin-exp)
  (expresion (primitiva-un "(" expresion ")") primapp-un-exp)
  (expresion ("def-rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) defrec-exp)
  
 
;--------------------------------------Enteros------------------------------------------------------------------------------
 ; Primitiva binaria 
  (primitiva-bin-enteros ("+") primitiva-suma)
  (primitiva-bin-enteros ("-") primitiva-resta)
  (primitiva-bin-enteros ("/") primitiva-div)
  (primitiva-bin-enteros ("*") primitiva-multi)
  (primitiva-bin-enteros ("%") primitiva-mod)

 ; Primitiva unaria
  (primitiva-un-enteros ("add1") primitiva-add1)
  (primitiva-un-enteros ("sub1") primitiva-sub1))

;--------------------------------Hexadecimales----------------------------------------------------------------------------------
; Primitiva binaria 
  (primitiva-bin-hexa ("+_hexa") primitiva-suma-hex)
  (primitiva-bin-hexa ("-_hexa") primitiva-resta-hex)
  (primitiva-bin-hexa ("*_hexa") primitiva-multi-hex)

; primitiva unaria
  (primitiva-un-hexa ("add1_hexa") primitiva-add1-hex)
  (primitiva-un-hexa ("sub1_hexa") primitiva-sub1-hex)

;------------------------------------- Operadores booleanos --------------------------------------------------
  (pred-prim ("<") menor)
  (pred-prim (">") mayor)
  (pred-prim ("<=") menor_igual)
  (pred-prim (">=") mayor_igual)
  (pred-prim ("==") igual)
  (pred-prim ("<>") noIgual)

  (oper-bin-bool ("and") and-op)
  (oper-bin-bool ("or") or-op)

  (oper-un-bool ("not") not)

 ;---------------------------------------Cadenas------------------------------------------------------------------------------------
 (primitiva-bin ("concat") primitiva-concat)
 (primitiva-un ("longitud") primitiva-longitud)

;--------------------------------------- Primitivas generales ------------------------------------------------
  (primitiva-un ("vacio?") primitiva-vacio?)
  (primitiva-un ("vacio") primitiva-vacio)
  (primitiva-un ("cabeza") primitiva-cabeza)
  (primitiva-un ("cola") primitiva-cola)
  
;---------------------------------------Listas-----------------------------------------------------------------------------------
  (primitiva-un ("crear-lista") primitiva-crear-lista)
  (primitiva-un ("lista?") primitiva-lista)
  (primitiva-bin ("append") primitiva-append)
  (primitiva-un ("ref-list") primitiva-refList)
  (primitiva-bin ("set-list") primitiva-setList)
  
;------------------------------------Tuplas---------------------------------------------------------------------------------------
  (primitiva-un ("crear-tupla") primitiva-crear-tupla)
  (primitiva-un ("tupla?") primitiva-tupla?)
  (primitiva-bin ("ref-tuple") primitiva-refTuple)
  
;-----------------------------------Registros---------------------------------------------------------------------------------------
  (primitiva-un ("registros?") primitiva-registros?)
  (primitiva-un ("crear-resgistro") primitiva-crearRegistro)
  (primitiva-bin ("ref-resgistro") primitiva-refRegistro)
  (primitiva-bin ("set-registro") primitiva-setRegistro))

;---------------------- interpretador ----------------------------
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

; 2)Defina un ambiente inicial con las variables (@a @b @c @d @e) con valores (1 2 3 "hola" "FLP") y modifique su función evaluar-expresión para que acepte dicho ambiente.
; -Diseñe una función llamada (buscar-variable) que recibe un símbolo (identificador) y un ambiente, retorna el valor si encuentra la variable en el ambiente. En caso contrario: "Error, la variable no existe"

; Evaluar-programa
;Es el procedimiento principal, toma un arbol de sintaxis abstracta y retorna un valor.
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (cuerpo)
                 (evaluar-expresion cuerpo (init-amb))))))

; Ambiente inicial
; Es una funcion cuyo dominio es un conjunto finito de variables y cuyo rango es el conjunto de todos los valores de Scheme, es usado usado para asociar las variables con sus valores en la implementacion
; de un lenguaje de programacion.
(define init-amb
  (lambda ()
    (extend-amb
     '($a $b $c $d $e)
     '(1 2 3 "hola" "FLP")
     (empty-amb))))

;evaluar-expresion: <expresion> <ambiente> -> numero
;evalua la expresión en el ambiente de entrada
;Esta funcion usa una expresion y un ambiente y retorna el valor de la expresion usando dicho ambiente para encontrar los valores de las variables
(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (numero-lit (datum) datum)
      
      (var-exp (id) (buscar-variable amb id))

      (texto-lit (texto) texto)
      
      (primapp-bin-exp (exp1 prim exp2)
                   (apply-prim-bin  exp1 prim exp2 amb))
      
      (condicional-exp (test-exp true-exp false-exp)
                        (if (valor-verdad? (evaluar-expresion test-exp amb))
                            (evaluar-expresion true-exp amb)
                            (evaluar-expresion false-exp amb)))

      (def-exp (ids cuerpo) (closure ids cuerpo amb))

      (primapp-un-exp (prim exp) (apply-prim-un prim exp amb))
      
      (app-exp (exp exps)
               (let ((proc (evaluar-expresion exp amb))
                     (args (eval-exps exps amb)))

                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'evaluar-expresion
                                 "no es un procedimiento" proc))))
      
      (defrec-exp (proc-names idss bodies defrec-body)
                  (evaluar-expresion defrec-body
                                   (extend-amb-recursively proc-names idss bodies amb)))
      )))

;---------------------- HASTA AQUI SE MODIFICO ... LO DE ABAJO FALTA ACOMODARLO ... GRACIAS X SU ATENCION --------------------------------------

;funciones auxiliares para aplicar evaluar-expresion a cada elemento de una lista de operandos (expresiones)

;Esta funcion auxiliar toma una lista de expresiones y un ambiente y evalua cada exp usando eval-exp
(define eval-exps
  (lambda (exps amb)
    (map (lambda (x) (eval-exp x amb)) exps)))

;Esta funcion llama a evaluar-expresion con el ambiente actual para determinar los valores de las variables
(define eval-exp
  (lambda (exp amb)
    (evaluar-expresion exp amb)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
;Realiza la especificacion de aplicación de las primitivas binarias
;Empleada para la suma, resta, multiplicacion y division de los numeros definidos, de forma que su escritura coincida con la
;notacion infija, y la concatenacion de dos expresiones
(define apply-prim-bin
  (lambda (exp1 prim exp2 amb)
    (cases primitiva-bin prim
      (primitiva-suma () (+ (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-resta () (- (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-multi () (* (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-div () (/ (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-concat () (string-append (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb))))))

;apply-prim-un
;Realiza la especificacion de aplicación de las primitivas unarias
;Empleada para conocer la longitud de una expresion, ademas de sumar y restar una unidad a un numero definido
(define apply-prim-un
  (lambda (prim arg amb)
    (cases primitiva-un prim
      (primitiva-longitud () (string-length(evaluar-expresion arg amb)))
      (primitiva-add1 () (+ (evaluar-expresion arg amb ) 1))
      (primitiva-sub1 () (- (evaluar-expresion arg amb ) 1)))))   

;definición del tipo de dato ambiente
; Es una herramienta de Scheme para construir e implementar interfaces para tipos de dato ambiente (ambiente vacio, ambiente extendido y ambiente extendido recursivo)
(define-datatype ambiente ambiente?
  (empty-amb-record)
  (extended-amb-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (amb ambiente?))
  (recursively-extended-amb-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (amb ambiente?)))

(define scheme-value? (lambda (v) #t))

;empty-amb:  -> ambiente
;función que crea un ambiente vacío
(define empty-amb  
  (lambda ()
    (empty-amb-record)))       ;llamado al constructor de ambiente vacío 


;extend-amb: <list-of symbols> <list-of numbers> ambiente -> ambiente
;Recibe un ambiente y lo extiene con los nuevos syms y vals con el ambiente
(define extend-amb
  (lambda (syms vals amb)
    (extended-amb-record syms vals amb)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> ambiente -> ambiente
;función que crea un ambiente extendido para procedimientos recursivos
;Recibe el nombre del procedimiento, los idss, los bodies y el viejo ambiente y lo extiende haciendo llamado a recursively-extended-amb-record para poder hacer la extension de un ambiente recursivo
(define extend-amb-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-amb-record
     proc-names idss bodies old-env)))

;Realiza la busqueda de un símbolo en un ambiente
;Empleada en la evaluacion de una expresion, para encontrar una variable dada en un ambiente dado, si no se encuentra la variable, retorna un
;mensaje de error
(define buscar-variable
  (lambda (amb sym)
    (cases ambiente amb
      (empty-amb-record ()
                        (eopl:error 'apply-amb "Error, la variable no existe" sym))
      (extended-amb-record (syms vals amb)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable amb sym))))
      (recursively-extended-amb-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (closure (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      amb)
                                             (buscar-variable old-env sym)))))))

; valor-verdad?
; Esta funcion recibe un argumento y determina si corresponde al valor booelano falso (es igual a cero) o al valor booelano verdadero (cualquier otro valor).
(define valor-verdad?
  (lambda (x)
    (not (zero? x))))

;Procedimientos
;Este es un constructor de los procedimientos, los cuales sirven para asignarle los ids, cuerpo y el ambiente de los procedimientos que usamos en este lenguaje
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (cuerpo expresion?)
   (amb ambiente?)))

;-------------------------------------------Funciones Auxiliares-------------------------------------------

;funciones auxiliares para encontrar la posición de un símbolo en la lista de símbolos de un ambiente

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