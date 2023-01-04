#lang eopl
; Mayra Alejandra Sanchez - 2040506
; Laura Daniela Jaimes - 2040430
; Maria Paula Giraldo - 2022411
; Santiago Casañas Tabares - 2025301
; Jesus Adrian Peña - 2025513

; URL github: https://github.com/mayra-Sanchez/ProyectoFinal_LP

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
  ;(expresion (expr-bool) exp-bool)
  ;(expresion ("[]") vacio-exp)
  ;(expresion ("x16" "(" (arbno numero) ")" ) hexa-exp)
  (expresion (variable) varLet-exp)
  (bool ("true") true-exp)
  (bool ("false") false-exp)

  ;Identificador
  (expresion (identificador) var-exp)

  ; Definiciones
  (variable ("var" (separated-list identificador "=" expresion ",") ";" "in"  expresion ) variable-def);Hacer el manejo de los valores mutables
  ;(expression ("const" (separated-list identifier "=" expression ",") ";" "in" "{" expression "}") const-exp)
  ;(expression ("rec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" "{" expression "}") rec-exp)
  ;(expresion ("const" (arbno identificador "=" (expresion))"," "in" expresion ";")const-exp)
  ;(expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) rec-exp)

  ;Constructores de datos predefinidos:
  ;(expresion ("[" (separated-list expresion ";") "]") list-exp)
  ;(expresion ("tupla" "[" (separated-list expresion ";") "]") tupla-exp) ;preguntar
  ;(expresion ("{" (identificador "=" expresion (arbno ";" identificador "=" expresion)) "}") registro-exp) ;preguntar
  ;(expr-bool ("comparar" "(" expresion pred-prim expresion ")") comparar-exp) ;cambio
  ;(expr-bool ("(" expr-bool oper-bin-bool expr-bool ")") oper-bin-exp);cambio
  ;(expr-bool (oper-un-bool "(" expr-bool ")") oper-un-exp);cambio
  ;(expr-bool (bool) bool-exp);cambio
  
   ;Estructura de control
  ;(expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
  (expresion ("if" expresion "then" expresion "[" "else" expresion "]" "end") condicional-exp)
  ;(expresion ("while" expr-bool "do" expresion "done") while-exp)
  ;(expresion ("for" identificador "=" expresion (or "to" "downto") expresion "do" expresion "done") for-exp)
  ;(expresion ("set" identificador "=" expresion) set-exp)

  ;Otros
  (expresion ("def" "(" (separated-list identificador ",") ")" "{" expresion "}") def-exp) ;Crear procedimiento
  (expresion ("eval" expresion "[" (separated-list expresion ",") "]") app-exp);invocar procedimientos
  (expresion ("(" expresion primitiva-bin-entero expresion ")") primapp-bin-exp)
  (expresion (primitiva-un-entero "(" expresion ")") primapp-un-exp)
  (expresion ("def-rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) defrec-exp)
  
 
;--------------------------------------Enteros------------------------------------------------------------------------------
 ; Primitiva binaria 
  (primitiva-bin-entero ("+") primitiva-suma)
  (primitiva-bin-entero ("-") primitiva-resta)
  (primitiva-bin-entero ("/") primitiva-div)
  (primitiva-bin-entero ("*") primitiva-multi)
  (primitiva-bin-entero ("%") primitiva-mod)

 ; Primitiva unaria
  (primitiva-un-entero ("++") primitiva-add1)
  (primitiva-un-entero ("--") primitiva-sub1)

;--------------------------------Hexadecimales----------------------------------------------------------------------------------
; Primitiva binaria 
 ; (primitiva-bin-hexa ("+_hexa") primitiva-suma-hex)
  ;(primitiva-bin-hexa ("-_hexa") primitiva-resta-hex)
  ;(primitiva-bin-hexa ("*_hexa") primitiva-multi-hex)

; primitiva unaria
  ;(primitiva-un-hexa ("++_hexa") primitiva-add1-hex)
  ;(primitiva-un-hexa ("--_hexa") primitiva-sub1-hex)

;------------------------------------- Operadores booleanos --------------------------------------------------
  (pred-prim ("<") menor-prim)
  (pred-prim (">") mayor-prim)
  (pred-prim ("<=") menor_igual-prim)
  (pred-prim (">=") mayor_igual-prim)
  (pred-prim ("==") igual-prim)
  (pred-prim ("<>") noIgual-prim)

  (oper-bin-bool ("and") and-op)
  (oper-bin-bool ("or") or-op)

  (oper-un-bool ("not") not-op)

 ;---------------------------------------Cadenas------------------------------------------------------------------------------------
; (primitiva-bin ("concat") primitiva-concat)
; (primitiva-un ("longitud") primitiva-longitud)

;--------------------------------------- Primitivas generales ------------------------------------------------
  ;(primitiva-un ("vacio?") primitiva-vacio?)
  ;(primitiva-un ("vacio") primitiva-vacio)
  ;(primitiva-un ("cabeza") primitiva-cabeza)
  ;(primitiva-un ("cola") primitiva-cola)
  
;---------------------------------------Listas-----------------------------------------------------------------------------------
  ;(primitiva-un ("crear-lista") primitiva-crear-lista)
  ;(primitiva-un ("lista?") primitiva-lista)
  ;(primitiva-bin ("append") primitiva-append)
  ;(primitiva-un ("ref-list") primitiva-refList)
  ;(primitiva-bin ("set-list") primitiva-setList)
  
;------------------------------------Tuplas---------------------------------------------------------------------------------------
  ;(primitiva-un ("crear-tupla") primitiva-crear-tupla)
  ;(primitiva-un ("tupla?") primitiva-tupla?)
  ;(primitiva-bin ("ref-tuple") primitiva-refTuple)
  
;-----------------------------------Registros---------------------------------------------------------------------------------------
  ;(primitiva-un ("registros?") primitiva-registros?)
  ;(primitiva-un ("crear-resgistro") primitiva-crearRegistro)
  ;(primitiva-bin ("ref-resgistro") primitiva-refRegistro)
  ;(primitiva-bin ("set-registro") primitiva-setRegistro)
))
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

;----------------------------------------------- EVALS --------------------------------------------------------------------------
; ----------------------------Evaluar-programa-------------------------------------
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (cuerpo)
                 (evaluar-expresion cuerpo (init-amb))))))

;----------------------------------------evaluar-expresion--------------------------------------------------
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

      ; VarLet-exp --------- MAY -----------
      (varLet-exp (variables) (evaluar-var variables amb))

      ;exp-bool  -------- MAY ----------
      ;(exp-bool (exp) (eval-bool exp amb))

;       ;while-exp -MPL-
;      (while-exp (expr-bool cuerpo)
;                 (evaluar-while-exp expr-bool cuerpo amb ))
;      
;      ;for-exp -MPL-
;      (for-exp (identificador numero-lit expr-bool cuerpo)
;               (evaluar-for-exp identificador numero-lit expr-bool cuerpo amb))
      )))

;------------------------------ evaluar-var ------------------------------------
(define evaluar-var
  (lambda (struct amb)
    (cases variable struct
      (variable-def (ids exps cuerpo)
               (let ((args (eval-var-exp-rands exps amb)))
                 (evaluar-expresion cuerpo (extend-amb ids args amb))))
      )
    )
  )
;---------------------------------------------------------------------------
; Eval-bool
;(define eval-bool
;  (lambda (exp amb )
;    (cases expr-bool amb 
;      [comparar-exp (a prim b) (primitiva-comparativa (if (expresion? exp) (evaluar-expresion a amb ) ) prim (eval-exp b amb ))]  
;      [oper-bin-exp (a op b) (apply-operate (eval-bool (evaluar-expresion a amb ) amb ) op (eval-bool (evaluar-expresion b amb ) amb ))]
;      [oper-un-exp (un prim) (apply-un-exp un (eval-bool prim amb ))]
;      [bool-exp (bool-prim) (cases bool bool-prim
;                              [true-exp () #t]
;                              [false-exp () #f])]  
;      [else #f]
;      )
;    ))

;; evaluar-while-exp
;(define evaluar-while-exp
;  (lambda (expr-bool cuerpo amb)
;      (let
;          ((condicion (evaluar-expresion (evaluar-expresion expr-bool) amb )))
;
;        (if condicion
;            (begin
;              (evaluar-expresion cuerpo amb)
;              (evaluar-while-exp expr-bool cuerpo amb ))
;            1))))

;funciones auxiliares para aplicar evaluar-expresion a cada elemento de una lista de operandos (expresiones)
;Esta funcion auxiliar toma una lista de expresiones y un ambiente y evalua cada exp usando eval-exp
(define eval-exps
  (lambda (exps amb)
    (map (lambda (x) (eval-exp x amb)) exps)))

;Esta funcion llama a evaluar-expresion con el ambiente actual para determinar los valores de las variables
(define eval-exp
  (lambda (exp amb)
    (cases expresion exp
      (varLet-exp (id)
               (indirect-target
                (let ((ref (buscar-variable amb id)))
                  (cases target (primitiva-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (evaluar-expresion exp amb))))))

(define eval-var-exp-rands
  (lambda (exps amb)
    (map (lambda (x) (eval-var-exp-rand x amb))
         exps)))

(define eval-var-exp-rand
  (lambda (exp amb)
    (direct-target (evaluar-expresion exp amb))))

; ---------------------------------------Definición tipos de datos referencia y blanco ---------------------------

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype referencia referencia?
  (a-ref (position integer?)
         (vec vector?)))

;--------------------------- Blancos y Referencias ---------------------------

(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (string? x) (not (boolean? x)) (symbol? x))))

(define ref-to-direct-target?
  (lambda (x)
    (and (referencia? x)
         (cases referencia x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitiva-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitiva-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Referencia ilegal: ~s" ref1)))))))

(define primitiva-deref
  (lambda (ref)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitiva-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitiva-setref! ref (direct-target expval)))))

(define primitiva-setref!
  (lambda (ref val)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))
;------------------------------------------------ Ambientes ---------------------------------------------------------------------

; Ambiente iniciales un ambiente vacio.

(define init-amb
  (lambda ()
     (empty-amb)))

; Definicion tipo de ambiente
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
;------------------------------------------------------ PRIMITIVAS ----------------------------------------------------------------
;-------------------------------------------------- PRIMITIVAS BOOLEANAS -----------------------------------------------------------------
(define apply-un-exp
  (lambda (un prim)
    (cases oper-un-bool un
      [not-op () (not prim)]
      )))

(define apply-operate
  (lambda (a op b)
    (cases oper-bin-bool op
      [and-op () (and a b)]
      [or-op () (or a b)])))

(define primitiva-comparativa
  (lambda (a prim b)
    (cases pred-prim prim
      [mayor-prim () (> a b)]
      [mayor_igual-prim () (>= a b)]
      [menor-prim () (< a b)]
      [menor_igual-prim () (<= a b)]
      [igual-prim () (= a b)]
      [noIgual-prim () (not (= a b))]
      )))

;--------------------------------------------------PRIMITIVAS ENTEROS---------------------------------------------------------------------
;apply-prim-un
;Realiza la especificacion de aplicación de las primitivas unarias
;Empleada para conocer la longitud de una expresion, ademas de sumar y restar una unidad a un numero definido
(define apply-prim-un
  (lambda (prim arg amb)
    (cases primitiva-un-entero prim
      ;(primitiva-longitud () (string-length(evaluar-expresion arg amb)))
      (primitiva-add1 () (+ (evaluar-expresion arg amb ) 1))
      (primitiva-sub1 () (- (evaluar-expresion arg amb ) 1)))))

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
      ;(primitiva-concat () (string-append (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      )))

;--------------------------------------------------- PROCEDIMIENTOS --------------------------------------------------------------------
;Procedimientos
;Este es un constructor de los procedimientos, los cuales sirven para asignarle los ids, cuerpo y el ambiente de los procedimientos que usamos en este lenguaje
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (cuerpo expresion?)
   (amb ambiente?)))

;<expresion> :=  "evaluar" expresion   (expresion ",")*  finEval
;app-exp(exp exps)
;Determina como aplicar un valor de tipo procedimiento
 (define apply-procedure
   (lambda (proc exps)
     (cases procval proc
      (closure (ids cuerpo amb)
               (evaluar-expresion cuerpo (extend-amb ids exps amb))))))
; ------------------------------------------------- FUNCIONES AUXILIARES -----------------------------------------------------------------
; valor-verdad?
; Esta funcion recibe un argumento y determina si corresponde al valor booelano falso (es igual a cero) o al valor booelano verdadero (cualquier otro valor).
(define valor-verdad?
  (lambda (x)
    (not (zero? x))))

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
;______________________________________________________________________________ P R U E B A S ___________________________________________________________________________________

;def-rec
;      $suma($a,$b) = if $b then eval $suma[++($a),--($b)] [else $a ]end
;in 
;   eval $suma[4,5]

;----------------------------------------------------------------------

;def-rec
;      $factorial($numero) = if $numero then ($numero * eval $factorial[--($numero)]) [else 1 ]end
;in 
;   eval $factorial[5]

;----------------------------------------------------------------------

;def-rec
;      $factorial($numero) = if $numero then ($numero * eval $factorial[--($numero)]) [else 1 ]end
;in 
;   eval $factorial[10]

;---------------------------------------------------------------------

;def-rec
;      $restar($a,$b) = if $b then eval $restar[--($a),--($b)] [ else $a] end      
;    in
; eval $restar[10,3]

;--------------------------------------------------------------------

;def-rec
;$suma($a,$b) = if $b then eval $suma[++($a),--($b)][else $a] end
;
;      $restar($a,$b) = if $b then eval $restar[--($a),--($b)] [ else $a] end
;
;$multiplicacion($a,$b) = if $b then eval $suma[$a, eval $multiplicacion[$a, --($b)]] [else eval $restar[$a,$a]] end
;in
;eval $multiplicacion[10,3]