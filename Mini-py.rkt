#lang eopl
; Mayra Alejandra Sanchez - 2040506
; Laura Daniela Jaimes - 2040430
; Maria Paula Giraldo - 2022411
; Santiago Casañas Tabares - 2025301
; Jesus Adrian Peña - 2025513

; URL github: https://github.com/mayra-Sanchez/ProyectoFinal_LP.git

; ----------------------------------------------------- Miny.py ---------------------------------------------

;Especificacion lexica, la cual refiere a la forma en la que el programa se divide en unidades lexicas 
;Empleada para proveer la informacion sobre el espacio blanco, especificacion de comentarios, identificador, numeros y texto
(define especificacion-lexica
  '((espacio-blanco (whitespace) skip)
    (comentario ("#" (arbno (or digit letter #\newline whitespace))) skip)  ;Los comentarios inician con #
    (letter("'" letter "'") symbol)
    (identificador ( letter (arbno (or letter digit))) symbol) ;Con letter se identifica que puede iniciar con mayuscula o minuscula 
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (bool (or "true" "false") boolean)
    (texto (letter (arbno (or letter ":" "?" "=" "'" "#" "$" "&" "." "," ";" "*" "!" "¡" "¿" "-" "_"))) string)))

; Gramatica
;Esta es la gramatica del lenguaje, la cual describe las reglas del mismo
;Empleada para definir los identificadores, las definiciones, los datos, los contructores de datos predefinidos y las estructuras de control
(define gramatica
'(
  ;programa
  (programa (expresion) un-programa)

  ;Cuerpo
  (cuerpo (expresion (arbno expresion)) cuerpoc)

  ;Expresiones
  
  ;Identificador
  (expresion (identificador) var-exp)

  ;Definiciones
  (expresion ("var" (arbno identificador "=" (expresion))"," "in" expresion ";") varLet-exp) ;Hacer el manejo de los valores mutables
  (expresion ("const" (arbno identificador "=" (expresion))"," "in" expresion ";")const-exp)
  (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) 
                rec-exp)
  ;Datos
  (expresion (numero) numero-lit)
  (expresion (" " "" texto " " "") cadena-lit)
  (expresion (bool) bool-lit)
  
  ;Constructores de datos predefinidos:

  (expresion ("[" (separated-list expresion ";") "]") list-exp)
  (expresion ("tupla" "[" (separated-list expresion ";") "]") tupla-exp) ;preguntar
  (expresion ("{" (identificador "=" expresion (arbno ";" identificador "=" expresion))) registro-exp) ;preguntar

  (expr-bool (pred-prim "(" expresion "," expresion ")") pred-exp)
  (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") oper-bin-exp)
  (expr-bool (bool) bool-exp)
  (expr-bool (oper-un-bool "(" expr-bool ")" ) oper-un-exp)
  
  
  ;Estructura de control
  (expression ("begin" expresion (arbno ";" expression) "end") begin-exp)
  (expresion ("if" expr-bool "then" expresion ("[" "else" expresion "]")"end") condicional-exp)
  (expresion ("while" expr-bool "do" expresion "done") while-exp)
  (expresion ("for" identificador "=" expresion (or "to" "downto") expresion "do" expresion "done") for-exp)

  ;Otros
  (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc") proc-exp)
  (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval") app-exp)
  (expresion ("(" expresion primitiva-bin expresion ")") primapp-bin-exp)
  (expresion (primitiva-un "(" expresion ")") primapp-un-exp)
  (expresion ("declarar" "(" (separated-list identificador "=" expresion ";") ")" "{" expresion "}" ) variableLocal-exp)

;--------------------------------------Enteros------------------------------------------------------------------------------
  ; Primitiva binaria 
  (primitiva-bin ("+") primitiva-suma)
  (primitiva-bin ("-") primitiva-resta)
  (primitiva-bin ("/") primitiva-div)
  (primitiva-bin ("*") primitiva-multi)
  (primitiva-bin ("%") primitiva-mod)

  ; Primitiva unaria
  (primitiva-un ("add1") primitiva-add1)
  (primitiva-un ("sub1") primitiva-sub1))

;--------------------------------Hexadecimales----------------------------------------------------------------------------------
; Primitiva binaria 
  (primitiva-bin ("+") primitiva-suma-hex)
  (primitiva-bin ("-") primitiva-resta-hex)
  (primitiva-bin ("*") primitiva-multi-hex)

; primitiva unaria
  (primitiva-un ("add1") primitiva-add1-hex)
  (primitiva-un ("sub1") primitiva-sub1-hex)

;------------------------------------- Operadores booleanos --------------------------------------------------
  (pred-prim ("<") menor)
  (pred-prim (">") mayor)
  (pred-prim ("<=") menor_igual)
  (pred-prim (">=") mayor_igual)
  (pred-prim ("==") igual)

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

;------------------------- para el interpretador --------------------------------------------------------------
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

  
