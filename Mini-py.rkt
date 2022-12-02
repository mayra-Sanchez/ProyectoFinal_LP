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
    (identificador ( letter (arbno (or letter digit))) symbol) ;Con letter se identifica que puede iniciar con mayuscula o minuscula 
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    ;(bool ())
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
  (expresion ("var" (arbno indentificador "=" (expresion))"," "in" expresion ";")varLet-exp) ;Hacer el manejo de los valores mutables
  (expresion ("const" (arbno indentificador "=" (expresion))"," "in" expresion ";")const-exp)
  (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) 
                rec-exp)
  ;Datos
  (expresion (numero) numero-lit)
  (expresion (" " "" texto " " "") cadena-lit)
  (expresion (bool) bool-lit)
  
  ;Constructores de datos predefinidos:

  ;**Faltan los constructores de datos predefinidos

  ;Estructura de control
  (expression ("begin" expresion (arbno ";" expression) "end")
                begin-exp)
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
  ;(primitiva-bin ("%") primitiva-modulo)

  ; Primitiva unaria
  (primitiva-un ("longitud") primitiva-longitud)
  (primitiva-un ("add1") primitiva-add1)
  (primitiva-un ("sub1") primitiva-sub1)))

;--------------------------------Hexadecimales----------------------------------------------------------------------------------
; Primitiva binaria 
  (primitiva-bin ("+") primitiva-suma-ex)
  (primitiva-bin ("-") primitiva-resta)
  (primitiva-bin ("*") primitiva-multi)

; primitiva unaria
  (primitiva-un ("add1") primitiva-add1)
  (primitiva-un ("sub1") primitiva-sub1)

;---------------------------------------Cadenas------------------------------------------------------------------------------------
 (primitiva-bin ("concat") primitiva-concat)
 (primitiva-un ("longitud") primitiva-longitud)

;---------------------------------------Listas-----------------------------------------------------------------------------------
  (primitiva-un ("vacio?") primitiva-vacio?)
  (primitiva-un ("vacio") primitiva-vacio)
  (primitiva-un ("crear") primitiva-crear)
  (primitiva-un ("cabeza") primitiva-cabeza)
  (primitiva-un ("cola") primitiva-cola)
  (primitiva-bi ("append") primitiva-append)
  (primitiva-un ("ref-list") primitiva-refList)
  (primitiva-bi ("set-list") primitiva-setList)

;-----------------------------------Listas/tuplas---------------------------------------------------------------------------------
  (primitiva-un ("vacio?") primitiva-vacio?)
  (primitiva-un ("vacio") primitiva-vacio)
  (primitiva-un ("crear") primitiva-crear)
  (primitiva-un ("cabeza") primitiva-cabeza)
  (primitiva-un ("cola") primitiva-cola)

;------------------------------------Tuplas---------------------------------------------------------------------------------------
  (primitiva-un ("tupla?") primitiva-tupla?)
  (primitiva-bi ("ref-tuple") primitiva-refTuple)

;-----------------------------------Registros---------------------------------------------------------------------------------------
  (primitiva-un ("registros?") primitiva-registros?)
  (primitiva-un ("crear-resgistro") primitiva-crearRegistro)
  (primitiva-bi ("ref-resgistro") primitiva-refRegistro)
  (primitiva-bi ("set-registro") primitiva-setRegistro)


  
