(newline)
(display "*****************************************************") (newline)
(display "*                    SCHEME 2021                    *") (newline)
(display "* DEMO DE DEFINICION Y USO DE VARIABLES Y FUNCIONES *") (newline)
(display "*****************************************************") (newline)
(newline)
(display "OBS.: SCHEME NO DISTINGUE MAYUSCULAS DE MINUSCULAS.  ") (newline)
(display "      PARA APROVECHAR ESTA CARACTERISTICA, ALGUNAS   ") (newline)
(display "      VARIABLES Y FUNCIONES SE DEFINIERON A PROPOSITO") (newline)
(display "      EN MAYUSCULAS Y OTRAS EN MINUSCULAS.")            (newline)

(newline)
(display "DEFINICION DE VARIABLES") (newline)
(display "-----------------------") (newline)

(display "> (define u 'u)") (newline)
(display (define u 'u)) (newline)

(display "> (define v 'v)") (newline)
(display (define v 'v)) (newline)

(display "> (define w 'w)") (newline)
(display (define w 'w)) (newline)

(newline)
(display "LAS VARIABLES AHORA ESTAN EN EL AMBIENTE.") (newline)
(display "EVALUANDOLAS SE OBTIENEN SUS VALORES:") (newline)
(display "> u") (newline)
(display u) (newline)
(display "> v") (newline)
(display v) (newline)
(display "> w") (newline)
(display w) (newline)

(newline)
(display "UNA VEZ DEFINIDA UNA VARIABLE, CON SET! SE LE PUEDE") (newline)
(display "CAMBIAR EL VALOR:") (newline)

(display "> (define n 0)") (newline)
(display (define n 0)) (newline)

(display "> (set! N 17)") (newline)
(display (set! N 17)) (newline)

(display "> n") (newline)
(display n) (newline)

(newline)
(display "DEFINICION DE FUNCIONES") (newline)
(display "-----------------------") (newline)

(display "> (define (sumar a b) (+ a b))") (newline)
(display (define (sumar a b) (+ a b))) (newline)

(display "> (define (restar a b) (- a b))") (newline)
(display (define (restar a b) (- a b))) (newline)

(newline)
(display "LAS FUNCIONES AHORA ESTAN EN EL AMBIENTE.") (newline)
(display "ES POSIBLE APLICARLAS A VALORES FORMANDO EXPRESIONES") (newline)
(display "QUE EVALUADAS GENERAN RESULTADOS:") (newline)

(display "> (sumar 3 5)") (newline)
(display (sumar 3 5)) (newline)

(display "> (restar 12 5)") (newline)
(display (restar 12 5)) (newline)

(newline)
(display "SCHEME ES UN LENGUAJE DE AMBITO LEXICO (LEXICALLY SCOPED):") (newline)

(display "> (define x 1)") (newline)
(display (define x 1)) (newline)

(display "> (define (g y) (+ x y))") (newline)
(display (define (g y) (+ x y))) (newline)

(display "> (define (f x) (g 2))") (newline)
(display (define (f x) (g 2))) (newline)

(display "> (f 5)") (newline)
(display (f 5)) (newline)
(display "[En TLC-LISP -dynamically scoped- daria 7 en lugar de 3.]") (newline)

(newline)
(display "APLICACION DE FUNCIONES ANONIMAS [LAMBDAS]") (newline)
(display "------------------------------------------") (newline)

(newline)
(display "LAMBDA CON CUERPO SIMPLE:") (newline)
(display "> ((lambda (y) (+ 1 y)) 15)") (newline)
(display ((lambda (y) (+ 1 y)) 15)) (newline)

(newline)
(display "LAMBDA CON CUERPO MULTIPLE:") (newline)
(display "> ((lambda (y) (display \"Hola!\") (newline) (+ 1 y)) 5)") (newline)
(display ((lambda (y) (display "Hola!") (newline) (+ 1 y)) 5)) (newline)

(newline)
(display "LAMBDA CON CUERPO MULTIPLE Y EFECTOS COLATERALES [SIDE EFFECTS]:") (newline)
(display "> ((lambda (a b c) (set! u a) (set! v b) (set! w c)) 1 2 3)") (newline)
(display ((lambda (a b c) (set! u a) (set! v b) (set! w c)) 1 2 3)) (newline)
(newline)

(display "LOS NUEVOS VALORES DE LAS VARIABLES MODIFICADAS:") (newline)
(display "> u") (newline)
(display u) (newline)
(display "> v") (newline)
(display v) (newline)
(display "> w") (newline)
(display w) (newline)

(newline)
(display "APLICACION PARCIAL:") (newline)
(display "> (((lambda (x) (lambda (y) (- x y))) 8) 3)") (newline)
(display (((lambda (x) (lambda (y) (- x y))) 8) 3)) (newline)

(newline)
(display "EL MISMO EJEMPLO ANTERIOR, AHORA DEFINIENDO UNA FUNCION:") (newline)
(display "> (define p (lambda (x) (lambda (y) (- x y))))") (newline)
(display (define p (lambda (x) (lambda (y) (- x y))))) (newline)

(display "> (p 8)") (newline)
(display (p 8)) (newline)

(display "> ((p 8) 3)") (newline)
(display ((p 8) 3)) (newline)

(newline)
(display "DEFINICION DE FUNCIONES RECURSIVAS [RECORRIDO LINEAL]") (newline)
(display "-----------------------------------------------------") (newline)

(newline)
(display "FUNCION RECURSIVA CON EFECTO COLATERAL") (newline)
(display "[DEJA EN LA VARIABLE D LA CANTIDAD DE PARES]:") (newline)

(display "> (define (recorrer L)") (newline)
(display "    (recorrer2 L 0))") (newline)

(display
(define (recorrer L)
  (recorrer2 L 0))
) (newline)
  
(display "> (define D 0)") (newline)
(display (define D 0)) (newline)

(display "> (define (recorrer2 L i)") (newline)
(display "    (cond") (newline)
(display "      ((null? (cdr L)) (set! D (+ 1 D)) (list (car L) i))") (newline)
(display "      (#t (display (list (car L) i)) (set! D (+ i 1)) (newline) (recorrer2 (cdr L) D))))") (newline)

(display
(define (recorrer2 L i)
  (cond
    ((null? (cdr L)) (set! D (+ 1 D)) (list (car L) i))
    (#t (display (list (car L) i)) (set! D (+ i 1)) (newline) (recorrer2 (cdr L) D))))
) (newline)

(display "> (recorrer '(a b c d e f))") (newline)
(display (recorrer '(a b c d e f))) (newline)

(display "> d") (newline)
(display d) (newline)

(newline)
(display "DEFINICION DE FUNCIONES RECURSIVAS [RECORRIDO \"A TODO NIVEL\"]") (newline)
(display "-------------------------------------------------------------") (newline)

(newline)
(display "EXISTENCIA DE UN ELEMENTO ESCALAR EN UNA LISTA:") (newline)

(display "> (DEFINE (EXISTE? A L)") (newline)
(display "    (COND") (newline)
(display "      ((NULL? L) #F)") (newline)
(display "      ((NOT (LIST? (CAR L))) (OR (EQUAL? A (CAR L)) (EXISTE? A (CDR L))))") (newline)
(display "      (ELSE (OR (EXISTE? A (CAR L)) (EXISTE? A (CDR L))))))") (newline)

(display
(DEFINE (EXISTE? A L)
  (COND
    ((NULL? L) #F)
    ((NOT (LIST? (CAR L))) (OR (EQUAL? A (CAR L)) (EXISTE? A (CDR L))))
    (ELSE (OR (EXISTE? A (CAR L)) (EXISTE? A (CDR L))))))
) (newline)

(display "> (existe? 'c '(a ((b) ((d c) a) e f)))") (newline)
(display (existe? 'c '(a ((b) ((d c) a) e f)))) (newline)

(display "> (existe? 'g '(a ((b) ((d c) a) e f)))") (newline)
(display (existe? 'g '(a ((b) ((d c) a) e f)))) (newline)

(newline)
(display "ELIMINACION DE UN ELEMENTO DE UNA LISTA:") (newline)

(display "> (define (eliminar dat li)") (newline)
(display "    (cond") (newline)
(display "      ((null? li) li)") (newline)
(display "      ((equal? dat (car li)) (eliminar dat (cdr li)))") (newline)
(display "      ((list? (car li)) (cons (eliminar dat (car li)) (eliminar dat (cdr li))))") (newline)
(display "      (else (cons (car li) (eliminar dat (cdr li))))))") (newline)

(display
(define (eliminar dat li)
  (cond
    ((null? li) li)
    ((equal? dat (car li)) (eliminar dat (cdr li)))
    ((list? (car li)) (cons (eliminar dat (car li)) (eliminar dat (cdr li))))
    (else (cons (car li) (eliminar dat (cdr li))))))
) (newline)

(display "> (eliminar 'c '(a ((b) ((d c) a) c f)))") (newline)
(display (eliminar 'c '(a ((b) ((d c) a) c f)))) (newline)

(display "> (eliminar '(1 2 3) '(a ((b) (((1 2 3) c) a) c f)))") (newline)
(display (eliminar '(1 2 3) '(a ((b) (((1 2 3) c) a) c f)))) (newline)

(newline)
(display "PROFUNDIDAD DE UNA LISTA:") (newline)

(display "> (define (profundidad lista)") (newline)
(display "    (if (or (not (list? lista)) (null? lista)) 0") (newline)
(display "        (if (> (+ 1 (profundidad (car lista))) (profundidad (cdr lista)))") (newline)
(display "            (+ 1 (profundidad (car lista)))") (newline)
(display "            (profundidad (cdr lista)))))") (newline)

(display
(define (profundidad lista)
  (if (or (not (list? lista)) (null? lista)) 0
      (if (> (+ 1 (profundidad (car lista))) (profundidad (cdr lista)))
          (+ 1 (profundidad (car lista)))
          (profundidad (cdr lista)))))
) (newline)

(display "> (profundidad '((2 3)(3 ((7))) 5))") (newline)
(display (profundidad '((2 3)(3 ((7))) 5))) (newline)
(display "[El valor esperado es 4.]") (newline)

(newline)
(display "\"PLANCHADO\" DE UNA LISTA:") (newline)

(display "> (define (planchar li)") (newline)
(display "    (cond") (newline)
(display "      ((null? li) ())") (newline)
(display "      ((list? (car li)) (append (planchar (car li)) (planchar (cdr li))))") (newline)
(display "      (else (cons (car li) (planchar (cdr li))))))") (newline)

(display
(define (planchar li)
  (cond
    ((null? li) ())
    ((list? (car li)) (append (planchar (car li)) (planchar (cdr li))))
    (else (cons (car li) (planchar (cdr li))))))
) (newline)

(display "> (planchar '((2 3)(3 ((7))) 5))") (newline)
(display (planchar '((2 3)(3 ((7))) 5))) (newline)

(newline)
(display "DEFINICION DE FUNCIONES PARA \"OCULTAR\" LA RECURSIVIDAD EN LA PROGRAMACION FUNCIONAL") (newline)
(display "-----------------------------------------------------------------------------------") (newline)

(newline)
(display "FILTRAR [SELECCIONA DE UNA LISTA LOS ELEMENTOS QUE CUMPLAN CON UNA CONDICION DADA]:") (newline)

(display "> (DEFINE (FILTRAR F L)") (newline)
(display "    (COND") (newline)
(display "      ((NULL? L) ())") (newline)
(display "      ((F (CAR L)) (CONS (CAR L) (FILTRAR F (CDR L))))") (newline)
(display "      (ELSE (FILTRAR F (CDR L)))))") (newline)

(display
(DEFINE (FILTRAR F L)
  (COND
    ((NULL? L) ())
    ((F (CAR L)) (CONS (CAR L) (FILTRAR F (CDR L))))
    (ELSE (FILTRAR F (CDR L)))))
) (newline)

(display "> (filtrar (lambda (x) (> x 0)) '(5 0 2 -1 4 6 0 8))") (newline)
(display (filtrar (lambda (x) (> x 0)) '(5 0 2 -1 4 6 0 8))) (newline)

(newline)
(display "REDUCIR [REDUCE UNA LISTA APLICANDO DE A PARES UNA FUNCION DADA]:") (newline)

(display "> (DEFINE (REDUCIR F L)") (newline)
(display "    (IF (NULL? (CDR L))") (newline)
(display "        (CAR L)") (newline)
(display "        (F (CAR L) (REDUCIR F (CDR L)))))") (newline)

(display
(DEFINE (REDUCIR F L)
  (IF (NULL? (CDR L))
      (CAR L)
      (F (CAR L) (REDUCIR F (CDR L)))))
) (newline)

(display "> (reducir (lambda (x y) (if (> x 0) (cons x y) y)) '(5 0 2 -1 4 6 0 8 ()))") (newline)
(display (reducir (lambda (x y) (if (> x 0) (cons x y) y)) '(5 0 2 -1 4 6 0 8 ()))) (newline)

(newline)
(display "MAPEAR [APLICA A CADA ELEMENTO DE UNA LISTA UNA FUNCION DADA]:") (newline)

(display "> (DEFINE (MAPEAR OP L)") (newline)
(display "    (IF (NULL? L)") (newline)
(display "        ()") (newline)
(display "        (CONS (OP (CAR L)) (MAPEAR OP (CDR L)))))") (newline)

(display
(DEFINE (MAPEAR OP L)
  (IF (NULL? L)
      ()
      (CONS (OP (CAR L)) (MAPEAR OP (CDR L)))))
) (newline)

(display "> (mapear (lambda (x) (if (equal? x 0) 'Z x)) '(5 0 2 -1 4 6 0 8))") (newline)
(display (mapear (lambda (x) (if (equal? x 0) 'Z x)) '(5 0 2 -1 4 6 0 8))) (newline)

(newline)
(display "TRANSPONER [TRANSPONE UNA LISTA DE LISTAS]:") (newline)

(display "> (DEFINE (TRANSPONER M)") (newline)
(display "    (IF (NULL? (CAR M))") (newline)
(display "        ()") (newline)
(display "        (CONS (MAPEAR CAR M) (TRANSPONER (MAPEAR CDR M)))))") (newline)

(display
(DEFINE (TRANSPONER M)
  (IF (NULL? (CAR M))
    ()
    (CONS (MAPEAR CAR M) (TRANSPONER (MAPEAR CDR M)))))
) (newline)

(display "> (transponer '((a b c) (d e f) (g h i)))") (newline)
(display (transponer '((a b c) (d e f) (g h i)))) (newline)

(newline)
(display "IOTA [RETORNA UNA LISTA CON LOS PRIMEROS N NUMEROS NATURALES]:") (newline)

(display "> (DEFINE (IOTA N)") (newline)
(display "    (IF (< N 1)") (newline)
(display "         ()") (newline)
(display "         (AUXIOTA 1 N)))") (newline)

(display
(DEFINE (IOTA N)
    (IF (< N 1)
     ()
     (AUXIOTA 1 N)))
) (newline)

(display "> (DEFINE (AUXIOTA I N)") (newline)
(display "    (IF (EQUAL? I N)") (newline)
(display "        (LIST N)") (newline)
(display "        (CONS I (AUXIOTA (+ I 1) N))))") (newline)

(display
(DEFINE (AUXIOTA I N)
  (IF (EQUAL? I N)
    (LIST N)
    (CONS I (AUXIOTA (+ I 1) N))))
) (newline)

(display "> (IOTA 10)") (newline)
(display (IOTA 10)) (newline)

(newline)
(display "FUNCIONES IMPLEMENTADAS USANDO LAS FUNCIONES ANTERIORES") (newline)
(display "-------------------------------------------------------") (newline)

(newline)
(display "SUMATORIA DE LOS PRIMEROS N NUMEROS NATURALES:") (newline)

(display "> (define (sumatoria n) (reducir + (iota n)))") (newline)

(display
(define (sumatoria n) (reducir + (iota n)))
) (newline)

(display "> (sumatoria 100)") (newline)
(display (sumatoria 100)) (newline)
(display "[El valor esperado es 5050.]") (newline)

(newline)
(display "ELIMINACION DE LOS ELEMENTOS REPETIDOS EN UNA LISTA SIMPLE:") (newline)

(display "> (define (eliminar-repetidos li)") (newline)
(display "    (reverse (reducir (lambda (x y) (if (existe? x y) y (cons x y))) (reverse (cons () li)))))") (newline)

(display
(define (eliminar-repetidos li)
  (reverse (reducir (lambda (x y) (if (existe? x y) y (cons x y))) (reverse (cons () li)))))
) (newline)

(display "> (eliminar-repetidos '(a b c d e f g d c h b i j))") (newline)
(display (eliminar-repetidos '(a b c d e f g d c h b i j))) (newline)

(newline)
(display "SELECCION DEL ENESIMO ELEMENTO DE UNA LISTA DADA:") (newline)

(display "> (define (seleccionar n li)") (newline)
(display "    (if (or (< n 1) (> n (length li)))") (newline)
(display "        ()") (newline)
(display "        (car (car (filtrar (lambda (x) (equal? n (car (cdr x)))) (transponer (list li (iota (length li)))))))))") (newline)

(display
(define (seleccionar n li)
  (if (or (< n 1) (> n (length li)))
      ()
      (car (car (filtrar (lambda (x) (equal? n (car (cdr x)))) (transponer (list li (iota (length li)))))))))
) (newline)

(display "> (SELECCIONAR 5 '(A B C D E F G H I J))") (newline)
(display (SELECCIONAR 5 '(A B C D E F G H I J))) (newline)

(newline)
(display "APLICACION DE TODAS LAS FUNCIONES DE UNA LISTA A UN ELEMENTO DADO:") (newline)

(display "> (define (aplicar-todas lf x)") (newline)
(display "    (mapear (lambda (f) (f x)) lf))") (newline)

(display
(define (aplicar-todas lf x)
  (mapear (lambda (f) (f x)) lf))
) (newline)

(display "> (aplicar-todas (list length cdr car) '((3 2 1)(9 8)(7 6)(5 4)))") (newline)
(display (aplicar-todas (list length cdr car) '((3 2 1)(9 8)(7 6)(5 4)))) (newline)

(newline)
(display "ENTRADA DE DATOS Y SALIDA DEL INTERPRETE") (newline)
(display "----------------------------------------") (newline)

(newline)
(display "CARGA DE DATOS DESDE LA TERMINAL/CONSOLA:") (newline)
(display "> (define R 0)") (newline)
(display "> (define (cargarR)") (newline)
(display "    (display \"->R: \")(set! R (read))(display \"R*2: \")(display (+ R R))(newline))") (newline)
(display "> (cargarR)") (newline)

(define R 0)
(define (cargarR)
  (display "->R: ")(set! R (read))(display "R*2: ")(display (+ R R))(newline))
(cargarR)

(newline)
(display "PARA VER EL AMBIENTE [NO FUNCIONA EN SCM version 5f2]: (env)") (newline)
(display "PARA SALIR DEL INTERPRETE: (exit)") (newline)
