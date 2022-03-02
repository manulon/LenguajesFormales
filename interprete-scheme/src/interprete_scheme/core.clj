(ns interprete-scheme.core
  (:gen-class))

(require '[clojure.string :as st :refer [blank? starts-with? ends-with? lower-case]]
         '[clojure.java.io :refer [delete-file reader]]
         '[clojure.walk :refer [postwalk postwalk-replace]])

(defn spy
  ([x] (do (prn x) x))
  ([msg x] (do (print msg) (print ": ") (prn x) x))
)

; Funciones principales
(declare repl)
(declare evaluar)
(declare aplicar)

; Funciones secundarias de evaluar
(declare evaluar-if)
(declare evaluar-or)
(declare evaluar-cond)
(declare evaluar-eval)
(declare evaluar-exit)
(declare evaluar-load)
(declare evaluar-set!)
(declare evaluar-quote)
(declare evaluar-define)
(declare evaluar-lambda)
(declare evaluar-escalar)

; Funciones secundarias de aplicar
(declare aplicar-lambda)
(declare aplicar-funcion-primitiva)

; Funciones primitivas
(declare fnc-car)
(declare fnc-cdr)
(declare fnc-env)
(declare fnc-not)
(declare fnc-cons)
(declare fnc-list)
(declare fnc-list?)
(declare fnc-read)
(declare fnc-mayor)
(declare fnc-menor)
(declare fnc-null?)
(declare fnc-sumar)
(declare fnc-append)
(declare fnc-equal?)
(declare fnc-length)
(declare fnc-restar)
(declare fnc-display)
(declare fnc-newline)
(declare fnc-reverse)
(declare fnc-mayor-o-igual)

; Funciones auxiliares

(declare buscar)
(declare error?)
(declare igual?)
(declare imprimir)
(declare cargar-arch)
(declare revisar-fnc)
(declare revisar-lae)
(declare leer-entrada)
(declare leer-entrada-wrapper)
(declare actualizar-amb)
(declare restaurar-bool)
(declare generar-nombre-arch)
(declare nombre-arch-valido?)
(declare controlar-aridad-fnc)
(declare proteger-bool-en-str)
(declare verificar-parentesis)
(declare generar-mensaje-error)
(declare aplicar-lambda-simple)
(declare aplicar-lambda-multiple)
(declare evaluar-clausulas-de-cond)
(declare evaluar-secuencia-en-cond)


; REPL (read–eval–print loop).
; Aridad 0: Muestra mensaje de bienvenida y se llama recursivamente con el ambiente inicial.
; Aridad 1: Muestra > y lee una expresion y la evalua. El resultado es una lista con un valor y un ambiente. 
; Si la 2da. posicion del resultado es nil, devuelve 'Goodbye! (caso base de la recursividad).
; Si no, imprime la 1ra. pos. del resultado y se llama recursivamente con la 2da. pos. del resultado. 
(defn repl
  "Inicia el REPL de Scheme."
  ([]
   (println "Interprete de Scheme en Clojure")
   (println "Trabajo Practico de 75.14/95.48 - Lenguajes Formales 2021") (prn)
   (println "Inspirado en:")
   (println "  SCM version 5f2.")                        ; https://people.csail.mit.edu/jaffer/SCM.html
   (println "  Copyright (C) 1990-2006 Free Software Foundation.") (prn) (flush)
   (repl (list 'append 'append 'car 'car 'cdr 'cdr 'cond 'cond 'cons 'cons 'define 'define
               'display 'display 'env 'env 'equal? 'equal? 'eval 'eval 'exit 'exit
               'if 'if 'lambda 'lambda 'length 'length 'list 'list 'list? 'list? 'load 'load
               'newline 'newline 'nil (symbol "#f") 'not 'not 'null? 'null? 'or 'or 'quote 'quote
               'read 'read 'reverse 'reverse 'set! 'set! (symbol "#f") (symbol "#f")
               (symbol "#t") (symbol "#t") '+ '+ '- '- '< '< '> '> '>= '>=)))
  ([amb]
   (print "> ") (flush)
   (try
     (let [renglon (leer-entrada)]                       ; READ
          (if (= renglon "")
              (repl amb)
              (let [str-corregida (proteger-bool-en-str renglon),
                    cod-en-str (read-string str-corregida),
                    cod-corregido (restaurar-bool cod-en-str),
                    res (evaluar cod-corregido amb)]     ; EVAL
                    (if (nil? (second res))              ;   Si el ambiente del resultado es `nil`, es porque se ha evaluado (exit)
                        'Goodbye!                        ;   En tal caso, sale del REPL devolviendo Goodbye!.
                        (do (imprimir (first res))       ; PRINT
                            (repl (second res)))))))     ; LOOP (Se llama a si misma con el nuevo ambiente)
     (catch Exception e                                  ; PRINT (si se lanza una excepcion)
                   (imprimir (generar-mensaje-error :error (get (Throwable->map e) :cause)))
                   (repl amb)))))                        ; LOOP (Se llama a si misma con el ambiente intacto)


(defn evaluar
  "Evalua una expresion `expre` en un ambiente. Devuelve un lista con un valor resultante y un ambiente."
  [expre amb]
  (if (and (seq? expre) (or (empty? expre) (error? expre))) ; si `expre` es () o error, devolverla intacta
      (list expre amb)                                      ; de lo contrario, evaluarla
      (cond
        (not (seq? expre))             (evaluar-escalar expre amb)
        (igual? (first expre) 'define) (evaluar-define expre amb)
        (igual? (first expre) 'if) (evaluar-if expre amb)
        (igual? (first expre) 'or) (evaluar-or expre amb)
        (igual? (first expre) 'set!) (evaluar-set! expre amb)
        (igual? (first expre) 'cond) (evaluar-cond expre amb)
        (igual? (first expre) 'eval) (evaluar-eval expre amb)
        (igual? (first expre) 'exit) (evaluar-exit expre amb)
        (igual? (first expre) 'load) (evaluar-load expre amb)
        (igual? (first expre) 'quote) (evaluar-quote expre amb)
        (igual? (first expre) 'lambda) (evaluar-lambda expre amb)

         ;
         ;
         ;
         ; Si la expresion no es la aplicacion de una funcion (es una forma especial, una macro...) debe ser evaluada
         ; por una funcion de Clojure especifica debido a que puede ser necesario evitar la evaluacion de los argumentos
         ;
         ;
         ;

	    	  :else (let [res-eval-1 (evaluar (first expre) amb),
             						 res-eval-2 (reduce (fn [x y] (let [res-eval-3 (evaluar y (first x))] (cons (second res-eval-3) (concat (next x) (list (first res-eval-3)))))) (cons (list (second res-eval-1)) (next expre)))]
					              	(aplicar (first res-eval-1) (next res-eval-2) (first res-eval-2))))))


(defn aplicar
  "Aplica la funcion `fnc` a la lista de argumentos `lae` evaluados en el ambiente dado."
  ([fnc lae amb]
   (aplicar (revisar-fnc fnc) (revisar-lae lae) fnc lae amb))
  ([resu1 resu2 fnc lae amb]
   (cond
     (error? resu1) (list resu1 amb)
     (error? resu2) (list resu2 amb)
     (not (seq? fnc)) (list (aplicar-funcion-primitiva fnc lae amb) amb)
     :else (aplicar-lambda fnc lae amb))))


(defn aplicar-lambda
  "Aplica la funcion lambda `fnc` a `lae` (lista de argumentos evaluados)."
  [fnc lae amb]
  (cond
    (not= (count lae) (count (second fnc))) (list (generar-mensaje-error :wrong-number-args fnc) amb)
    (nil? (next (nnext fnc))) (aplicar-lambda-simple fnc lae amb)
    :else (aplicar-lambda-multiple fnc lae amb)))


(defn aplicar-lambda-simple
  "Evalua un lambda `fnc` con un cuerpo simple"
  [fnc lae amb]
  (let [lae-con-quotes (map #(if (or (number? %) (string? %) (and (seq? %) (igual? (first %) 'lambda)))
                                 %
                                 (list 'quote %)) lae),
        nuevos-pares (reduce concat (map list (second fnc) lae-con-quotes)),
        mapa (into (hash-map) (vec (map vec (partition 2 nuevos-pares)))),
        cuerpo (first (nnext fnc)),
        expre (if (and (seq? cuerpo) (seq? (first cuerpo)) (igual? (ffirst cuerpo) 'lambda))
                  (cons (first cuerpo) (postwalk-replace mapa (rest cuerpo)))
                  (postwalk-replace mapa cuerpo))]
        (evaluar expre amb)))


(defn aplicar-lambda-multiple
  "Evalua una funcion lambda `fnc` cuyo cuerpo contiene varias partes."
  [fnc lae amb]
  (aplicar (cons 'lambda (cons (second fnc) (next (nnext fnc))))
           lae
           (second (aplicar-lambda-simple fnc lae amb))))


(defn aplicar-funcion-primitiva
  "Aplica una funcion primitiva a una `lae` (lista de argumentos evaluados)."
  [fnc lae amb]
  (cond
    (= fnc '<)            (fnc-menor lae)
    (= fnc '>)            (fnc-mayor lae)
    (= fnc '>=)           (fnc-mayor-o-igual lae)
    (= fnc '+)            (fnc-sumar lae)
    (= fnc '-)            (fnc-restar lae)
    ;
    ;
    ; Si la funcion primitiva esta identificada por un simbolo, puede determinarse mas rapido que hacer con ella
    ;
    ;
    (igual? fnc 'append)  (fnc-append lae)
    (igual? fnc 'car)     (fnc-car lae)
    (igual? fnc 'cdr)     (fnc-cdr lae)
    (igual? fnc 'env)     (fnc-env lae amb)
    (igual? fnc 'not)     (fnc-not lae)
    (igual? fnc 'cons)    (fnc-cons lae)
    (igual? fnc 'list)    (fnc-list lae)
    (igual? fnc 'list?)   (fnc-list? lae)
    (igual? fnc 'read)    (fnc-read lae)
    (igual? fnc 'null?)   (fnc-null? lae)
    (igual? fnc 'equal?)  (fnc-equal? lae)
    (igual? fnc 'length)  (fnc-length lae)
    (igual? fnc 'display) (fnc-display lae)
    (igual? fnc 'newline) (fnc-newline lae)
    (igual? fnc 'reverse) (fnc-reverse lae)
    (igual? fnc 'max)     (fnc-max lae)

    ;
    ;
    ; Si la funcion primitiva esta identificada mediante una palabra reservada, debe ignorarse la distincion entre mayusculas y minusculas 
    ;
    ;

    :else (generar-mensaje-error :wrong-type-apply fnc)))


(defn fnc-car
  "Devuelve el primer elemento de una lista."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'car), arg1 (first lae)]
       (cond
         (error? ari) ari
         (or (not (seq? arg1)) (empty? arg1)) (generar-mensaje-error :wrong-type-arg1 'car arg1)
         :else (first arg1))))

(defn fnc-cdr
  "Devuelve una lista sin su 1ra. posicion."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'cdr), arg1 (first lae)]
       (cond
         (error? ari) ari
         (or (not (seq? arg1)) (empty? arg1)) (generar-mensaje-error :wrong-type-arg1 'cdr arg1)
         :else (rest arg1))))


(defn fnc-cons
  "Devuelve el resultado de insertar un elemento en la cabeza de una lista."
  [lae]
  (let [ari (controlar-aridad-fnc lae 2 'cons), arg1 (first lae), arg2 (second lae)]
       (cond
         (error? ari) ari
					   	(not (seq? arg2)) (generar-mensaje-error :only-proper-lists-implemented 'cons)
					   	:else (cons arg1 arg2))))


(defn fnc-display
  "Imprime un elemento por la termina/consola y devuelve #<unspecified>."
  [lae]
  (let [cant-args (count lae), arg1 (first lae)]
       (case cant-args
         1 (do (print arg1) (flush) (symbol "#<unspecified>"))
         2 (generar-mensaje-error :io-ports-not-implemented 'display)
         (generar-mensaje-error :wrong-number-args-prim-proc 'display))))


(defn fnc-env
  "Devuelve el ambiente."
  [lae amb]
  (let [ari (controlar-aridad-fnc lae 0 'env)]
       (if (error? ari)
           ari
           amb)))


(defn fnc-length
  "Devuelve la longitud de una lista."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'length), arg1 (first lae)]
       (cond
         (error? ari) ari
         (not (seq? arg1)) (generar-mensaje-error :wrong-type-arg1 'length arg1)
         :else (count arg1))))


(defn fnc-list
  "Devuelve una lista formada por los args."
  [lae]
  (if (< (count lae) 1)
      ()
      lae))


(defn fnc-list?
  "Devuelve #t si un elemento es una lista. Si no, #f."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'list?), arg1 (first lae)]
       (if (error? ari)
           ari
           (if (seq? arg1)
               (symbol "#t")
               (symbol "#f")))))


(defn fnc-newline
  "Imprime un salto de linea y devuelve #<unspecified>."
  [lae]
  (let [cant-args (count lae)]
       (case cant-args
         0 (do (newline) (flush) (symbol "#<unspecified>"))
         1 (generar-mensaje-error :io-ports-not-implemented 'newline)
         (generar-mensaje-error :wrong-number-args-prim-proc 'newline))))


(defn fnc-not
  "Niega el argumento."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'not)]
       (if (error? ari)
           ari
           (if (igual? (first lae) (symbol "#f"))
               (symbol "#t")
               (symbol "#f")))))


(defn fnc-null?
  "Devuelve #t si un elemento es ()."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'null?)]
       (if (error? ari)
           ari
          (if (= (first lae) ())
               (symbol "#t")
               (symbol "#f")))))


(defn fnc-reverse
  "Devuelve una lista con los elementos de `lae` en orden inverso."
  [lae]
    (let [ari (controlar-aridad-fnc lae 1 'reverse), arg1 (first lae)]
       (cond
         (error? ari) ari
         (not (seq? arg1)) (generar-mensaje-error :wrong-type-arg1 'reverse arg1)
         :else (reverse arg1))))


(defn controlar-aridad-fnc
  "Si la `lae` tiene la longitud esperada, se devuelve este valor (que es la aridad de la funcion).
   Si no, devuelve una lista con un mensaje de error."
  [lae val-esperado fnc]
  (if (= val-esperado (count lae))
      val-esperado
      (generar-mensaje-error :wrong-number-args-prim-proc fnc)))


(defn imprimir
  "Imprime, con salto de linea, atomos o listas en formato estandar (las cadenas
  con comillas) y devuelve su valor. Muestra errores sin parentesis."
  ([elem]
   (cond
     (= \space elem) elem    ; Si es \space no lo imprime pero si lo devuelve
     (and (seq? elem) (starts-with? (apply str elem) ";")) (imprimir elem elem)
     :else (do (prn elem) (flush) elem)))
  ([lis orig]
   (cond
     (nil? lis) (do (prn) (flush) orig)
     :else (do (pr (first lis))
               (print " ")
               (imprimir (next lis) orig)))))


(defn revisar-fnc
  "Si la `lis` representa un error lo devuelve; si no, devuelve nil."
  [lis] (if (error? lis) lis nil))


(defn revisar-lae
  "Si la `lis` contiene alguna sublista que representa un error lo devuelve; si no, devuelve nil."
  [lis] (first (remove nil? (map revisar-fnc (filter seq? lis)))))


(defn evaluar-cond
  "Evalua una expresion `cond`."
  [expre amb]
  (if (= (count expre) 1) ; si es el operador solo
      (list (generar-mensaje-error :bad-or-missing 'cond expre) amb)
      (let [res (drop-while #(and (seq? %) (not (empty? %))) (next expre))]
            (if (empty? res) 
                (evaluar-clausulas-de-cond expre (next expre) amb)
                (list (generar-mensaje-error :bad-or-missing 'cond (first res)) amb)))))


(defn evaluar-clausulas-de-cond
  "Evalua las clausulas de cond."
  [expre lis amb]
  (if (nil? lis)
	     (list (symbol "#<unspecified>") amb) ; cuando ninguna fue distinta de #f
		    (let [res-eval (if (not (igual? (ffirst lis) 'else))
		                       (evaluar (ffirst lis) amb)
		                       (if (nil? (next lis))
		                           (list (symbol "#t") amb)
		                           (list (generar-mensaje-error :bad-else-clause 'cond expre) amb)))]
		         (cond
		           (error? (first res-eval)) res-eval
		           (igual? (first res-eval) (symbol "#f")) (recur expre (next lis) (second res-eval)) 
		           :else (evaluar-secuencia-en-cond (nfirst lis) (second res-eval))))))


(defn evaluar-secuencia-en-cond
  "Evalua secuencialmente las sublistas de `lis`. Devuelve el valor de la ultima evaluacion."
  [lis amb]
	  (if (nil? (next lis))
	      (evaluar (first lis) amb)
	      (let [res-eval (evaluar (first lis) amb)]
	           (if (error? (first res-eval))
   		           res-eval
  	             (recur (next lis) (second res-eval))))))


(defn evaluar-eval
  "Evalua una expresion `eval`."
  [expre amb]
  (if (not= (count expre) 2) ; si no son el operador y exactamente 1 argumento
      (list (generar-mensaje-error :wrong-number-args (symbol "#<CLOSURE <anon> ...")) amb)
      (let [arg (second expre)]
           (if (and (seq? arg) (igual? (first arg) 'quote))
               (evaluar (second arg) amb)
               (evaluar arg amb)))))


(defn evaluar-exit
  "Sale del interprete de Scheme."
  [expre amb]
  (if (> (count expre) 2) ; si son el operador y mas de 1 argumento
      (list (generar-mensaje-error :wrong-number-args-prim-proc 'quit) amb)
      (list nil nil)))


(defn evaluar-lambda
  "Evalua una expresion `lambda`."
  [expre amb]
  (cond
    (< (count expre) 3) ; si son el operador solo o con 1 unico argumento
          (list (generar-mensaje-error :bad-body 'lambda (rest expre)) amb)
    (not (seq? (second expre)))
          (list (generar-mensaje-error :bad-params 'lambda expre) amb)
    :else (list expre amb)))


(defn evaluar-load
  "Evalua una expresion `load`. Carga en el ambiente un archivo `expre` de codigo en Scheme."
  [expre amb]
  (if (= (count expre) 1) ; si es el operador solo
      (list (generar-mensaje-error :wrong-number-args (symbol "#<CLOSURE scm:load ...")) amb)
      (list (symbol "#<unspecified>") (cargar-arch amb (second expre)))))


(defn cargar-arch
  "Carga y devuelve el contenido de un archivo."
  ([amb arch]
   (let [res (evaluar arch amb),
         nom-original (first res),
         nuevo-amb (second res)]
         (if (error? nom-original)
             (do (imprimir nom-original) nuevo-amb)                 ; Mostrar el error
             (let [nom-a-usar (generar-nombre-arch nom-original)]
                   (if (error? nom-a-usar)
                       (do (imprimir nom-a-usar) nuevo-amb)          ; Mostrar el error
                       (let [tmp (try
                                    (slurp nom-a-usar)
                                    (catch java.io.FileNotFoundException _
                                      (generar-mensaje-error :file-not-found)))]
                            (if (error? tmp)
                                (do (imprimir tmp) nuevo-amb)        ; Mostrar el error
                                (do (spit "scm-temp" (proteger-bool-en-str tmp))
                                    (let [ret (with-open [in (java.io.PushbackReader. (reader "scm-temp"))]
                                                (binding [*read-eval* false]
                                                  (try
                                                    (imprimir (list (symbol ";loading") (symbol nom-original)))
                                                    (cargar-arch (second (evaluar (restaurar-bool (read in)) nuevo-amb)) in nom-original nom-a-usar)
                                                    (catch Exception e
                                                       (imprimir (generar-mensaje-error :end-of-file 'list))))))]
                                          (do (delete-file "scm-temp" true) ret))))))))))
  ([amb in nom-orig nom-usado]
   (try
     (cargar-arch (second (evaluar (restaurar-bool (read in)) amb)) in nom-orig nom-usado)
     (catch Exception _
       (imprimir (list (symbol ";done loading") (symbol nom-usado)))
       amb))))


(defn generar-nombre-arch
  "Dada una entrada la convierte en un nombre de archivo .scm valido."
  [nom]
  (if (not (string? nom))
      (generar-mensaje-error :wrong-type-arg1 'string-length nom)
      (let [n (lower-case nom)]
            (if (nombre-arch-valido? n)
                n
                (str n ".scm")))))    ; Agrega '.scm' al final


(defn nombre-arch-valido?
  "Chequea que el string sea un nombre de archivo .scm valido."
  [nombre] (and (> (count nombre) 4) (ends-with? nombre ".scm")))


(defn evaluar-quote
  "Evalua una expresion `quote`."
  [expre amb]
  (if (not= (count expre) 2) ; si no son el operador y exactamente 1 argumento
      (list (generar-mensaje-error :missing-or-extra 'quote expre) amb)
      (list (second expre) amb)))


(defn generar-mensaje-error
  "Devuelve un mensaje de error expresado como lista."
  ([cod]
 			(case cod 
         :file-not-found (list (symbol ";ERROR:") 'No 'such 'file 'or 'directory)
         :warning-paren (list (symbol ";WARNING:") 'unexpected (symbol "\")\"#<input-port 0>"))
         ()))
  ([cod fnc]
    (cons (symbol ";ERROR:")
    			(case cod
         :end-of-file (list (symbol (str fnc ":")) 'end 'of 'file)
         :error (list (symbol (str fnc)))
         :io-ports-not-implemented (list (symbol (str fnc ":")) 'Use 'of 'I/O 'ports 'not 'implemented)
         :only-proper-lists-implemented (list (symbol (str fnc ":")) 'Only 'proper 'lists 'are 'implemented)
         :unbound-variable (list 'unbound (symbol "variable:") fnc)
         :wrong-number-args (list 'Wrong 'number 'of 'args 'given fnc)
         :wrong-number-args-oper (list (symbol (str fnc ":")) 'Wrong 'number 'of 'args 'given)
         :wrong-number-args-prim-proc (list 'Wrong 'number 'of 'args 'given (symbol "#<primitive-procedure") (symbol (str fnc '>)))
         :wrong-type-apply (list 'Wrong 'type 'to 'apply fnc)
         ())))
  ([cod fnc nom-arg]
    (cons (symbol ";ERROR:") (cons (symbol (str fnc ":"))
    			(case cod
     			 :bad-body (list 'bad 'body nom-arg)
     			 :bad-else-clause (list 'bad 'ELSE 'clause nom-arg)
      			:bad-or-missing (list 'bad 'or 'missing 'clauses nom-arg)
     			 :bad-params (list 'Parameters 'are 'implemented 'only 'as 'lists nom-arg)
      			:bad-variable (list 'bad 'variable nom-arg)
     			 :missing-or-extra (list 'missing 'or 'extra 'expression nom-arg)
     			 :wrong-type-arg (list 'Wrong 'type 'in 'arg nom-arg)
     			 :wrong-type-arg1 (list 'Wrong 'type 'in 'arg1 nom-arg)
     			 :wrong-type-arg2 (list 'Wrong 'type 'in 'arg2 nom-arg)
         ())))))

; FUNCIONES QUE DEBEN SER IMPLEMENTADAS PARA COMPLETAR EL INTERPRETE DE SCHEME (ADEMAS DE COMPLETAR `EVALUAR` Y `APLICAR-FUNCION-PRIMITIVA`):

; LEER-ENTRADA:
; user=> (leer-entrada)
; (hola
; mundo)
; "(hola mundo)"
; user=> (leer-entrada)
; 123
; "123"

(defn leer-entrada 
  ([] 
    (let [input(read-line)]
    (if (= 0 (verificar-parentesis input)) 
      input
      (leer-entrada input)
    )
    )
  )
  ([input-parcial]
    (let [input(read-line)]
    (if (= 0 (verificar-parentesis (str input-parcial input))) 
      (str input-parcial input)
      (leer-entrada input-parcial)
    )
    )
  )
) 

; user=> (verificar-parentesis "(hola 'mundo")
; 1
; user=> (verificar-parentesis "(hola '(mundo)))")
; -1
; user=> (verificar-parentesis "(hola '(mundo) () 6) 7)")
; -1
; user=> (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")
; -1
; user=> (verificar-parentesis "(hola '(mundo) )")
; 0

(defn valor-parentesis [ch]
  (
    cond ( = ch \( ) 1
    :else (cond ( = ch \) ) -1 :else 0)
  )  
)

(defn ver [ch, rest, cont]
  (if (neg? (+ cont (valor-parentesis ch)))
    -1
    (if (= "" rest) 
      (+ cont (valor-parentesis ch))
      (ver (first rest) (subs rest 1) (+ cont (valor-parentesis ch)))
    )
  )
)

(defn verificar-parentesis [input]
  (ver (first input) (subs input 1) 0)
)

; user=> (actualizar-amb '(a 1 b 2 c 3) 'd 4)
; (a 1 b 2 c 3 d 4)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b 4)
; (a 1 b 4 c 3)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))
; (a 1 b 2 c 3)
; user=> (actualizar-amb () 'b 7)
; (b 7)
(defn actualizar-amb [arg1, arg2, arg3]
  (if (= (error? arg3) true) 
    arg1
    (if (= (generar-mensaje-error :unbound-variable arg2) (buscar arg2 arg1))
        (concat arg1 (list (symbol (lower-case arg2))) (list arg3))
      (reduce
        concat
        (map (fn[x] (if (= (first x) (symbol (lower-case arg2))) (concat (list (symbol (lower-case arg2))) (list arg3)) x)) (partition 2 arg1))
      )
    )
  )
)

; user=> (buscar 'c '(a 1 b 2 c 3 d 4 e 5))
; 3
; user=> (buscar 'f '(a 1 b 2 c 3 d 4 e 5))
; (;ERROR: unbound variable: f)

(defn buscar [arg1, arg2]
  (let [resultado (remove false?
                    (map
                      (fn [x]
                        (if (= (first x) (symbol (lower-case arg1)))
                          (second x)
                          false
                        )
                      )   
                      (partition 2 arg2)
                      )
                    )
                  ]
    (if (= (first resultado) nil) 
      (generar-mensaje-error :unbound-variable arg1)
      (first resultado)
    )  
  )
)


;;user=> (error? (list (symbol ";ERROR:") 'mal 'hecho))
; true
; user=> (error? (list 'mal 'hecho))
; false
; user=> (error? (list (symbol ";WARNING:") 'mal 'hecho))
; true
(defn error? [lista]
  (if (not (seq? lista)) false
    (if  (= (first lista) (symbol ";ERROR:")) true 
      (if (= (first lista) (symbol ";WARNING:")) true false) 
    )
  )
)

; user=> (proteger-bool-en-str "(or #F #f #t #T)")
; "(or %F %f %t %T)"
; user=> (proteger-bool-en-str "(and (or #F #f #t #T) #T)")
; "(and (or %F %f %t %T) %T)"
; user=> (proteger-bool-en-str "")
; ""
(defn proteger-bool-en-str [input]
  (clojure.string/replace input #"#" "%")
)

; user=> (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))
; (and (or #F #f #t #T) #T)
; user=> (restaurar-bool (read-string "(and (or %F %f %t %T) %T)") )
; (and (or #F #f #t #T) #T)
(defn aplicar-modificacion [x]
  (cond
    (= x '%F) (symbol "#F")
    (= x '%T) (symbol "#T")
    (= x '%f) (symbol "#f")
    (= x '%t) (symbol "#t")
    :else x
  )
)

(defn restaurar-bool [arg]
  (map (fn [x] (
    cond
    (symbol? x) (aplicar-modificacion x)
    (seq? x) (restaurar-bool x)
    :else x
  )) arg)
)

; user=> (igual? 'if 'IF)
; true
; user=> (igual? 'if 'if)
; true
; user=> (igual? 'IF 'IF)
; true
; user=> (igual? 'IF "IF")
; false
; user=> (igual? 6 "6")
; false
(defn igual? [a, b]
  (cond
    (and (symbol? a) (symbol? b)) (= (lower-case a) (lower-case b))
    (and (number? a) (number? b)) (= a b)
    (and (string? a) (string? b)) (= a b)
    (and (seq? a) (seq? b)) (and (igual? (first a) (first b)) (igual? (rest a) (rest b)))
    :else false
  )
)

; user=> (fnc-append '( (1 2) (3) (4 5) (6 7)))
; (1 2 3 4 5 6 7)
; user=> (fnc-append '( (1 2) 3 (4 5) (6 7)))
; (;ERROR: append: Wrong type in arg 3)
; user=> (fnc-append '( (1 2) A (4 5) (6 7)))
; (;ERROR: append: Wrong type in arg A)
(defn verificar-tipo [arg]
  (map
    (fn [x] 
      (if (number? x) 1 0)
    )
    arg
  )
)

(defn obtener-error [arg]
  (remove false?
    (map
    (fn[a,b]
      (if (= 1 a) false b)
    )
    (map
      (fn [x] 
        (if (= true x) 1 0)
      )
      (map list? arg)
    )
    arg
    )
  )
)

(defn no-lista? [arg] 
  (not (seq? arg))
)

(defn fnc-append [arg]
  (if (= 0 (count (filter no-lista? arg))) 
    (reduce concat arg)
    (generar-mensaje-error :wrong-type-arg 'append (obtener-error arg))
  )
)


; user=> (fnc-equal? ())
; #t
; user=> (fnc-equal? '(A))
; #t
; user=> (fnc-equal? '(A a))
; #t
; user=> (fnc-equal? '(A a A))
; #t
; user=> (fnc-equal? '(A a A a))
; #t
; user=> (fnc-equal? '(A a A B))
; #f
; user=> (fnc-equal? '(1 1 1 1))
; #t
; user=> (fnc-equal? '(1 1 2 1))
; #f

(defn verificar-igualdad [arg]
  (let [primer-valor (first arg)]
  (map
    (fn [x] 
      (if (igual? (lower-case (str primer-valor)) (lower-case (str x))) 1 0)
    )
    arg
  )
  )
)

(defn fnc-equal? [arg]
  (if (= (count arg) (reduce + (verificar-igualdad arg))) (symbol "#t") (symbol "#f") )
)

; user=> (fnc-read ())
; (hola
; mundo)
; (hola mundo)
; user=> (fnc-read '(1))
; (;ERROR: read: Use of I/O ports not implemented)
; user=> (fnc-read '(1 2))
; (;ERROR: Wrong number of args given #<primitive-procedure read>)
; user=> (fnc-read '(1 2 3))
; (;ERROR: Wrong number of args given #<primitive-procedure read>)

(defn fnc-read [arg]
  (if (= (count arg) 0)
    (read-string (leer-entrada)) 
    (if (= (count arg) 1)
      (generar-mensaje-error :io-ports-not-implemented 'read)
      (generar-mensaje-error :wrong-number-args-prim-proc 'read)
    )
  )
)

; user=> (fnc-sumar ())
; 0
; user=> (fnc-sumar '(3))
; 3
; user=> (fnc-sumar '(3 4))
; 7
; user=> (fnc-sumar '(3 4 5))
; 12
; user=> (fnc-sumar '(3 4 5 6))
; 18
; user=> (fnc-sumar '(A 4 5 6))
; (;ERROR: +: Wrong type in arg1 A)
; user=> (fnc-sumar '(3 A 5 6))
; (;ERROR: +: Wrong type in arg2 A)
; user=> (fnc-sumar '(3 4 A 6))
; (;ERROR: +: Wrong type in arg2 A)

(defn obtener-argumento-error [arg]
  (first 
  (remove false?
    (map
    (fn[a,b]
      (if (= 1 a) false b)
    )
    (map
      (fn [x] 
        (if (= true x) 1 0)
      )
      (map number? arg)
    )
    arg
    )
  )
  )
)

(defn obtener-numero-error [arg]
  (if (= (obtener-argumento-error arg) (first arg))
    :wrong-type-arg1
    :wrong-type-arg2
  )
)
 
 (defn fnc-sumar [arg]
   (if (= (count arg) (reduce + (verificar-tipo arg)))
     (reduce + arg) 
     (generar-mensaje-error (obtener-numero-error arg) '+ (obtener-argumento-error arg))
   )  
 )


; user=> (fnc-restar ())
; (;ERROR: -: Wrong number of args given)
; user=> (fnc-restar '(3))
; -3
; user=> (fnc-restar '(3 4))
; -1
; user=> (fnc-restar '(3 4 5))
; -6
; user=> (fnc-restar '(3 4 5 6))
; -12
; user=> (fnc-restar '(A 4 5 6))
; (;ERROR: -: Wrong type in arg1 A)
; user=> (fnc-restar '(3 A 5 6))
; (;ERROR: -: Wrong type in arg2 A)
; user=> (fnc-restar '(3 4 A 6))
; (;ERROR: -: Wrong type in arg2 A)

(defn fnc-restar [arg]
  (if (= arg '()) (generar-mensaje-error (obtener-numero-error arg) '- (obtener-argumento-error arg))
    (if (= 1 (count arg)) (- 0 (first arg))
      (if (= (count arg) (reduce + (verificar-tipo arg)))
        (reduce - arg) 
        (generar-mensaje-error (obtener-numero-error arg) '- (obtener-argumento-error arg))
      )   
    )  
  )
)


; user=> (fnc-menor ())
; #t
; user=> (fnc-menor '(1))
; #t
; user=> (fnc-menor '(1 2))
; #t
; user=> (fnc-menor '(1 2 3))
; #t
; user=> (fnc-menor '(1 2 3 4))
; #t
; user=> (fnc-menor '(1 2 2 4))
; #f
; user=> (fnc-menor '(1 2 1 4))
; #f
; user=> (fnc-menor '(A 1 2 4))
; (;ERROR: <: Wrong type in arg1 A)
; user=> (fnc-menor '(1 A 1 4))
; (;ERROR: <: Wrong type in arg2 A)
; user=> (fnc-menor '(1 2 A 4))
; (;ERROR: <: Wrong type in arg2 A)

(defn fnc-menor [arg]
  (if (= (count arg) 0) (symbol "#t") 
    (if (= (count arg) (reduce + (verificar-tipo arg)))
      (let [lista-bool-menores (map (fn [x] (if (< (first x) (second x)) 1 0)) (partition 2 1 arg))]
            (if (= (count lista-bool-menores) (reduce + lista-bool-menores)) 
                (symbol "#t")                
                (symbol "#f")
            )
        )
      (generar-mensaje-error (obtener-numero-error arg) '< (obtener-argumento-error arg))
    )
  )
)


; user=> (fnc-mayor ())
; #t
; user=> (fnc-mayor '(1))
; #t
; user=> (fnc-mayor '(2 1))
; #t
; user=> (fnc-mayor '(3 2 1))
; #t
; user=> (fnc-mayor '(4 3 2 1))
; #t
; user=> (fnc-mayor '(4 2 2 1))
; #f
; user=> (fnc-mayor '(4 2 1 4))
; #f
; user=> (fnc-mayor '(A 3 2 1))
; (;ERROR: >: Wrong type in arg1 A)
; user=> (fnc-mayor '(3 A 2 1))
; (;ERROR: >: Wrong type in arg2 A)
; user=> (fnc-mayor '(3 2 A 1))
; (;ERROR: >: Wrong type in arg2 A)

(defn fnc-mayor [arg]
  (if (= 0 (count arg)) (symbol "#t")
    (if (= (count arg) (reduce + (verificar-tipo arg)))
        (let [lista-bool-mayores (map (fn [x] (if (> (first x) (second x)) 1 0)) (partition 2 1 arg))]
            (if (= (count lista-bool-mayores) (reduce + lista-bool-mayores)) 
                (symbol "#t")
                (symbol "#f")
            )
        )
        (generar-mensaje-error (obtener-numero-error arg) '> (obtener-argumento-error arg))
    )
  )
)

; user=> (fnc-mayor-o-igual ())
; #t
; user=> (fnc-mayor-o-igual '(1))
; #t
; user=> (fnc-mayor-o-igual '(2 1))
; #t
; user=> (fnc-mayor-o-igual '(3 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 3 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 2 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 2 1 4))
; #f
; user=> (fnc-mayor-o-igual '(A 3 2 1))
; (;ERROR: <: Wrong type in arg1 A)
; user=> (fnc-mayor-o-igual '(3 A 2 1))
; (;ERROR: <: Wrong type in arg2 A)
; user=> (fnc-mayor-o-igual '(3 2 A 1))
; (;ERROR: <: Wrong type in arg2 A)

(defn fnc-mayor-o-igual [arg]
  (if (= arg ()) (symbol "#t")
    (if (= (count arg) 1) (symbol "#t")
      (if (= (count arg) (reduce + (verificar-tipo arg)))
        (let [lista-bool-mayor-o-igual
              (map 
                (fn [x] 
                  (if (or
                        (> (first x) (second x))
                        (= (first x) (second x))
                      ) 
                      1 
                      0
                  )
                ) 
                (partition 2 1 arg)
              )]
            (if (= (count lista-bool-mayor-o-igual) (reduce + lista-bool-mayor-o-igual)) 
                (symbol "#t")
                (symbol "#f")
            )
        )
        (if (= 0 (.indexOf (map number? arg) false)) 
          (generar-mensaje-error :wrong-type-arg1 ">" (nth arg (.indexOf (map number? arg) false)))
          (generar-mensaje-error :wrong-type-arg2 ">" (nth arg (.indexOf (map number? arg) false)))
        )
      )
    )
  )
)

; user=> (evaluar-escalar 32 '(x 6 y 11 z "hola"))
; (32 (x 6 y 11 z "hola"))
; user=> (evaluar-escalar "hola" '(x 6 y 11 z "hola"))
; ("hola" (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'y '(x 6 y 11 z "hola"))
; (11 (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'z '(x 6 y 11 z "hola"))
; ("hola" (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'n '(x 6 y 11 z "hola"))
; ((;ERROR: unbound variable: n) (x 6 y 11 z "hola"))

(defn evaluar-escalar [arg1, arg2]
  (if (not (error? (buscar arg1 arg2)))
    (list (buscar arg1 arg2) arg2)
    (if (symbol? arg1)
      (list ((second (first (remove 
                              (fn[x] 
                                (not (= arg1 (second x)))
                              ) 
                              (partition 2 arg2))))))
      (list arg1 arg2)
    )
  )
)
  
; user=> (evaluar-define '(define x 2) '(x 1))
; (#<unspecified> (x 2))

; user=> (evaluar-define '(define (f x) (+ x 1)) '(x 1))
; (#<unspecified> (x 1 f (lambda (x) (+ x 1))))

; user=> (evaluar-define '(define) '(x 1))
; ((;ERROR: define: missing or extra expression (define)) (x 1))

; user=> (evaluar-define '(define x) '(x 1))
; ((;ERROR: define: missing or extra expression (define x)) (x 1))

; user=> (evaluar-define '(define x 2 3) '(x 1))
; ((;ERROR: define: missing or extra expression (define x 2 3)) (x 1))

; user=> (evaluar-define '(define ()) '(x 1))
; ((;ERROR: define: missing or extra expression (define ())) (x 1))

; user=> (evaluar-define '(define () 2) '(x 1))
; ((;ERROR: define: bad variable (define () 2)) (x 1))

; user=> (evaluar-define '(define 2 x) '(x 1))
; ((;ERROR: define: bad variable (define 2 x)) (x 1))
(defn evaluar-define [arg1, arg2]
  (cond
    (or (> 3 (count arg1)) (= 4 (count arg1))) 
      (list (generar-mensaje-error :missing-or-extra "define" arg1) arg2)
    (symbol? (second arg1)) 
      (list (symbol "#<unspecified>") (actualizar-amb arg2 (nth arg1 1) (first (evaluar (nth arg1 2) arg2))))
    (number? (second arg1))
      (list (generar-mensaje-error :bad-variable "define" arg1) arg2)
    (seq? (second arg1)) 
      (if (= 0 (count (second arg1)))
        (list (generar-mensaje-error :bad-variable "define" arg1) arg2)
        (if (< 3 (count arg1))
          (list 
            (symbol "#<unspecified>") 
            (actualizar-amb arg2 (first (nth arg1 1)) (cons 'lambda (conj (restaurar-bool (nthrest arg1 2)) (rest (nth arg1 1)))))
          )
          (list 
            (symbol "#<unspecified>")
            (actualizar-amb arg2 (first (nth arg1 1)) (list 'lambda (rest (nth arg1 1)) (restaurar-bool (nth arg1 2))) )
          )
        )
      )
  )
)

; user=> (evaluar-if '(if 1 2) '(n 7))
; (2 (n 7))
; user=> (evaluar-if '(if 1 n) '(n 7))
; (7 (n 7))
; user=> (evaluar-if '(if 1 n 8) '(n 7))
; (7 (n 7))
; user=> (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<unspecified> (n 7 #f #f))
; user=> (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f")))
; (8 (n 7 #f #f))
; user=> (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<unspecified> (n 9 #f #f))
; user=> (evaluar-if '(if) '(n 7))
; ((;ERROR: if: missing or extra expression (if)) (n 7))
; user=> (evaluar-if '(if 1) '(n 7))
; ((;ERROR: if: missing or extra expression (if 1)) (n 7))


(defn evaluar-condicion [arg1 arg2]
  (if (= (first (evaluar arg1 arg2)) (symbol "#f")) false true)
)

(defn evaluar-if [arg1 arg2]
  (cond 
    (not (or (= 3 (count arg1)) (= 4 (count arg1)))) 
      (list (generar-mensaje-error :missing-or-extra 'if arg1) arg2) 
    :else
      (if (evaluar-condicion (second arg1) arg2) 
        (evaluar (nth arg1 2) arg2)
        (cond 
          (= 4 (count arg1)) (evaluar (nth arg1 3) arg2)
          :else 
            (list (symbol "#<unspecified>") arg2)
        )
      )
  )
)


; user=> (evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#f (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#t (#f #f #t #t))
; user=> (evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (7 (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (5 (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#f (#f #f #t #t))

(defn evaluar-or [arg1, arg2]
  (cond 
    (=  (count arg1) 1)     (actualizar-amb '() (symbol "#f") arg2)
    (not (= (symbol "#f") (first (evaluar (second arg1) arg2)))) (list (first (evaluar (second arg1) arg2)) arg2)
    :else
      (evaluar-or (concat (list 'or) (rest (rest arg1))) arg2)
  )
)

; user=> (evaluar-set! '(set! x 1) '(x 0))
; (#<unspecified> (x 1))

; (evaluar-set! '(set! x 1) '())
; ((;ERROR: unbound variable: x) ())

; user=> (evaluar-set! '(set! x) '(x 0))
; ((;ERROR: set!: missing or extra expression (set! x)) (x 0))

; user=> (evaluar-set! '(set! x 1 2) '(x 0))
; ((;ERROR: set!: missing or extra expression (set! x 1 2)) (x 0))

; user=> (evaluar-set! '(set! 1 2) '(x 0))
; ((;ERROR: set!: bad variable 1) (x 0))
(defn evaluar-set! [arg1, arg2]
  (if (number? (second arg1))
    (actualizar-amb '() (generar-mensaje-error :bad-variable 'set! (second arg1)) arg2)
    (if (error? (buscar (second arg1) arg2))
      (actualizar-amb '() (buscar (second arg1) arg2) arg2)
      (if (not (= (count arg1) 3)) 
        (actualizar-amb '() (generar-mensaje-error :missing-or-extra 'set! arg1) arg2)
        (actualizar-amb '() (symbol "#<unspecified>") (actualizar-amb arg2 (second arg1) (first (evaluar (nth arg1 2) arg2))))
      )
    )
  )
)

; ////////////////////    PRACTICA PARA EL FINAL    ////////////////////

(defn obtener-maximo [arg]
  (let [lista-maximos 
        (map 
          (fn [x] (if (> (first x) (second x)) (first x) (second x)))
          (partition 2 1 arg)
        )
       ]
    (if (= 1 (count lista-maximos))
      lista-maximos
      (obtener-maximo lista-maximos)
    )
  )
)

(defn fnc-max [arg]
  (if (= 0 (count arg)) (generar-mensaje-error :wrong-number-args-prim-proc 'max)
    (if (= (count arg) (reduce + (verificar-tipo arg)))
        (first (obtener-maximo arg))
        (generar-mensaje-error (obtener-numero-error arg) 'max (obtener-argumento-error arg))
    )
  )
)

(defn fnc-sen [arg]
  (if (not (= 1 (count arg))) (generar-mensaje-error :wrong-number-args-prim-proc 'max)
    (if (number? (first arg))
        (Math/sin (first arg))
        (generar-mensaje-error (obtener-numero-error arg) 'sen (obtener-argumento-error arg))
    )
  )
)

(defn fnc-pow [arg]
  (if (not (= 2 (count arg))) (generar-mensaje-error :wrong-number-args-prim-proc 'max)
    (if (and (number? (first arg)) (number? (second arg)))
        (Math/pow (first arg) (second arg))
        (generar-mensaje-error (obtener-numero-error arg) 'pow (obtener-argumento-error arg))
    )
  )
)

(defn obtener-multiplicacion [arg, multiplicando]
  (let  [iteraciones-restantes (- (second arg) 1),
        suma-parcial (first arg)
        ]
    (if (= 0 iteraciones-restantes)
      suma-parcial
      (obtener-multiplicacion (list (+ suma-parcial multiplicando) iteraciones-restantes) multiplicando)
    )
  )
)

(defn fnc-multiplicacion [arg]
  (if (not (= 2 (count arg))) (generar-mensaje-error :wrong-number-args-prim-proc 'multiplicacion)
    (if (or (= 0 (second arg)) (= 0 (first arg)))
      0
        (if (= (count arg) (reduce + (verificar-tipo arg)))
            (obtener-multiplicacion arg (first arg))
            (generar-mensaje-error (obtener-numero-error arg) 'multiplicacion (obtener-argumento-error arg))
        )
    )
  )
)

; ////////////////////    PRACTICA PARA EL FINAL    ////////////////////

(defn -main
  "Interprete de Scheme"
  [& args]
  (repl)
)