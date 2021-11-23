(define inicial '())

(define final '())

(define (breadth-first bc)
    (display "Ingrese el estado inicial: ") (set! inicial (read))
    (display "Ingrese el estado   final: ") (set! final (read))
    (cond ((equal? inicial final) (display "El problema ya esta resuelto !!!") (newline) (breadth-first bc))
          (#T (buscar bc final (list (list inicial)) '()))))

(define (buscar bc fin grafobusq estexp)
    (cond ((null? grafobusq) (fracaso))
          ((pertenece fin (car grafobusq)) (exito grafobusq))
          (#t (buscar bc fin (append (cdr grafobusq) (expandir (car grafobusq) bc estexp)) 
		                         (if (pertenece (car (car grafobusq)) estexp) estexp (cons (car (car grafobusq)) estexp))))))

(define (expandir linea basecon estexp)
    (if (or (null? basecon) (pertenece (car linea) estexp))
        ()
	      (if (not (equal? ((eval (car basecon)) (car linea)) (car linea)))
	          (cons (cons ((eval (car basecon)) (car linea)) linea) (expandir linea (cdr basecon) estexp))
            (expandir linea (cdr basecon) estexp))))

(define (pertenece x lista)
    (cond ((null? lista) #f)
          ((equal? x (car lista)) #t)
          (else (pertenece x (cdr lista)))))

(define (fracaso)
    (display "No existe solucion") (newline) #t)

(define (exito grafobusq)
    (display "Exito !!!") (newline)
    (display "Prof ....... ") (display (- (length (car grafobusq)) 1)) (newline)
    (display "Solucion ... ") (display (reverse (car grafobusq))) (newline) #t)
