(ns tp.core-test
  (:require [clojure.test :refer :all]
            [tp.core :refer :all]))


(deftest  verificar-parentesis-test
  (testing "Menos cantidad de paréntesis abiertos"
    (is (= -1 (verificar-parentesis "(hola '(mundo)))"))))
  (testing "Misma cantidad de paréntesis abierto y cerrados"
    (is (= 0 (verificar-parentesis "(hola '(mundo) )"))))
  (testing "Más cantidad de paréntesis abiertos"
    (is (= 1 (verificar-parentesis "(hola 'mundo")))))


(deftest  actualizar-amb-test
  (testing "Actualizar con parámetros válidos"
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'd 4) '(a 1 b 2 c 3 d 4)))
    (is (= (actualizar-amb '(a 1 b 2) 'd 4) '(a 1 b 2 d 4)))
    (is (= (actualizar-amb () 'b 7) '(b 7))))

  (testing "Actualizar con parámeros inválidos"
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho)) '(a 1 b 2 c 3)))))


(deftest actualizar-valor-en-pos-test
      (testing "Actualizar valor en posición"
        (is (= (actualizar-valor-en-pos '(1 2 3) 0 3) '(3 2 3)))))


(deftest buscar-test
   (testing "Buscar cuando el elemento existe"
     (is (= (buscar 'c '(a 1 b 2 c 3 d 4 e 5)) 3)))
  (testing "Buscar cuando el elemento no existe"
    (is (= (buscar 'g '(a 1 b 2 c 3 d 4 e 5)) (generar-mensaje-error :unbound-variable 'g))))
)

(deftest error?-test
  (testing "Cuando el primer elmento es válido"
    (is (true? (error? (list (symbol ";ERROR:") 'mal 'hecho))))
    (is (true? (error? (list (symbol ";ERROR:") 'mal 'hecho))))
    )
  (testing "Cuando el primer elemento no es válido"
         (is (false? (error? (list 'mal 'hecho))))
    )
  ) 
 

(deftest proteger-bool-en-str-test
      (testing "Cuando hay valores a reemplazar"
        (is (= (proteger-bool-en-str "(or #f #t)") "(or %f %t)"))
        (is (= (proteger-bool-en-str "(and (or #f #t) #t)") "(and (or %f %t) %t)"))
        )
  (testing "Cuando hay una cadena vacia o un solo #f"
    (is (= (proteger-bool-en-str "") ""))
    (is (= (proteger-bool-en-str "(or #f #f)") "(or %f %f)"))
    )
  ) 


(deftest restaurar-bool-test

  (testing "Usando read-string"
    (is (= (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)"))) (list (symbol "and") (list (symbol "or") (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T")) (symbol "#T"))))
    (is (= (restaurar-bool (read-string "(and (or %F %f %t %T) %T)")) (list (symbol "and") (list (symbol "or") (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T")) (symbol "#T"))))
  )
  )

; user=> (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))
; (and (or #F #f #t #T) #T)
; user=> (restaurar-bool (read-string "(and (or %F %f %t %T) %T)") )
; (and (or #F #f #t #T) #T)


(deftest fnc-append-test
  (testing "Con listas validdas"
    (is (= (fnc-append '((1 2) (3) (4 5) (6 7))) '(1 2 3 4 5 6 7)))
    (is (= (fnc-append '((1 2) (7))) '(1 2 7))))

  (testing "Con listas invalidas"
    (is (= (fnc-append '((1 2) 3 (4 5) (6 7))) (generar-mensaje-error :wrong-type-arg 'append 3)))
    (is (= (fnc-append '(5 (4 5) (6 7))) (generar-mensaje-error :wrong-type-arg 'append 5)))
    (is (= (fnc-append '((1 2) A (4 5) (6 7))) (generar-mensaje-error :wrong-type-arg 'append 'A))))
  )


(deftest menor-o-igual-a-test
  (testing "Devuelve #t o #f"
    (is (= (menor-o-igual-a 4 3) (symbol "#t")))
    (is (= (menor-o-igual-a 4 6) (symbol "#f")))))

(deftest fnc-equal?-test
  (testing "Cuando son iguales"
    (is (= (fnc-equal? ()) (symbol "#t")))
    (is (= (fnc-equal? '(A)) (symbol "#t")))
    (is (= (fnc-equal? '(A a)) (symbol "#t")))
    (is (= (fnc-equal? '(A a A a)) (symbol "#t")))
    (is (= (fnc-equal? '(1 1 1 1)) (symbol "#t"))))
  (testing "Cuando son diferentes"
    (is (= (fnc-equal? '(1 1 2 1)) (symbol "#f")))
    (is (= (fnc-equal? '(a b c)) (symbol "#f"))))) 


(deftest fnc-read-test
  (testing "Cuando los parametros validos"
    (is (= (fnc-read '(1)) (generar-mensaje-error :io-ports-not-implemented 'read)))
    (is (= (fnc-read '(1 2 3)) (generar-mensaje-error :wrong-number-args-prim-proc 'read))))) 

(deftest fnc-sumar-test
  (testing "Cuando es vacio"
    (is (= (fnc-sumar ()) 0)))
  (testing "Cuando es una suma valida"
    (is (= (fnc-sumar '(3)) 3))     
    (is (= (fnc-sumar '(4 2)) 6))     
    )
  (testing "Cuando es una suma invalida"
    (is (= (fnc-sumar '(A 4 5 6)) (generar-mensaje-error :wrong-type-arg '+ 'A)))
    (is (= (fnc-sumar '(4 5 B 6)) (generar-mensaje-error :wrong-type-arg '+ 'B)))
         )
  ) 


(deftest fnc-restar-test
  (testing "Cuando es vacio"
    (is (= (fnc-restar ()) (generar-mensaje-error :wrong-number-args-oper '-))))
  (testing "Cuando es una resta valida"
    (is (= (fnc-restar '(3)) -3))
    (is (= (fnc-restar '(4 2)) 2)))
  (testing "Cuando es una resta invalida"
    (is (= (fnc-restar '(A 4 5 6)) (generar-mensaje-error :wrong-type-arg '- 'A)))
    (is (= (fnc-restar '(4 5 B 6)) (generar-mensaje-error :wrong-type-arg '- 'B))))) 


(deftest fnc-menor-test
  (testing "Cuando los valores estan ordenados"
    (is (= (fnc-menor ()) (symbol "#t")))
    (is (= (fnc-menor '(1)) (symbol "#t")))
    (is (= (fnc-menor '(1 2 3 4)) (symbol "#t"))))

  (testing "Cuando lo valores no son validos"
    (is (= (fnc-menor '(1 4 3 4)) (symbol "#f")))
    (is (= (fnc-menor '(1 A 3 4)) (generar-mensaje-error :wrong-type-arg '< 'A)))
    (is (= (fnc-menor '(1 3 3 A)) (generar-mensaje-error :wrong-type-arg '< 'A))))) 

(deftest fnc-mayor-test
  (testing "Cuando los valores estan ordenados"
    (is (= (fnc-mayor ()) (symbol "#t")))
    (is (= (fnc-mayor '(1)) (symbol "#t")))
    (is (= (fnc-mayor '(4 3 2 0)) (symbol "#t"))))

  (testing "Cuando lo valores no son validos"
    (is (= (fnc-mayor '(1 4 3 4)) (symbol "#f")))
    (is (= (fnc-mayor '(1 A 3 4)) (generar-mensaje-error :wrong-type-arg '> 'A)))
    (is (= (fnc-mayor '(1 3 3 A)) (generar-mensaje-error :wrong-type-arg '> 'A))))) 

(deftest fnc-mayor-o-igual-test
  (testing "Cuando los valores estan ordenados"
    (is (= (fnc-mayor-o-igual()) (symbol "#t")))
    (is (= (fnc-mayor-o-igual '(1)) (symbol "#t")))
    (is (= (fnc-mayor-o-igual '(4 2 2 0)) (symbol "#t")))
    (is (= (fnc-mayor-o-igual '(4 3 2 1)) (symbol "#t")))
    (is (= (fnc-mayor-o-igual '(4 4 4 4)) (symbol "#t")))
    )

  (testing "Cuando lo valores no son validos"
    (is (= (fnc-mayor-o-igual '(1 4 3 4)) (symbol "#f")))
    (is (= (fnc-mayor-o-igual '(1 A 3 4)) (generar-mensaje-error :wrong-type-arg '>= 'A)))
    (is (= (fnc-mayor-o-igual '(1 3 3 A)) (generar-mensaje-error :wrong-type-arg '>= 'A)))))


(deftest evaluar-escalar-test
  (testing "Cuando no existe en el ambiente"
    (is (= (evaluar-escalar 32 '(x 6 y 11 z "hola")) '(32 (x 6 y 11 z "hola"))))
    (is (= (evaluar-escalar "chau" '(x 6 y 11 z "hola")) '("chau" (x 6 y 11 z "hola"))))
    (is (= (evaluar-escalar 'n '(x 6 y 11 z "hola")) (list (generar-mensaje-error :unbound-variable 'n) '(x 6 y 11 z "hola")))))

  (testing "Cuando existe en el ambiente"
    (is (= (evaluar-escalar 'y '(x 6 y 11 z "hola")) '(11 (x 6 y 11 z "hola"))))
    (is (= (evaluar-escalar 'z '(x 6 y 11 z "hola")) '("hola" (x 6 y 11 z "hola"))))))

(deftest evaluar-define-test
  (testing "Cuando es una llamada correcta"
    (is (= (evaluar-define '(define x 2) '(x 1)) (list (symbol "#<void>") '(x 2))))
    (is (= (evaluar-define '(define y 2) '(x 1)) (list (symbol "#<void>") '(x 1 y 2))))
    (is (= (evaluar-define '(define (f x) (+ x 1)) '(x 1)) (list (symbol "#<void>") '(x 1 f (lambda (x) (+ x 1)))))))

  (testing "Cuando la llamada es invalida"
    (is (= (evaluar-define '(define) '(x 1)) (list (generar-mensaje-error :missing-or-extra 'define '(define)) '(x 1))))
    (is (= (evaluar-define '(define x) '(x 1)) (list (generar-mensaje-error :missing-or-extra 'define '(define x)) '(x 1))))
    (is (= (evaluar-define '(define x 2 3) '(x 1)) (list (generar-mensaje-error :missing-or-extra 'define '(define x 2 3)) '(x 1))))
    (is (= (evaluar-define '(define ()) '(x 1)) (list (generar-mensaje-error :missing-or-extra 'define '(define ())) '(x 1))))
    (is (= (evaluar-define '(define 2 x) '(x 1)) (list (generar-mensaje-error :bad-variable 'define '(define 2 x)) '(x 1))))
    (is (= (evaluar-define '(define () 2) '(x 1)) (list (generar-mensaje-error :bad-variable 'define '(define () 2)) '(x 1))))))

(deftest evaluar-or-test
      (testing "Sin operadores booleanos"
        (is (= (evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
               (list 7 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
        (is (= (evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
               (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))

        )
  
      (testing "Con al menos uno de los operadores boolenos" 
        (is (= (evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) 
               (list (symbol "#t") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
        (is (= (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
               (list 5 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
        (is (= (evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
               (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
        )
  
  ) 

(deftest evaluar-set!-test
      (testing "Cuando existe en el ambiente"
        (is (= (evaluar-set! '(set! x 1) '(x 0)) (list (symbol "#<void>") '(x 1))))
        (is (= (evaluar-set! '(set! y 1) '(y 2)) (list (symbol "#<void>") '(y 1)))) 
        )
      (testing "Cuando los parametros son invalidos"
        (is (= (evaluar-set! '(set! x 1) '()) (list (generar-mensaje-error :unbound-variable 'x) ())))
        ;(is (= (evaluar-set! '(set! x) '(x 0)) (list (generar-mensaje-error :missing-or-extra 'set! '(set! x)) '(x 0))))
        (is (= (evaluar-set! '(set! x 1 2) '(x 0)) (list (generar-mensaje-error :missing-or-extra 'set! '(set! x 1 2)) '(x 0))))
        (is (= (evaluar-set! '(set! 1 2) '(x 0)) (list (generar-mensaje-error :bad-variable 'set! 1) '(x 0)))) 
        )
  ) 
(deftest evaluar-if-test
      (testing "Cuando existe en el ambiente"
        (is (= (evaluar-if '(if 1 2) '(n 7)) '(2 (n 7))))
        (is (= (evaluar-if '(if 1 n) '(n 7)) '(7 (n 7)))) 
        (is (= (evaluar-if '(if 1 n 8) '(n 7)) '(7 (n 7)))) 
        (is (= (evaluar-if '(if (+ 3 3) n 8) '(n 7)) '(7 (n 7)))) 
        (is (= (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f"))) (list (symbol "#<void>") (list 'n 7 (symbol "#f") (symbol "#f"))))) 
        (is (= (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f"))) 
               (list (symbol "#<void>") (list 'n 9 (symbol "#f") (symbol "#f"))))) 
        )
  ) 

