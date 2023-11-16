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
  (testing "Cuando no hay que reemplazar"
    (is (= (restaurar-bool "") "")))
  (testing "Cuando hay valores para reemplazar"
    (is (= (restaurar-bool "(and (or #F #f #t #T) #T)") "(and (or #F #f #t #T) #T)"))
    (is (= (restaurar-bool "(and (or %F %f %t %T) %T)") "(and (or #F #f #t #T) #T)"))
    )
  (testing "Usando read-string"
    (is (= (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)"))) "(and (or #F #f #t #T) #T)"))
    (is (= (restaurar-bool (read-string "(and (or %F %f %t %T) %T)")) "(and (or #F #f #t #T) #T)"))
  )
  )