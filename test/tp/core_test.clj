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