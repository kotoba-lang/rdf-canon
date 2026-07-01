(ns rdf-canon.core-test
  (:require [clojure.test :refer [deftest is]]
            [rdf-canon.core :as canon]))

(defn iri [v] {:rdf/type :iri :value v})
(defn lit [v] {:rdf/type :literal :value v})

(deftest canonicalizes-by-sort
  (let [a {:subject (iri "b") :predicate (iri "p") :object (lit "2")}
        b {:subject (iri "a") :predicate (iri "p") :object (lit "1")}]
    (is (= "<a> <p> \"1\" .\n<b> <p> \"2\" .\n"
           (canon/canonical [a b])))
    (is (= 64 (count (canon/digest [a b]))))))
