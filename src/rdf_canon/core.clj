(ns rdf-canon.core
  "Deterministic RDF quad canonical string and hash helpers."
  (:require [clojure.string :as str])
  (:import [java.security MessageDigest]))

(defn esc [s]
  (-> (str s)
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")
      (str/replace "\r" "\\r")))

(defn term [x]
  (case (:rdf/type x)
    :iri (str "<" (:value x) ">")
    :blank (str "_:" (:id x))
    :literal (str "\"" (esc (:value x)) "\""
                  (cond
                    (:language x) (str "@" (:language x))
                    (:datatype x) (str "^^<" (:value (:datatype x)) ">")
                    :else ""))
    (throw (ex-info "Unknown RDF term" {:term x}))))

(defn quad-line [{:keys [subject predicate object graph]}]
  (str (term subject) " " (term predicate) " " (term object)
       (when graph (str " " (term graph)))
       " ."))

(defn canonical [quads]
  (str (str/join "\n" (sort (map quad-line quads)))
       (when (seq quads) "\n")))

(defn sha-256 [s]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn digest [quads]
  (sha-256 (canonical quads)))
