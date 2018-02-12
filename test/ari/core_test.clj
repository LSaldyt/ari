(ns ari.core_test
  (:require [clojure.test :refer :all]
            [ari.core :refer :all]
            [ari.parse.parse :refer :all]
            [ari.parse.base :refer :all]))

;TODO: macros unifying common testing patterns

(def test-tokens [["a" "name"] ["|" "pipe"] ["b" "name"]])

(defn full-tree [answer]
  (not (nil? (first answer))))

(deftest sep-test
  (testing "sepby"
    (is (= 2 (count (:values (first
      ((sep-by (tag "name" :name) (token "|"))
               test-tokens
               {}))))))
    (is (nil? (first 
      ((sep-by1 (tag "name" :name) (token "|"))
       [["a" "name"]]
       {}))))
    (is (= 2 (count (:values (first
      ((sep-by1 (tag "name" :name) (token "|"))
               test-tokens
               {}
               ))))))))

(def conseq-parsers [(token "a" :a)
                     (token "|" :pipe)
                     (token "b" :b)])

(deftest conseq-test
  (testing "conseq"
    (is (full-tree
      ((conseq-merge conseq-parsers)
       test-tokens
       {})))))
    (is (full-tree
      ((conseq conseq-parsers)
       test-tokens
       {})))

(defparser n-parser (tag "N"))
(defparser p-parser (tag "P"))

(deftest from-test
  (testing "from"
    (is (full-tree
      ((from-except [n-parser] [p-parser])
       [["N" "N"]]
       {})))
    (is (nil? (first 
      ((from-except [n-parser] [n-parser])
       [["N" "N"]]
       {}))))
    (is (full-tree
      ((from [(tag "NAH") (tag "name")])
       [["x" "name"]]
       {})))))

(deftest many-test
  (testing "many"
    (is (full-tree
          ((many (tag "name"))
           [["x" "name"]["x" "name"]]
          {})))
    (is (full-tree
          ((many (tag "x"))
           []
          {})))
    (is (full-tree
          ((many1 (tag "name"))
           [["x" "name"]["x" "name"]]
          {})))
    (is (not (full-tree
          ((many1 (tag "x"))
           []
          {}))))))

(deftest optional-test
  (testing "optional"
    (is (full-tree
          ((optional (tag "x"))
           []
           {})))
    (is (full-tree
          ((optional (tag "x"))
           [["x" "x"]]
           {})))))

(deftest discard-test
  (testing "discard"
    (is (full-tree
          ((discard (tag "x"))
           [["x" "x"]]
           {})))
    (is (not (full-tree
          ((discard (tag "x"))
           []
           {}))))))

(def parser-tree (atom {:ref (tag "x")}))

(deftest retrieve-test
  (testing "retrieve"
    (is (full-tree
          ((retrieve :ref parser-tree)
           [["x" "x"]]
           {})))
    (is (not (full-tree
          ((retrieve :ref parser-tree)
           []
           {}))))))

; Test status
; x conseq 
; x conseq-merge 
; x many 
; x many1
; x from
; x from-except
; x optional
; x discard
; x sepby
; x sepby1
; x retrieve

