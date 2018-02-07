(ns ari.core_test
  (:require [clojure.test :refer :all]
            [ari.core :refer :all]
            [ari.parse :refer :all]))

(def test-tokens [["a" "name"] ["|" "pipe"] ["b" "name"]])

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
               ))))
