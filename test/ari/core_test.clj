(ns ari.core_test
  (:require [clojure.test :refer :all]
            [ari.core :refer :all]
            [ari.parse.parse :refer :all]
            [ari.parse.base :refer :all]))

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
               ))))))))

(deftest conseq-test
  (testing "conseq"
    (is (not (nil? (first
      ((conseq-merge [
        (token "a" :a)
        (token "|" :pipe)
        (token "b" :b)])
       test-tokens
       {})))))))

(deftest from-test
  (testing "from"
    (is (not (nil? (first
      ((from [(tag "NAH") (tag "name")])
       [["x" "name"]]
       {})))))))

