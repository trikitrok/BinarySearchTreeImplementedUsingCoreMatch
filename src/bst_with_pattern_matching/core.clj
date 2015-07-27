(ns bst-with-pattern-matching.core
  (:require [clojure.core.match :refer [match]]))

(defrecord BinarySearchTree [value left right])

(defn singleton [value]
  (map->BinarySearchTree {:value value}))

(defn insert [new-value tree]
  (match [tree]
         [nil] (singleton new-value)

         [{:value value :left left :right right}]
         (if (<= new-value value)
           (->BinarySearchTree value (insert new-value left) right)
           (->BinarySearchTree value left (insert new-value right)))))

(defn from-list [values]
  (match [values]
         [([] :seq)] nil

         [([first-value & rest-values] :seq)]
         (reduce #(insert %2 %1) (singleton first-value) rest-values)))

(defn in-order [tree]
  (match [tree]
         [nil] []

         [{:value value :left left :right right}]
         (concat (in-order left) [value] (in-order right))))