(ns net.cgrand.indexed-set.test.core
  (:use [net.cgrand.indexed-set.core])
  (:use [clojure.test]))

(deftest unicity
  (let [s (-> empty-indexed-set
            (constrain-unique #(mod % 7))
            (constrain-unique #(mod % 5)))]
    (are [x coll] (= x (into s coll))
      #{5 7} [5 7]
      #{5 14} [5 7 14]
      #{35} [5 7 35])))

(deftest primary-key
  (let [s (-> empty-indexed-set
            (constrain-primary :id)
            (conj {:id 1 :name "Fred"} {:id 2 :name "Ethel"}))]
    (are [x y] (= x (s y))
      {:id 1 :name "Fred"} {:id 1}
      {:id 1 :name "Fred"} {:id 1 :name "Frederick"}
      {:id 2 :name "Ethel"} {:id 2})))
