;   Copyright (c) Christophe Grand, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns net.cgrand.indexed-set.core
  "A set implementation which can
   * maintain summaries (eg indexes),
   * enforce unicity of items in regards to custom keys (fns),
   * support primary key."
  {:author "Christophe Grand"})

;; define unchecked-subtract-int and unchecked-add-int for 1.2
(defmacro ^{:private true} compat-1.2 []
  (when (= (map *clojure-version* [:major :minor]) [1 2])
    `(do
       (defn unchecked-subtract-int [a# b#]
         (unchecked-subtract (int a#) (int b#)))
       (defn unchecked-add-int [a# b#]
         (unchecked-add (int a#) (int b#))))))

(compat-1.2)

(defprotocol UniqueConstrained
  "Protocol to manage unicity constraints. Unicity constraints modify the
   behaviour of conj."
  (constrain-unique [set key] 
    "Returns a new set where the function key is a unique constraint. If the
     existing items are not unique in regards to key, throws an exception.")  
  (remove-unique-constraint [set key]
    "Returns a new set where the function key is not a unique constraint any
     more.")
  (unique-constraints [set]
    "Returns a map of key functions to unicity indexes (map from unique keys
     (results of calling a key function on items) to actual items."))

(defprotocol PrimaryConstrained
  "Protocol to manage primary constraints. Primary constraints modify the
   behaviour of get, disj and contains?."
  (constrain-primary [set key]
    "Returns a new set where the function key is the primary constraint. The
     previous primary constraint remains as a unicity constraint. If the items
     are not unique for the new primary key, throws an exception.")
  (primary-constraint [set]
    "Returns a map from primary key values (results of the key fn) to items"))

(defn remove-primary-constraint 
 "Removes the current primary key fn. Equivalent to
  (constrain-primary set identity)."
 [set]
  (constrain-primary set identity))

(defn laws
 "Returns a laws map used for summaries. If items are of type A and the
  summary of type B then plus and minus are of type B*A->B and zero is of
  type B. zero is the initial value for the summary, plus updates the summary
  when a new item is added, minus when it is removes."
 [plus minus zero]
  {:plus plus :minus minus :zero zero})

(defprotocol Summarized
 "Protocol to manage \"online\" summaries (views, indexes, etc.). The value
  of a summary is always equal to (reduce (:plus laws) (:zero laws) set)."
  (add-summary [set laws]
    "Returns a new set with the additional summary specified by laws.")
  (remove-summary [set laws]
    "Returns a new set without the summary specified by laws.")
  (summaries [set]
    "Returns a map of laws to actual summary values."))

(defn summary
 "Returns the actual summary value for the summary specified by laws."
 [set laws]
  ((summaries set) laws))

(deftype IndexedSet [pk unique-indexes summaries ^int _h]
  UniqueConstrained
  (constrain-unique [this key]
    (if (unique-indexes key)
      this
      (let [index (into {} (map (juxt key identity) this))]
        (if (= (count index) (count this))
          (IndexedSet. pk (assoc unique-indexes key index) summaries _h)
          (throw (IllegalStateException. "Existing entries are not unique."))))))  
  (remove-unique-constraint [this key]
    (when (= pk key)
      (throw (IllegalStateException. 
               "Can't remove the primary unicity constraint")))
    (let [new-unique-indexes (dissoc unique-indexes key)]
      (if (= unique-indexes new-unique-indexes)
        this
        (IndexedSet. pk new-unique-indexes summaries _h))))
  (unique-constraints [this]
     unique-indexes)
  PrimaryConstrained
  (constrain-primary [this key]
    (cond
      (= pk key) this 
      (unique-indexes key) (IndexedSet. key unique-indexes summaries _h)
      :else
        (let [index (into {} (map (juxt key identity) this))]
          (if (= (count index) (count this))
            (IndexedSet. key (assoc unique-indexes key index) summaries _h)
            (throw (IllegalStateException. "Existing entries are not unique."))))))
  (primary-constraint [this] (unique-indexes pk))
  Summarized
  (add-summary [this laws]
    (if (summaries laws)
      this
      (IndexedSet. pk unique-indexes 
        (let [{:keys [plus zero]} laws] 
          (assoc summaries laws (reduce plus zero this)))
        _h)))
  (remove-summary [this laws]
    (let [new-summaries (dissoc summaries laws)]
      (if (= summaries new-summaries)
        this
        (IndexedSet. pk unique-indexes new-summaries _h))))
  (summaries [this]
    summaries)
  clojure.lang.IPersistentSet
  (get [this value] (get (unique-indexes pk) (pk value)))
  (contains [this value] (contains? (unique-indexes pk) (pk value)))
  (disjoin [this value]
    (if-let [kv (find (unique-indexes pk) (pk value))]
      (IndexedSet. pk
        (into unique-indexes (for [[k index] unique-indexes] 
                               [k (dissoc index (k value))]))
        (into summaries (for [[{- :minus :as laws} summary] summaries]
                          [laws (- summary value)]))
        (unchecked-subtract-int _h (hash (val kv))))
      this))
  clojure.lang.IPersistentCollection
  (count [this] (count (unique-indexes pk)))
  (cons [this value]
    (if (when-let [kv (find (unique-indexes pk) (pk value))]
          (= value (val kv)))
      this
      (if-let [clashes (seq (for [[k index] unique-indexes
                                  :let [kv (find index (k value))]
                                  :when kv]
                              (val kv)))]
        (conj (reduce disj this clashes) value)
        (IndexedSet. pk
          (into unique-indexes (for [[k index] unique-indexes] 
                                 [k (assoc index (k value) value)]))
          (into summaries (for [[{+ :plus :as laws} summary] summaries]
                            [laws (+ summary value)]))
          (unchecked-add-int _h (hash value))))))
  (empty [this]
    (IndexedSet. pk
      (zipmap (keys unique-indexes) (repeat {}))
      (zipmap (keys summaries) (map :zero (keys summaries)))
      0))
  (equiv [this that]
    (.equals this that))
  java.util.Set
  (containsAll [this coll]
    (every? #(contains? this %) coll))
  (isEmpty [this]
    (zero? (count this)))
  (iterator [this]
    (clojure.lang.SeqIterator. (seq this)))
  (size [this]
    (count this))
  (toArray [this]
    (into-array Object (seq this)))
  (toArray [this a]
    (into-array (-> a class .getComponentType) (seq this)))
  clojure.lang.Seqable
  (seq [this]
    (vals (unique-indexes pk)))
  clojure.lang.IFn
  (invoke [this v]
    (get this v))
  (invoke [this v not-found]
    (get this v not-found))
  Object
  (hashCode [this] _h)
  (equals [this that]
    (and
      (instance? java.util.Set that)
      (= (count this) (count that))
      (every? #(contains? this %) that))))

(def ^{:doc "An empty indexed-set with no constraints and no summaries,
             equivalent to a regular set."} 
  empty-indexed-set (IndexedSet. identity {identity {}} {} 0))

(defrecord Op-By [key f init]
  clojure.lang.IFn
  (invoke [this m v]
    (let [k (key v)]
      (assoc m k (f (m k init) v)))))

(defn- op-by [key f init] (Op-By. key f init))

(defn aggregate-by
 "Given summarization laws and a key function, returns new summarization laws
  which compute sub-summaries according to the original laws for items grouped
  by key."
 [{:keys [plus minus zero]} key]
  {:plus (op-by key plus zero)
   :minus (op-by key minus zero)
   :zero {}})

(defn add-summary-by 
 "\"Grouped by\" variant of add-summary." 
 [set key laws]
  (add-summary set (aggregate-by laws key)))

(defn remove-summary-by 
 "\"Grouped by\" variant of remove-summary." 
 [set key laws]
  (remove-summary set (aggregate-by laws key)))

(defn summary-by 
 "\"Grouped by\" variant of summary."
 [set key laws]
  (summary set (aggregate-by laws key)))

(defn index-laws
 "Given a key function, returns summarizations laws to compute an index."
 [key]
  (aggregate-by (laws conj disj #{}) key))

(defn add-index
 "Returns a new set with an index for the specified key added."
 [set key]
  (add-summary set (index-laws key)))

(defn remove-index
 "Returns a new set with the index for the specified key removed."
 [set key]
  (remove-summary set (index-laws key)))

(defn index
 "Returns the index for the specified key."
 [set key]
  (summary set (index-laws key)))

(def indexed-set-options {:primary constrain-primary
                          :unique constrain-unique
                          :uniques #(reduce constrain-unique %1 %2)
                          :index add-index
                          :indexes #(reduce add-index %1 %2)
                          :summary add-summary
                          :summaries #(reduce add-summary %1 %2)})

(defn indexed-set
 "Helper function to create an indexed-set. See indexed-set-options for
  supported options." [items? & options]
  (let [[items & options] (if (keyword? items?)
                            (list* nil items? options)
                            (list* items? options))
        options (partition 2 options)
        s (into empty-indexed-set items)]
    (reduce (fn [s [o v]]
              (if-let [f (indexed-set-options o)]
                (f s v)
                (throw (IllegalArgumentException. 
                         (str "Unknown option: " o)))))
            s options)))
