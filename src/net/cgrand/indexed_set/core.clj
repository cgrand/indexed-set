(ns net.cgrand.indexed-set.core)

(defprotocol UniqueConstrained
  (constrain-unique [set key])  
  (remove-unique-constraint [set key])
  (unique-constraints [set]))

(defn laws [plus minus zero]
  {:plus plus :minus minus :zero zero})

(defprotocol PrimaryConstrained
  (constrain-primary [set key])
  (primary-constraint [set]))

(defn remove-primary-constraint [set]
  (constrain-primary set identity))

(defprotocol Summarized
  (add-summary [set laws])
  (remove-summary [set laws])
  (summaries [set]))

(defn summary [set laws]
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

(def empty-indexed-set (IndexedSet. identity {identity {}} {} 0))

(defrecord Op-By [key f init]
  clojure.lang.IFn
  (invoke [this m v]
    (let [k (key v)]
      (assoc m k (f (m k init) v)))))

(defn- op-by [key f init] (Op-By. key f init))

(defn aggregate-by [{:keys [plus minus zero]} key]
  {:plus (op-by key plus zero)
   :minus (op-by key minus zero)
   :zero {}})

(defn add-summary-by [set key laws]
  (add-summary set (aggregate-by laws key)))

(defn remove-summary-by [set key laws]
  (remove-summary set (aggregate-by laws key)))

(defn summary-by [set key laws]
  (summary set (aggregate-by laws key)))

(defn index-by-laws [key]
  (aggregate-by (laws conj disj #{}) key))

(defn add-index-by [set key]
  (add-summary set (index-by-laws key)))

(defn get-index-by [set key]
  (summary set (index-by-laws key)))