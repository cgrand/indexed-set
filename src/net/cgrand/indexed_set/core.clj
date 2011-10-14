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

(deftype IndexedSet [^clojure.lang.IPersistentSet set unique-indexes summaries]
  UniqueConstrained
  (constrain-unique [this key]
    (if (unique-indexes key)
      this
      (let [index (into {} (map (juxt key identity) set))]
        (if (= (count index) (count set))
          (IndexedSet. set (assoc unique-indexes key index) summaries)
          (throw (IllegalStateException. "Existing entries are not unique."))))))  
  (remove-unique-constraint [this key]
    (let [new-unique-indexes (dissoc unique-indexes key)]
      (if (= unique-indexes new-unique-indexes)
        this
        (IndexedSet. set new-unique-indexes summaries))))
  (unique-constraints [this]
     unique-indexes)
  Summarized
  (add-summary [this laws]
    (if (summaries laws)
      this
      (IndexedSet. set unique-indexes 
        (let [{:keys [plus zero]} laws] 
          (assoc summaries laws (reduce plus zero set))))))
  (remove-summary [this laws]
    (let [new-summaries (dissoc summaries laws)]
      (if (= summaries new-summaries)
        this
        (IndexedSet. set unique-indexes new-summaries))))
  (summaries [this]
    summaries)
  clojure.lang.IPersistentSet
  (get [this value] (.get set value))
  (contains [this value] (.contains set value))
  (disjoin [this value]
    (let [new-set (.disjoin set value)] 
      (if (= new-set set)
        this
        (IndexedSet. new-set
          (into unique-indexes (for [[k index] unique-indexes] 
                                 [k (dissoc index (k value))]))
          (into summaries (for [[{- :minus :as laws} summary] summaries]
                            [laws (- summary value)]))))))
  clojure.lang.IPersistentCollection
  (count [this] (.count set))
  (cons [this value]
    (let [new-set (.cons set value)]
      (if (= new-set set)
        this
        (if-let [clashes (seq (for [[k index] unique-indexes
                                    :let [kv (find index (k value))]
                                    :when kv]
                                (val kv)))]
          (conj (reduce disj this clashes) value)
          (IndexedSet. new-set
            (into unique-indexes (for [[k index] unique-indexes] 
                                   [k (assoc index (k value) value)]))
            (into summaries (for [[{+ :plus :as laws} summary] summaries]
                              [laws (+ summary value)])))))))
  (empty [this]
    (IndexedSet. #{}
      (zipmap (keys unique-indexes) (repeat {}))
      (zipmap (keys summaries) (map :zero summaries))))
  (equiv [this that]
    (.equiv set that))
  clojure.lang.Seqable
  (seq [this]
    (seq set))
  clojure.lang.IFn
  (invoke [this v]
    (get this v))
  (invoke [this v not-found]
    (get this v not-found))
  Object
  (hashCode [this]
    (.hashCode set))
  (equals [this that]
    (.equals set that)))

(def empty-indexed-set (IndexedSet. #{} {} {}))

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