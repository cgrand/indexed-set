# indexed-set

A set implementation which can
 * maintain summaries (eg indexes),
 * enforce unicity of items in regards to custom keys (fns),
 * support primary key.


## Usage

    (require '[net.cgrand.indexed-set.core :as r])
    (r/indexed-set 
      :primary :id 
      :index :fname)
    ;=> #{}
    (conj *1 {:id 1 :fname "Fred" :lname "Brooks"})
    ;=> #{{:id 1, :fname "Fred", :lname "Brooks"}}
    (conj *1 {:id 1 :fname "Fred" :mname "Phillips" :lname "Brooks"})
    ;=> #{{:id 1, :fname "Fred", :mname "Phillips", :lname "Brooks"}}
    (conj *1 {:id 2 :fname "Fred" :lname "Mercury"})
    ;=> #{{:id 2, :fname "Fred", :lname "Mercury"} {:id 1, :fname "Fred", :mname "Phillips", :lname "Brooks"}}
    (conj *1 {:id 3 :fname "Mel" :lname "Brooks"})
    ;=> #{{:id 3, :fname "Mel", :lname "Brooks"} {:id 2, :fname "Fred", :lname "Mercury"} {:id 1, :fname "Fred", :mname "Phillips", :lname "Brooks"}}
    (r/summaries *1) ; the keys are complex, it's better to use helpers such as index-by to find the one you want
    ;=> {{:plus {:key :fname, :f #<core$conj clojure.core$conj@5d290ef4>, :init #{}}, :minus {:key :fname, :f #<core$disj clojure.core$disj@6815ee24>, :init #{}}, :zero {}} {"Mel" #{{:id 3, :fname "Mel", :lname "Brooks"}}, "Fred" #{{:id 1, :fname "Fred", :mname "Phillips", :lname "Brooks"} {:id 2, :fname "Fred", :lname "Mercury"}}}}
    (r/index-by *2 :fname)
    ;=> {"Mel" #{{:id 3, :fname "Mel", :lname "Brooks"}}, "Fred" #{{:id 1, :fname "Fred", :mname "Phillips", :lname "Brooks"} {:id 2, :fname "Fred", :lname "Mercury"}}}       
    

## License

Copyright (C) 2011 Christophe Grand

Distributed under the Eclipse Public License, the same as Clojure.
