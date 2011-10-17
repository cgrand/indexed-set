# indexed-set

A set implementation which can
 * maintain summaries (eg indexes),
 * enforce unicity of items in regards to custom keys (fns),
 * support primary key.


## Usage

Importing it your namespace:

    (require '[net.cgrand.indexed-set.core :as r])

Creating an empty indexed-set with a primary-key and an index:

    (r/indexed-set 
      :primary :id 
      :index :fname)
    ;=> #{}

Adding an item:

    (conj *1 {:id 1 :fname "Fred" :lname "Brooks"})
    ;=> #{{:id 1, :fname "Fred", :lname "Brooks"}}

:id is the primary fn and, as such, is also a unique constraint. Adding another item with the same :id replaces the existing one:

    (conj *1 {:id 1 :fname "Fred" :mname "Phillips" :lname "Brooks"})
    ;=> #{{:id 1, :fname "Fred", :mname "Phillips", :lname "Brooks"}}

:id being the primary fn means that get, disj and contains? only cares about the return value of the primary fns, not the whole value. Hence, retrieving something by its primary key is as easy as:

    (get *1 {:id 1})
    ;=> {:id 1, :fname "Fred", :lname "Brooks"}


Let's add more items:

    (conj *2 {:id 2 :fname "Fred" :lname "Mercury"})
    ;=> #{{:id 2, :fname "Fred", :lname "Mercury"} {:id 1, :fname "Fred", :mname "Phillips", :lname "Brooks"}}
    (conj *1 {:id 3 :fname "Mel" :lname "Brooks"})
    ;=> #{{:id 3, :fname "Mel", :lname "Brooks"} {:id 2, :fname "Fred", :lname "Mercury"} {:id 1, :fname "Fred", :mname "Phillips", :lname "Brooks"}}

Now we can look at the index. An index is a kind of summary.

    (r/summaries *1) 
    ;=> {{:plus {:key :fname, :f #<core$conj clojure.core$conj@5d290ef4>, :init #{}}, :minus {:key :fname, :f #<core$disj clojure.core$disj@6815ee24>, :init #{}}, :zero {}} {"Mel" #{{:id 3, :fname "Mel", :lname "Brooks"}}, "Fred" #{{:id 1, :fname "Fred", :mname "Phillips", :lname "Brooks"} {:id 2, :fname "Fred", :lname "Mercury"}}}}

Useful but a bit noisy when you don't have your summary's laws around. There's a shorthand for indexes:

    (r/index *2 :fname)
    ;=> {"Mel" #{{:id 3, :fname "Mel", :lname "Brooks"}}, 
         "Fred" #{{:id 1, :fname "Fred", :mname "Phillips", :lname "Brooks"}
                  {:id 2, :fname "Fred", :lname "Mercury"}}}       
    

## License

Copyright (C) 2011 Christophe Grand

Distributed under the Eclipse Public License, the same as Clojure.
