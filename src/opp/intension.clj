(ns opp.intension
  (:refer-clojure :exclude [comp complement constantly fn juxt partial])
  (:require [clojure.core :as core]))

(deftype Fn [hash func]
  Callable
  (call [_] (.call func))

  Runnable
  (run [_] (.run func))

  clojure.lang.IFn
  (invoke [_]
    (.invoke func))
  (invoke [_ a]
    (.invoke func a))
  (invoke [_ a b]
    (.invoke func a b))
  (invoke [_ a b c]
    (.invoke func a b c))
  (invoke [_ a b c d]
    (.invoke func a b c d))
  (invoke [_ a b c d e]
    (.invoke func a b c d e))
  (invoke [_ a b c d e f]
    (.invoke func a b c d e f))
  (invoke [_ a b c d e f g]
    (.invoke func a b c d e f g))
  (invoke [_ a b c d e f g h]
    (.invoke func a b c d e f g h))
  (invoke [_ a b c d e f g h i]
    (.invoke func a b c d e f g h i))
  (invoke [_ a b c d e f g h i j]
    (.invoke func a b c d e f g h i j))
  (invoke [_ a b c d e f g h i j k]
    (.invoke func a b c d e f g h i j k))
  (invoke [_ a b c d e f g h i j k l]
    (.invoke func a b c d e f g h i j k l))
  (invoke [_ a b c d e f g h i j k l m]
    (.invoke func a b c d e f g h i j k l m))
  (invoke [_ a b c d e f g h i j k l m n]
    (.invoke func a b c d e f g h i j k l m n))
  (invoke [_ a b c d e f g h i j k l m n o]
    (.invoke func a b c d e f g h i j k l m n o))
  (invoke [_ a b c d e f g h i j k l m n o p]
    (.invoke func a b c d e f g h i j k l m n o p))
  (invoke [_ a b c d e f g h i j k l m n o p q]
    (.invoke func a b c d e f g h i j k l m n o p q))
  (invoke [_ a b c d e f g h i j k l m n o p q s]
    (.invoke func a b c d e f g h i j k l m n o p q s))
  (invoke [_ a b c d e f g h i j k l m n o p q s t]
    (.invoke func a b c d e f g h i j k l m n o p q s t))
  (invoke [_ a b c d e f g h i j k l m n o p q s t u]
    (.invoke func a b c d e f g h i j k l m n o p q s t u))
  (invoke [_ a b c d e f g h i j k l m n o p q s t u rest]
    (.invoke func a b c d e f g h i j k l m n o p q s t u rest))
  (applyTo [_ args]
    (.applyTo func args))

  clojure.lang.IObj
  (meta [_]
    (.meta func))
  (withMeta [_ m]
    (Fn. hash (.withMeta func m)))

  Object
  (hashCode [_] hash)
  (equals [_ f]
    (= hash (core/hash f))))



(defmacro ^:private arglists [sym]
  `(-> (var ~sym) meta :arglists))

(defmacro ^:private docstring [sym]
  `(-> (var ~sym) meta :doc))


(defn comp
  {:arglists (arglists core/comp)
   :doc (docstring core/comp)}
  ([] identity)
  ([f] f)
  ([f g & fs]
     (let [args (list* f g fs)]
       (Fn. (reduce hash-combine (hash ::comp) args)
            (apply core/comp args)))))

(defn complement
  {:arglists (arglists core/complement)
   :doc (docstring core/complement)}
  ([f] (Fn. (hash-combine (hash ::complement) f)
            (core/complement f))))

(defn constantly
  {:arglists (arglists core/constantly)
   :doc (docstring core/constantly)}
  ([x] (Fn. (hash-combine (hash ::constantly) x)
            (core/constantly x))))

(defn juxt
  {:arglists (arglists core/juxt)
   :doc (docstring core/juxt)}
  ([f & fs]
     (let [args (list* f fs)]
       (Fn. (reduce hash-combine (hash ::juxt) args)
            (apply core/juxt args)))))

(defn partial
  {:arglists (arglists core/partial)
   :doc (docstring core/partial)}
  ([f] f)
  ([f arg1 & more]
     (let [args (list* f arg1 more)]
       (Fn. (reduce hash-combine (hash ::partial) args)
            (apply core/partial args)))))


;; keep this here to prevent accidental usage

(defmacro fn
  {:arglists (arglists core/fn)
   :doc (docstring core/fn)}
  [& body]
  `(Fn. ~(hash-combine (hash ::fn) body)
        (core/fn ~@body)))
