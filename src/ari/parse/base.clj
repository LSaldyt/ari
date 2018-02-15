(ns ari.parse.base
  (:require [ari.log :as log]))

(defn- demunge-fn
  "Helper function for retreiving a function name as a string"
  [fn-object]
  (let [dem-fn (str fn-object)
        pretty (second (re-find #"(.*?\/.*?)[\-\-|@].*" dem-fn))]
    (if pretty pretty dem-fn)))

(defn fn-name [fn-object]
  "Function for retreiving a function name as a string"
  (let [s (demunge-fn fn-object)]
    (subs s 0 (- (count s) 9))))


(defn use-parser [parser tokens log]
  (let [log (log/log-push log (fn-name parser))
        [tree remaining in-log] (parser tokens log)
        in-log (log/log-pop in-log)]
    [tree remaining in-log]))

(def any "*any*")

(defn- token-matcher 
  "((just any 'int') [2 'int']) -> [token tag]"
  ([etoken etag]
   (token-matcher etoken etag nil))
  ([etoken etag k]
    (fn [[token tag]]
      (if (and 
            (or (= etoken token) (= etoken any))
            (or (= etag tag)     (= etag any)))
        [token tag k]
        nil))))

(defn- token-matcher-wrapper [parser etoken etag]
  (fn [tokens log] 
    (let [result (parser (first tokens))]
      (if result
        (let [[token tag k] result]
          [(if (nil? k) ; Presence of k determines if value is kept
             {}
             {k [token tag]})
           (rest tokens)
           (log/log log (str "Success: <" etoken ">, <" etag ">"))])
        [nil tokens (log/log log (str "Failed: <" etoken ">, <" etag ">"
                                      " (against: " (first tokens) ")"))]))))

(defn just 
  ([etoken etag]
   (just etoken etag nil))
  ([etoken etag k]
   (token-matcher-wrapper (token-matcher etoken etag k) etoken etag)))

(defn tag
  ([etag]
    (just any etag))
  ([etag k]
    (just any etag k)))

(defn token
  ([etoken]
    (just etoken any))
  ([etoken k]
    (just etoken any k)))

(defn wild
  ([] (just any any))
  ([k] (just any any k)))
