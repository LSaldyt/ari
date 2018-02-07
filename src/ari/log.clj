(ns ari.log)

(defn log [log-tree message]
  (let [head (:head log-tree)
        head+ (concat head (list :log))
        result (get-in log-tree head+)]
    ;(println (apply str (repeat (count head) "  ")) message)
    (if result
      (assoc-in log-tree head+ (concat result (list message)))
      (assoc-in log-tree head+ (list message)))))

(defn log-pop [log-tree]
  (let [head (:head log-tree)]
    (assoc log-tree :head (drop-last 1 head))))

(defn log-push [log-tree k]
  (let [head (:head log-tree)
        tree (assoc log-tree :head (concat head (list k)))]
    tree))

(defn join [a b]
  (let [head (:head a)
        all-a (:all a)
        all-b (:all b)]
    {:head head
     :all (merge-with concat all-a all-b)}))
