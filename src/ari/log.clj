(ns ari.log)

(defn save [log-tree data k]
  (let [head (:head log-tree)
        head+ (concat head (list k))
        result (get-in log-tree head+)]
    (println (apply str (repeat (count head+) "  ")) data)))
    ; (println head+)
    ; (if result
    ;   (assoc-in log-tree head+ (concat result (list data)))
    ;   (assoc-in log-tree head+ (list data)))))

(defn log [log-tree message]
  (save log-tree message :log))

(defn snapshot [log-tree state]
  (save log-tree state :state))

(defn commit [log-tree message state]
  (snapshot (log log-tree message) state))

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
