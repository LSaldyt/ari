(ns ari.log)

(defn save [log-tree data k]
  (let [head (:head log-tree)
        head+ (concat head (list k))
        result (get-in log-tree head+)
        verb (:verbosity log-tree)]
    (if (< (count head+) verb)
      (do (println (apply str (repeat (count head+) "  ")) data)
          log-tree)
      log-tree)))
    ;(println (apply str (repeat (count head+) "  ")) head+)
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
  (let [head (:head log-tree)
        verb (:verbosity log-tree)]
    (assoc (assoc log-tree :head (drop-last 1 head))
           :verbosity verb
           )))

(defn log-push [log-tree k]
  (let [head (:head log-tree)
        verb (:verbosity log-tree)
        tree (assoc 
               (assoc log-tree :head (concat head (list k))) 
               :verbosity verb)]
    tree))

