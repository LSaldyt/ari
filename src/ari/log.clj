(ns ari.log)

(defn save [log-tree data show k]
  (let [head (:head log-tree)
        head+ (concat head (list k))
        result (get-in log-tree head+)
        verb (:verbosity log-tree)
        log-tree (if result
                   (assoc log-tree (apply str head+) (concat result (list data)))
                   (assoc log-tree (apply str head+) (list data)))]
    (if (< (count head+) verb)
      (do 
        (when show (println (apply str (repeat (count head+) "  ")) data))
          log-tree)
      log-tree)))

    ;(println (apply str (repeat (count head+) "  ")) head+)
    ; (println head+)
    ; (if result
    ;   (assoc-in log-tree head+ (concat result (list data)))
    ;   (assoc-in log-tree head+ (list data)))))

(defn snapshot [log-tree show state]
  (save log-tree state show :state))

(defn log [log-tree message]
  (save log-tree message true :log))

(defn vlog [log-tree message]
  (save log-tree message false :log))

(defn commit [log-tree message state]
  (snapshot (log log-tree message) true state))

(defn vcommit [log-tree message state]
  (snapshot (vlog log-tree message) false state))

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

(defn join [a b]
  (let [head (:head a)
        all-a (:all a)
        all-b (:all b)]
    {:head head
     :all (merge-with concat all-a all-b)}))
