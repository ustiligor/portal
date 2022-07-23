(ns portal.oracle)

(defn roll
  [die-size]
  (let [random-number (rand-int die-size)
        die-face (+ 1 random-number)]
    die-face))

(defn select-item
  [oracle]
  (get oracle (roll (count oracle))))

(defn -main
  []
  (println (select-item ["hello" "sword" "dagger"])))


