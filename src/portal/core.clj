(ns portal.core
  (:require
   [clojure.string :as string]))

(defn default-action
  [state choice]
  (cond
    (= choice "die")
    (assoc state :dead true)

    (#{"inventory" "I" "i" "inv"} choice)
    (do
      (println "your inventory is:" (string/join ", " (map name (keys (get state :inventory)))))
      state)

    (#{"look" "l" "L"} choice)
    (dissoc state :previous-place)

    :else
    (do 
      (println choice "is not a valid choice")
      state)))

(def example-game
  {:initial-state
   (fn [state]
     {:place :starting
      :inventory {:eyeball {}}
      :place-state {}})
   :places
   {:starting
    {:description 
     (fn [state]
       [(str (get state :name) ", you stand in a room")
        "there are 3 portals before you"
        "a red one, a green one and a blue one"])
     :choices
     (fn [state]
       {:red
        (fn [state]
          (assoc state :place :red))
        :green
        (fn [state]
          (assoc state :place :green))
        :blue
        (fn [state]
          (assoc state :place :blue))})}

    :red
    {:description
     (fn [state]
       ["the room is red."
        (if (get-in state [:place-state :red :heart :squished])
          "There is a heart squished on a pedestal here"
          "There is a beating heart on a pedestal.")
        "There is a foggy glass case"])
     :default
     (fn [state choice]
       (if (= choice "return")
         (assoc state :place :starting)
         (default-action state choice)))
     :choices
     (fn [state]
       (let [choices 
             {:case
              (fn [state]
                (println "the case is foggy, but you can see it can be lifted")
                (assoc-in state [:place-state :red :case :looked] true))
              :heart
              (fn [state]
                (println "the heart is huge and grotesque and pumping obscenely")
                (assoc-in state [:place-state :red :heart :looked] true))}]
         (reduce
          (fn [choices state-check]
            (state-check choices))
          choices
          [(fn [choices]
             (if (get-in state [:place-state :red :case :looked])
               (assoc choices :lift (fn [state]
                                      (println "you lift the case and release poison gas")
                                      (assoc state :dead true)))
               choices))
           (fn [choices]
             (if (get-in state [:place-state :red :heart :looked])
               (assoc choices :squish (fn [state]
                                        (println "you squish the disgusting heart and it splatters all over")
                                        (assoc-in state [:place-state :red :heart :squished] true)))
               choices))])))}

    :green
    {:description
     (fn [state]
       ["the room is green"])
     :choices
     (fn [state])}

    :blue
    {:description
     (fn [state]
       ["the room is blue"])
     :choices
     (fn [state])}}})

(defn new-game
  []
)

(defn make-choice
  [game state]
  (let [place-key (get state :place)
        previous-place (get state :previous-place)
        state (assoc state :previous-place place-key)
        place (get-in game [:places place-key])
        choices ((get place :choices) state)]
    (if (not= place-key previous-place)
      (doseq [line ((get place :description) state)]
        (println line)))
    (println "your choices are:" (string/join ", " (map name (keys choices))))
    (let [choice (read-line)
          action (get choices (keyword choice))]
      (if action
        (action state)
        (if-let [default (get place :default)]
          (default state choice)
          (default-action state choice))))))

(defn game-engine
  [game state]
  (loop [state state]
    (let [state (make-choice game state)]
      (cond
        (get state :dead)
        (do
          (println "you are dead")
          state)
        
        (get state :place)
        (recur state)

        :else state))))

(defn intro
  [state]
  (println "welcome to portal")
  (println "what is your name?")
  (let [name (read-line)]
    (println "hello" name)
    (assoc state :name name)))

(defn starting-room
  [state]
  (print (get state :name))
  (println ", you stand in a room")
  (println "there are 3 portals before you")
  (println "a red one, a green one and a blue one"))

(defn -main
  []
  (let [game example-game
        state ((get game :initial-state) {})
        state (intro state)]
    (println (game-engine game state))))
