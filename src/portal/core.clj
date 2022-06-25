(ns portal.core)

(def example-game
  {:places
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
       ["the room is red"])
     :choices
     (fn [state])}

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
  {:place :starting})

(defn make-choice
  [game state]
  (println game)
  (println state)
  (let [place-key (get state :place)
        place (get-in game [:places place-key])
        choices ((get place :choices) state)]
    (doseq [line ((get place :description) state)]
      (println line))
    (println "your choices are:" (keys choices))
    (let [choice (read-line)
          action (get choices (keyword choice))]
      (action state))))

(defn game-engine
  [game state]
  (loop [state state]
    (let [state (make-choice game state)]
      (if (get state :place)
        (recur state)
        state))))

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
  (let [state (new-game)
        state (intro state)]
    (println (game-engine example-game state))))
