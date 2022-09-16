(ns portal.core
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]))

(s/def :portal/game
  (s/keys :req-un [:portal/initial-state
                   :portal/places]))
(s/def :portal/initial-state fn?)
(s/def :portal/places
  (s/every-kv keyword? :portal/place-map))
(s/def :portal/place-map
  (s/keys :req-un [:portal/description
                   :portal/choices]))
(s/def :portal/description fn?)
(s/def :portal/choices
  (s/every-kv keyword? :portal/choice-map))
(s/def :portal/choice-map
  (s/keys :req-un [:portal/action]
          :opt-un [:portal/condition]))
(s/def :portal/action fn?)
(s/def :portal/condition fn?)

(s/def :portal/state
  (s/keys :req-un [:portal/place
                   :portal/place-state
                   :portal/inventory
                   :portal/name]
          :opt-un [:portal/previous-place
                   :portal/dead]))
(s/def :portal/place keyword?)
(s/def :portal/place-state map?)
(s/def :portal/inventory map?)
(s/def :portal/name string?)
(s/def :portal/previous-place keyword?)

(s/def :portal/choice string?)
(s/def :portal/player-choices
  (s/every-kv keyword? fn?))

(defn default-action
  [state choice]
  {:pre [(s/valid? :portal/state state)
         (s/valid? :portal/choice choice)]
   :post [(fn [x] (s/valid? :portal/state x))]}

  (let [tokens (string/split choice #" +")
        command (first tokens)
        arguments (rest tokens)
        place-key (get state :place)]
    (cond
      (= command "die")
      (assoc state :dead true)

      (#{"inventory" "I" "i" "inv"} command)
      (do
        (println "your inventory is:" (string/join ", " (map name (keys (get state :inventory)))))
        state)

      (#{"look" "l" "L"} command)
      (dissoc state :previous-place)
      
      (#{"drop" "D" "d"} command)
      (let [item-key (keyword (first arguments))
            item (get-in state [:inventory item-key])]
        (if (not item)
          (do
            (println "you don't have" (name item-key))
            state)
          (let [state (update state :inventory dissoc item-key)]
            (println "you dropped" (name item-key))
            (assoc-in state [:place-state place-key :items item-key] item))))

      (#{"take" "t" "T"} command)
      (let [item-key (keyword (first arguments))
            item (get-in state [:place-state place-key :items item-key])]
        (if (not item)
          (do
            (println "there is no" (name item-key) "here.")
            state)
          (let [state (update-in state [:place-state place-key :items] dissoc item-key)]
            (println "you took" (name item-key))
            (assoc-in state [:inventory item-key] item))))

      :else
      (do 
        (println command "is not a valid choice")
        state))))

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
     {:red
      {:action
       (fn [state]
         (assoc state :place :red))}
      :green
      {:action
       (fn [state]
         (assoc state :place :green))}
      :blue
      {:action
       (fn [state]
         (assoc state :place :blue))}}}

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
     {:case
      {:action
       (fn [state]
         (println "the case is foggy, but you can see it can be lifted")
         (assoc-in state [:place-state :red :case :looked] true))}
      :heart
      {:action
       (fn [state]
         (if (get-in state [:place-state :red :heart :squished])
           (if (get-in state [:place-state :red :heart :key])
             (do
               (println "the squished up heart has been mashed around severely")
               state)
             (do
               (println "the once beating heart is brutally squished")
               (println "you see something glinting in the mush")
               (assoc-in state [:place-state :red :heart :glint] true)))
           (do
             (println "the heart is huge and grotesque and pumping obscenely")
             (assoc-in state [:place-state :red :heart :looked] true))))}
      :lift
      {:condition
       (fn [state]
         (get-in state [:place-state :red :case :looked]))
       :action
       (fn [state]
         (println "you lift the case and release poison gas")
         (assoc state :dead true))}
      :glint
      {:condition
       (fn [state]
         (and
          (get-in state [:place-state :red :heart :glint])
          (not (get-in state [:place-state :red :heart :key]))))
       :action
       (fn [state]
         (println "you root around and find a small golden key")
         (let [state (assoc-in state [:inventory :gold-key] {})]
           (assoc-in state [:place-state :red :heart :key] true)))}
      :squish
      {:condition
       (fn [state]
         (and
          (get-in state [:place-state :red :heart :looked])
          (not (get-in state [:place-state :red :heart :squished]))))
       :action
       (fn [state]
         (println "you squish the disgusting heart and it splatters all over")
         (assoc-in state [:place-state :red :heart :squished] true))}}}

    :green
    {:description
     (fn [state]
       ["the room is a long hallway of glowing green"
        "at the end of the hall is a jet black doorway"])
     :choices
     {:door
      {:action
       (fn [state]
         (println "you walk along the green hallway but the door doesn't get any closer")
         (println "you turn around but find that you haven't moved at all")
         state)}}}

    :blue
    {:description
     (fn [state]
       ["the room is blue"])
     :choices
     {}}}})

(defn player-choices
  "given all possible choices, determine which choices are available,
   given the state."
  [choices state]
  {:pre [(s/valid? :portal/choices choices)
         (s/valid? :portal/state state)]
   :post [(fn [x] (s/valid? :portal/player-choices x))]}
  (let [passing
        (filter
         (fn [[choice action]]
           (let [condition (get action :condition)]
             (or (nil? condition)
                 (condition state))))
         choices)

        actions
        (map
         (fn [[choice action]]
           [choice (get action :action)])
         passing)]
    (into {} actions)))

(defn make-choice
  [game state]
  {:pre [(s/valid? :portal/game game)
         (s/valid? :portal/state state)]
   :post [(fn [x] (s/valid? :portal/state x))]}
  (let [place-key (get state :place)
        previous-place (get state :previous-place)
        state (assoc state :previous-place place-key)
        place (get-in game [:places place-key])
        all-choices (get place :choices) 
        choices (player-choices all-choices state)]
    (if (not= place-key previous-place)
      (do
        (doseq [line ((get place :description) state)]
          (println line))
        (let [items (get-in state [:place-state place-key :items])]
          (if (not (empty? items))
            (println "the items here are:" (string/join ", " (map name (keys items))))))))
    (println "your choices are:" (string/join ", " (map name (keys choices))))
    (let [choice (read-line)
          action (get choices (keyword choice))]
      (if action
        (action state)
        (if-let [default (get place :default)]
          (default state choice)
          (default-action state choice))))))

(defn game-engine
  [game initial-state]
  (loop [state initial-state]
    (let [new-state (make-choice game state)]
      (cond
        (get new-state :dead)
        (do
          (println "you are dead")
          new-state)
        
        (get new-state :place)
        (recur new-state)

        :else new-state))))

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
