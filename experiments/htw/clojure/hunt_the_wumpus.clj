(import '(java.util Random))

(def width 10)
(def cell-count 100)
(def pits #{"O12" "O27" "O44" "O67" "O81" "I18" "I33" "I58" "I72" "I96"})
(def bats #{"O5" "O20" "O46" "O70" "O88" "I9" "I24" "I31" "I63" "I94"})
(def command-help
  "Commands: move <room>, shoot <r1> <r2> ... <r5>, status, help, quit")

(defn room-torus [room]
  (subs room 0 1))

(defn room-index [room]
  (Integer/parseInt (subs room 1)))

(defn make-room [torus index]
  (str torus index))

(defn cell [row col]
  (+ (* (mod row width) width) (mod col width)))

(defn bridge-room [room rotation]
  (let [index (room-index room)]
    (if (= (room-torus room) "O")
      (make-room "I" (mod (+ index rotation) cell-count))
      (make-room "O" (mod (- index rotation) cell-count)))))

(defn cave [room rotation]
  (let [index (room-index room)
        row (quot index width)
        col (mod index width)
        torus (room-torus room)]
    [(make-room torus (cell row (inc col)))
     (make-room torus (cell (inc row) col))
     (make-room torus (cell row (dec col)))
     (make-room torus (cell (dec row) col))
     (bridge-room room rotation)]))

(defn adjacent? [from to rotation]
  (some #{to} (cave from rotation)))

(defn random-room [rng]
  (make-room (if (.nextBoolean rng) "O" "I") (.nextInt rng cell-count)))

(defn random-neighbor [rng room rotation]
  (nth (cave room rotation) (.nextInt rng 5)))

(defn move-wumpus [rng room rotation]
  (let [roll (.nextInt rng 4)]
    (if (zero? roll)
      room
      (nth (cave room rotation) (dec roll)))))

(defn room-warnings [{:keys [player wumpus rotation]}]
  (let [neighbors (cave player rotation)]
    (concat
      [(format "Tunnels lead to %s" (clojure.string/join ", " neighbors))]
      (when (some #{wumpus} neighbors) ["You smell a Wumpus."])
      (when (some pits neighbors) ["You feel a draft."])
      (when (some bats neighbors) ["You hear rustling bats."]))))

(defn parse-command [line]
  (let [[verb & args] (remove clojure.string/blank? (clojure.string/split (clojure.string/trim line) #"\s+"))]
    {:verb (or verb "") :args args}))

(defn parse-room [token]
  (when (and token (<= 2 (count token)))
    (let [prefix (subs token 0 1)
          index-text (subs token 1)]
      (when (#{"O" "I"} prefix)
        (try
          (let [index (Integer/parseInt index-text)]
            (when (<= 0 index 99)
              (make-room prefix index)))
          (catch Exception _ nil))))))

(defn apply-bats [rng room]
  (if (bats room) (random-room rng) room))

(defn shoot [rng {:keys [player wumpus rotation]} path]
  (loop [arrow-room player
         remaining (take 5 path)]
    (if-let [requested (first remaining)]
      (let [next-room (if (adjacent? arrow-room requested rotation)
                        requested
                        (random-neighbor rng arrow-room rotation))]
        (cond
          (= next-room wumpus) :won
          (= next-room player) :lost-self
          :else (recur next-room (rest remaining))))
      :miss)))

(defn room-outcome [{:keys [player wumpus]}]
  (cond
    (= player wumpus) [:lost "You enter the Wumpus room. You lose."]
    (pits player) [:lost "You fall into a bottomless pit. You lose."]
    :else nil))

(defn print-room [state]
  (println)
  (println "You are in room" (:player state))
  (doseq [line (room-warnings state)]
    (println line))
  (println "Arrows:" (:arrows state) "| command: move <room>, shoot <r1> <r2> ... <r5>, status, help, quit"))

(defn step-into-room [rng state room]
  (assoc state :player (apply-bats rng room)))

(defn rotate-state [state]
  (update state :rotation #(mod (+ % width) cell-count)))

(defn make-rng [args]
  (if-let [seed-text (first args)]
    (try
      (Random. (Long/parseLong seed-text))
      (catch Exception _ (Random.)))
    (Random.)))

(defn play [args]
  (let [rng (make-rng args)]
    (println "Hunt the Wumpus")
    (println "Double torus rules: O0..O99 and I0..I99 with a rotating inner bridge.")
    (println command-help)
    (loop [state {:player "O0" :wumpus "O55" :arrows 5 :rotation 0}]
      (if-let [[status message] (room-outcome state)]
        (do (println message) status)
        (let [state (if (bats (:player state))
                      (do
                        (println "Super bats whisk you to another room!")
                        (assoc state :player (random-room rng)))
                      state)]
          (if-let [[status message] (room-outcome state)]
            (do (println message) status)
            (do
              (print-room state)
              (let [{:keys [verb args]} (parse-command (or (read-line) ""))]
                (cond
                  (= verb "quit")
                  (do (println "Goodbye.") :quit)

                  (= verb "help")
                  (do (println command-help) (recur state))

                  (= verb "status")
                  (do
                    (println "Status:" (str "room=" (:player state)
                                            ", arrows=" (:arrows state)
                                            ", rotation=" (:rotation state)
                                            ", tunnels=" (cave (:player state) (:rotation state))))
                    (recur state))

                  (= verb "move")
                  (if-let [room (some-> (first args) parse-room)]
                    (if (adjacent? (:player state) room (:rotation state))
                      (do
                        (println "You move to room" room)
                        (recur (rotate-state (step-into-room rng state room))))
                      (do
                        (println "You can only move to an adjacent room.")
                        (recur state)))
                    (do
                      (println "You must name an adjacent room.")
                      (recur state)))

                  (= verb "shoot")
                  (let [path (keep parse-room args)]
                    (if (seq path)
                      (case (shoot rng state path)
                        :won (do (println "Your arrow strikes true. You win!") :won)
                        :lost-self (do (println "The crooked arrow returns to your room. You lose.") :lost)
                        (let [next-state (-> state
                                             rotate-state
                                             (update :arrows dec)
                                             (assoc :wumpus (move-wumpus rng (:wumpus state) (:rotation (rotate-state state)))))]
                          (println "Your arrow misses.")
                          (cond
                            (= (:player next-state) (:wumpus next-state))
                            (do (println "The Wumpus wakes, moves, and eats you.") :lost)

                            (zero? (:arrows next-state))
                            (do (println "You are out of arrows. You lose.") :lost)

                            :else
                            (recur next-state))))
                      (do
                        (println "Provide one to five rooms for the arrow path.")
                        (recur state))))

                  :else
                  (do
                    (println "Unknown command.")
                    (recur state)))))))))))

(defn -main [& args]
  (play args))

(-main)
