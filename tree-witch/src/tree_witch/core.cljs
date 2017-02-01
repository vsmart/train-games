(ns tree-witch.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def initial-state
  {:witch-x 20
   :witch-y 400
   :direction :right
   :game-state :play
   :parts-count 0
   :parts [{:x 20 :y 20}]})

(defn setup []
  (q/frame-rate 30)
  initial-state)

(def load-image  (memoize q/load-image))

(defn move-witch  [state by-x by-y]
  (let [new-state  (update state :witch-x + by-x)
        newer-state  (update new-state :witch-y + by-y)]
    newer-state))

(def one-move 20)
(def one-neg-move  (* one-move -1))

(def parts-generation-rate 60)

(defn move-part [part]
  (update part :y inc))

(defn move-parts [state]
  (assoc state :parts
      (into [] (map move-part (:parts state)))))

(defn move-witch-left  [state]
  (-> state
    (move-witch one-neg-move 0)
    (assoc :direction :left)))

(defn move-witch-right  [state]
  (-> state
    (move-witch one-move 0)
    (assoc :direction :right)))

(defn move-witch-up  [state]
  (-> state
    (move-witch 0 one-neg-move)))
;    (assoc :direction :up)))

(defn move-witch-down  [state]
  (-> state
    (move-witch 0 one-move)))
;    (assoc :direction :down)))

(defn move-after-key-pressed [state event]
  (let  [key (:key event)]
    (cond
      (= key :left) (move-witch-left state)
      (= key :right) (move-witch-right state)
      (= key :up) (move-witch-up state)
      (= key :down) (move-witch-down state)
      :else state)))

(defn collided? )

(defn check-for-parts-collected [state]
  (let [collided? (map collided? (:parts state)      (and
        (and (> (:witch-x state) (- (:x p) 20) ) (< (:witch-x state) (+ (:x p) 20) ))
        (and (> (:witch-y state) (- (:y p) 20) ) (< (:witch-y state) (+ (:y p) 20) ))))]
    (if collided?
      (update state :parts-count inc)
      state)))

(defn generate-parts [state]
  (if (= (mod (q/frame-count) parts-generation-rate) 0)
    (assoc state :parts (conj (:parts state) {:x (rand (q/width)) :y 50}))
    state))

(defn update-state [state]
  (-> state
    (check-for-parts-collected)
    (generate-parts)
    (move-parts)))

(defn draw-parts [state]
  (q/fill 100)
  (doseq [p (:parts state)]
    (q/image (load-image (str "images/part.png")) (:x p) (:y p))))

 (defn draw-witch [state]
  (q/image
    (load-image (str "images/witch-" (name (:direction state)) ".png")) (:witch-x state) (:witch-y state) 100 100))

(defn draw-status [state]
  (q/fill 255)
  (q/text (str state " --- " (q/frame-count)) 20 20))

(defn draw-play-state [state]
  (q/background 0 53 0)
  (draw-status state)
  (draw-witch state)
  (draw-parts state)
  (q/fill 0))

(defn draw-win-state [state]
  (q/background 206 70 145)
  (q/image (load-image (str "images/done.png")) 300 150 100 120)
  (q/text-size 30)
  (q/text "you ate the food, well done sleepy witch." 50 180 200 200))

(defn draw-state [state]
  (cond
    (= :win (:game-state state)) (draw-win-state state)
    (= :play (:game-state state)) (draw-play-state state)))

(q/defsketch tree-witch
  :host "tree-witch"
  :size [800 800]
  :setup setup
  :key-pressed move-after-key-pressed
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])
