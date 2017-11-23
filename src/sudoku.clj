(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board [row col]]
  (let [func (fn [y]
         (value-at board [row y]))]
    (set (map func (range 9)))))

(defn col-values [board [row col]]
  (let [func (fn [x]
         (value-at board [x col]))]
    (set (map func (range 9)))))


(defn coord-pairs [coords]
  (for [x coords y coords]
    [x y]))

(defn block-top-left-corner [[x y]]
  [(- x (mod x 3)) (- y (mod y 3))])

(defn block-values [board coords]
  (let [[top-left-row top-left-col] (block-top-left-corner coords)
        helper (fn [coordinates] (value-at board coordinates))]
    (set (map helper
      (for [x (range top-left-row (+ top-left-row 3))
            y (range top-left-col (+ top-left-col 3))]
        [x y])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    (set nil)
    (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))))

(defn board-in-a-line [board]
 (apply concat board))

(defn filled? [board]
  (not (contains? (set (board-in-a-line board)) 0)))


(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (let [helper (fn [x] (= all-values (set x)))]
    (every? identity (map helper (rows board)))))

(defn cols [board]
  (let [helper (fn [coords] (col-values board coords))]
    (map helper (for [y (range 9)] [0 y]))))

(defn valid-cols? [board]
  (let [helper (fn [x] (= all-values (set x)))]
    (every? identity (map helper (cols board)))))

(defn blocks [board]
  (let [helper (fn [coords] (block-values board coords))]
    (map helper (coord-pairs (range 0 9 3)))))

(defn valid-blocks? [board]
  (let [helper (fn [x] (= all-values (set x)))]
    (every? identity (map helper (blocks board)))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (for [x (range 0 9)
        y (range 0 9)
        :when (not (has-value? board [x y]))] [x y])))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      nil)
    (let [empty-point (find-empty-point board)
          valid-values (valid-values-for board empty-point)]
      (for [value valid-values
            solution (solve-helper (set-value-at board empty-point value))]
        solution))))


(defn solve [board]
  (first (solve-helper board)))



