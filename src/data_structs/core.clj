(ns data-structs.core
  (:require [clojure.set :as cset]))

(defn expand
  [the-vector
   distance
   length]
  (let [end (count the-vector)
        start (- end distance)
        ;; Here we go backwards 'distance' elements.
        pattern (subvec the-vector
                        start
                        end)] ;; We have our pattern
    (into [] (take length (cycle pattern)))))

(defn un-LZ77
  [bytes]
  (loop [result []
         remaining bytes]
    (if (seq remaining)
      (let [current (first remaining)
            the-rest (rest remaining)]
        (if-not (vector? current)
          (recur (conj result
                       current)
                 the-rest)
          (recur (into result (expand result
                                      (first current)
                                      (second current)))
                 the-rest)))
      result)))

(defn all-subvecs-from-beginning
  [v]
  (set (map #(subvec v 0 %)
            (range 1 (inc (count v))))))

(defn all-subvecs
  [v]
  (loop [result #{}
         remaining v]
    (if (seq remaining)
      (recur (into result
                   (all-subvecs-from-beginning remaining))
             (into [] (rest remaining)))
      result)))

(defn longest-match-w-beginning
  [left-array right-array]
  (let [all-left-chunks (all-subvecs left-array)
        all-right-chunks-from-beginning (all-subvecs-from-beginning right-array)
        all-matches (cset/intersection all-right-chunks-from-beginning
                                       all-left-chunks)]
    (->> all-matches
         (sort-by count >)
         first)))

(defn pos-of-subvec
  [sv v]
  {:pre [(<= (count sv)
             (count v))]}
  (loop [cursor 0]
    (if (or (empty? v)
            (empty? sv)
            (= cursor (count v)))
      nil
      (if (= (subvec v cursor
                     (+ (count sv)
                        cursor))
             sv)
        cursor
        (recur (inc cursor))))))

(defn LZ77-STEP
  [window look-ahead]
  (let [longest (longest-match-w-beginning window look-ahead)]
    (if-let [pos-subv-w (pos-of-subvec longest window)]
      (let [distance (- (count window) pos-subv-w)
            pos-subv-l (pos-of-subvec longest look-ahead)
            the-char (first (subvec look-ahead
                                    (+ pos-subv-l
                                       (count longest))))]
        {:distance distance
         :length (count longest)
         :char the-char})
      {:distance 0
       :length 0
       :char (first look-ahead)})))

