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
