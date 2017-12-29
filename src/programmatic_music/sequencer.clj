(ns programmatic-music.sequencer
  (:require [overtone.live :refer [at apply-by]]
            [clojure.core.match :refer [match]]))

(defrecord Sequencer [step-index step-count step-mapping steps-enabled])

(defn make-sequencer*
  "Makes a step sequencer.

  `step-count` is the number of steps in the sequence (the sequence x-axis).

  `:step-mapping` maps step indices to functions to be called for that step.

  `:steps-enabled` is a set of step indexes which should be enabled."
  [step-count step-mapping steps-enabled]
  (->Sequencer (atom 0) step-count (atom step-mapping) (atom steps-enabled)))

(defn step
  "Executes the next step in a sequencer"
  [sequencer]
  (let [{:keys [step-index step-count step-mapping steps-enabled]} sequencer]
    (when (contains? @steps-enabled @step-index)
      (when-let [func (@step-mapping @step-index)]
        (func)))
    (reset! step-index
            (mod (inc @step-index) step-count))))

(defn loop-sequencer
  ;; TODO how would you stop the loop without disabling the sequencer?
  ;; You could make this a macro that defines a var in the current environment
  ;; that can be manipulated
  "Loops through the sequencer according to a metronome beat"
  [nome sequencer]
  (let [beat (nome)]
    (at (nome beat) (step sequencer))
    (apply-by (nome (inc beat)) loop-sequencer [nome sequencer])))

(defn disable
  "Disables all steps in a sequencer"
  [sequencer]
  (let [{:keys [steps-enabled]} sequencer]
    (reset! steps-enabled #{})))

(defn enable
  "Enables all steps in a sequencer"
  [sequencer]
  (let [{:keys [steps-enabled step-count]} sequencer]
    (reset! steps-enabled (set (range 0 step-count)))))

(defn enable-step
  "Enables a step in a sequencer"
  [sequencer step]
  (let [{:keys [steps-enabled]} sequencer]
    (swap! steps-enabled conj step)))

(defn disable-step
  "Disables a step in a sequencer"
  [sequencer step]
  (let [{:keys [steps-enabled]} sequencer]
    (swap! steps-enabled disj step)))

(defn set-step
  "Sets the value of a step in a sequencer"
  [sequencer step func]
  (let [{:keys [step-mapping]} sequencer]
    (swap! step-mapping assoc step func)))

(defn sequencer->str
  "Returns a textual representation of a sequencer"
  [sequencer]
  "TODO: Implement me!")

(defn make-sequencer-helper
  "Helper function for the `make-sequencer` macro.

  It processes the sequencer list representation row-by-row recursively."
  [[current-form & rest-seq-list :as seq-list] row-count step-mapping]
  (match current-form
         (:or '. '!) (loop [[first & rest] seq-list
                            idx 0
                            steps-enabled #{}]
                       (cond
                         (nil? row-count) (throw
                                           (RuntimeException.
                                            "Sequencer must have at least one row"))
                         (> idx row-count) (throw
                                            (RuntimeException. "Row size does not match"))
                         (nil? first) `(make-sequencer* ~row-count ~step-mapping ~steps-enabled)
                         :else (match first
                                      '. (recur rest (inc idx) steps-enabled)
                                      '! (recur rest (inc idx) (conj steps-enabled idx))
                                      x (throw (RuntimeException.
                                                (str "Illegal sequencer expression. "
                                                     "Expected . or !, found "
                                                     x "."))))))
         func-name (let [[row-step-map remaining-rows row-size]
                         (loop [[first & rest :as remaining-rows] rest-seq-list
                                row-size (or row-count 0)
                                first-row? (nil? row-count)
                                idx 0
                                acc step-mapping]
                           (if (and (not first-row?) (> idx row-size))
                             (throw (RuntimeException. "Row size does not match"))
                             (match first
                                    '- (recur rest
                                              (if first-row? (inc row-size) row-size)
                                              first-row?
                                              (inc idx)
                                              acc)
                                    'o (recur rest
                                              (if first-row? (inc row-size) row-size)
                                              first-row?
                                              (inc idx)
                                              (assoc acc idx func-name))
                                    _ [acc remaining-rows row-size])))]
                     (recur remaining-rows row-size row-step-map))))

(defmacro make-sequencer
  "Makes a sequencer from a visual representation.

  Example usage: (make-sequencer (func1 - - - o
                                  func2 - - - -
                                  func3 o - o -
                                        . ! ! !))
  TODO: write extended description"
  [seq-list]
  (make-sequencer-helper seq-list nil {}))
