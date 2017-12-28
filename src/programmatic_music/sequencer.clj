(ns programmatic-music.sequencer
  (:require [overtone.live :refer [at apply-by]]))

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

#_(defmacro make-sequencer)
