(ns programmatic-music.sequencer
  (:require [overtone.live :refer [at apply-by]]))

(defrecord Sequencer [step-index step-count step-values step-mapping steps-enabled])

(defn make-sequencer*
  "Makes a step sequencer.

  `step-count` is the number of steps in the sequence (the sequence x-axis).

  `step-values` is a vector or a map describing the
  values available for any step (the sequence y-axis)

  `:step-mapping` is a map describing which value to use for which step.
  The keys should be numbers representing the step number (0-indexed),
  and the values are indices into the intial-step-values parameter (numbers if it is
  a vector, or keys if it is a map).

  `:steps-enabled` is a set of step indexes which should be enabled."
  [step-count step-values step-mapping steps-enabled]
  (->Sequencer (atom 0) step-count (atom step-values) (atom step-mapping) (atom steps-enabled)))

(defn step
  "Executes the next step in a sequencer"
  [sequencer]
  (let [{:keys [step-index step-count step-values step-mapping steps-enabled]} sequencer
        step-func-key (get @step-mapping @step-index nil)]
    (when (and (contains? @steps-enabled @step-index)
               step-func-key)
      ((@step-values step-func-key)))
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
  [sequencer step func-key]
  (let [{:keys [step-values step-mapping]} sequencer]
    (if (nil? func-key)
      (swap! step-mapping assoc step func-key)
      (if (nil? (get @step-values func-key nil))
        (println "Key not in step values:" func-key)
        (swap! step-mapping assoc step func-key)))))

(defn sequencer->str
  "Returns a textual representation of a sequencer"
  [sequencer]
  "TODO: Implement me!")

#_(defmacro make-sequencer)
