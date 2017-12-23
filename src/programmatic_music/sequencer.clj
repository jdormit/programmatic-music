(ns programmatic-music.sequencer
  (:require [overtone.live :refer [at apply-by]]))

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
  (let [step-index (atom 0)
        step-values (atom step-values)
        step-mapping (atom step-mapping)
        steps-enabled (atom steps-enabled)]
    {:step (fn []
             (let [current-step @step-index
                   step-func-key (get @step-mapping current-step nil)]
               (when (contains? @steps-enabled current-step)
                 (if (nil? step-func-key)
                   (println "No value mapped for step" current-step)
                   ((@step-values step-func-key))))
               (reset! step-index
                       (mod (inc @step-index) step-count))))
     :stop (fn []
             (reset! steps-enabled []))
     :enable-step (fn [step]
                    (swap! steps-enabled conj step))
     :disable-step (fn [step]
                     (swap! steps-enabled disj step))
     :set-value (fn [step func-key]
                  (if (nil? (get @step-values func-key nil))
                    (println "Key not in step values:" func-key)
                    (swap! step-mapping assoc step func-key)))
     :print (fn []
              (str "TODO: Implement me!"))}))

#_(defmacro make-sequencer)

(defn play-sequencer [nome sequencer]
  (let [beat (nome)]
    (at (nome beat) ((:step sequencer)))
    (apply-by (nome (inc beat)) play-sequencer [nome sequencer])))
