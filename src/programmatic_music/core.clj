(ns programmatic-music.core
  (:require [overtone.live :refer :all]
            [overtone.inst.drum :as drum]))

(definst sin-wave [note 60 attack 0.01 sustain 0.4 release 0.2 vol 0.4]
  (let [freq (midicps note)]
    (* (env-gen (asr attack sustain release) 1 1 0 1 FREE)
       (saw freq)
       vol)))

(defn play-chord [a-chord]
  (doseq [note a-chord] (sin-wave (midi->hz note))))

(defn play-seq [notes nome inst]
  (if (not (nil? notes))
    (let [beat (nome)
          note (first notes)]
      (at (nome beat) (inst (midi->hz note)))
      (apply-by (nome (inc beat)) #'play-seq [(rest notes) nome inst]))))

(defn make-sequencer
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
                    (swap! step-mapping assoc step func-key)))}))

(defn play-sequencer [nome sequencer]
  (let [beat (nome)]
    (at (nome beat) ((:step sequencer)))
    (apply-by (nome (inc beat)) play-sequencer [nome sequencer])))
