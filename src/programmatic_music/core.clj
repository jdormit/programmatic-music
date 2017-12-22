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
