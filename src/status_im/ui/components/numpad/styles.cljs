(ns status-im.ui.components.numpad.styles
  (:require
   [status-im.ui.components.colors :as colors]))

(def vertical-number-separator
  {:flex      1
   :min-width 16
   :max-width 32})

(def number
  {:font-size   22
   :color       colors/blue})

(defn horizontal-separator
  [min-height max-height]
  {:flex       1
   :min-height min-height
   :max-height max-height})

(def number-row
  {:flex-direction :row})

(def number-pad
  {:flex          1
   :align-items   :center
   :margin-bottom 24
   :min-height    292
   :max-height    328})

(def number-container
  {:width            64
   :height           64
   :border-radius    32
   :justify-content  :center
   :align-items      :center
   :background-color colors/blue-light})

