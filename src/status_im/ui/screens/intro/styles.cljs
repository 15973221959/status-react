(ns status-im.ui.screens.intro.styles
  (:require-macros [status-im.utils.styles :refer [defnstyle defstyle]])
  (:require [status-im.ui.components.colors :as colors]))

(def intro-view
  {:flex               1
   :justify-content    :flex-end
   :padding-horizontal 30})

(def intro-logo-container
  {:align-items     :center
   :justify-content :center})

(defn dot-selector [n]
  {:flex-direction  :row
   :justify-content :space-between
   :align-items     :center
   :height          6
   :width           (+ 6 (* (+ 6 10) (dec n)))})

(defn dot [color selected?]
  {:background-color (if selected?
                       color
                       (colors/alpha color 0.2))
   :width            6
   :height           6
   :border-radius    3})

(def welcome-image-container
  {:align-items :center
   :margin-top  42})

(def wizard-title
  {:font-size     22
   :line-height   28
   :text-align    :center
   :font-weight   "600"
   :margin-bottom 16})

(def wizard-text
  {:font-size   15
   :line-height 22
   :color       colors/gray
   :text-align  :center})

(def welcome-text
  {:typography  :header
   :margin-top  32
   :text-align  :center})

(def welcome-text-bottom-note
  {:font-size   12
   :line-height 14
   :color       colors/gray
   :text-align  :center})

(defn list-item [selected?]
  {:flex-direction   :row
   :align-items      :center
   :padding-left     16
   :padding-right    10
   :background-color (if selected? colors/blue-light colors/white)
   :padding-vertical 10})

(def account-image
  {:width         40
   :height        40
   :border-radius 20
   :border-width  1
   :border-color  (colors/alpha colors/black 0.1)})

(def welcome-text-description
  {:margin-top        8
   :text-align        :center
   :margin-horizontal 32
   :color             colors/gray})

(def intro-logo
  {:size      111
   :icon-size 46})

(def password-text-input
  {:font-size    20
   :margin-top   60
   :line-height  24
   :font-weight  "600"})

(defstyle intro-text
  {:text-align  :center
   :font-weight "700"
   :font-size   24})

(def intro-text-description
  {:margin-top    8
   :margin-bottom 16
   :text-align    :center
   :color         colors/gray})

(def buttons-container
  {:align-items :center
   :margin-top  32})

(def bottom-button
  {:padding-horizontal 24
   :justify-content    :center
   :align-items        :center
   :flex-direction     :row})

(def bottom-button-container
  {:margin-bottom 24
   :margin-top    16})

(def bottom-arrow
  {:flex-direction   :row
   :justify-content  :flex-end
   :align-self       :stretch
   :padding-top      16
   :border-top-width 1
   :border-top-color colors/gray-lighter
   :margin-right     20})
