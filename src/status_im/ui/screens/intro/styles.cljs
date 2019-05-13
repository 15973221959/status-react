(ns status-im.ui.screens.intro.styles
  (:require-macros [status-im.utils.styles :refer [defnstyle defstyle]])
  (:require [status-im.ui.components.colors :as colors]))

(def intro-view
  {:flex               1
   :padding-horizontal 30})

(def intro-logo-container
  {;:flex            1
   :align-items     :center
   :justify-content :center
   ;:margin-bottom 110
})

(def welcome-image-container
  {:align-items :center
   :margin-top  42})

(def intro-button
  {:margin-vertical    8
   :padding-horizontal 32
   :align-self         :center
   :justify-content    :center
   :align-items        :center})

(def wizard-title
  {:font-size 22
   :line-height 28
   :text-align :center
   :font-weight "600"
   :margin-bottom 16})

(def wizard-text
  {:font-size 15
   :line-height 22
   :color colors/gray
   :text-align :center})

(def welcome-text
  {:typography  :header
   :margin-top  32
   :text-align  :center})

(def welcome-text-bottom-note
  {:font-size   12
   :line-height 14
   :color       colors/gray
   :text-align  :center})

(def wizard-bottom-note
  {:font-size 15
   :line-height 22
   :margin-top 20
   :color colors/gray
   :text-align :center})

(def welcome-text-description
  {:margin-top        8
   :text-align        :center
   :margin-horizontal 32
   :color             colors/gray})

(def intro-logo
  {:size      111
   :icon-size 46})

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
  {:align-items :center})

(def bottom-button-container
  {:margin-bottom 6
   :margin-top    16})
