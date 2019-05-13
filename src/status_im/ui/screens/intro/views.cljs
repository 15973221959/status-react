(ns status-im.ui.screens.intro.views
  (:require-macros [status-im.utils.views :refer [defview letsubs]])
  (:require [status-im.ui.components.react :as react]
            [re-frame.core :as re-frame]
            [status-im.react-native.resources :as resources]
            [taoensso.timbre :as log]
            [status-im.ui.components.colors :as colors]
            [reagent.core :as r]
            [status-im.ui.components.toolbar.actions :as actions]
            [status-im.ui.components.common.common :as components.common]
            [status-im.ui.components.numpad.views :as numpad]
            [status-im.ui.screens.intro.styles :as styles]
            [status-im.ui.components.toolbar.view :as toolbar]
            [status-im.i18n :as i18n]
            [status-im.ui.components.status-bar.view :as status-bar]
            [status-im.ui.screens.privacy-policy.views :as privacy-policy]))

(def margin 24)

(defn intro-viewer [slides window-width]
  (let [view-width  (- window-width (* 2 margin))
        scroll-x (r/atom 0)
        scroll-view-ref (atom nil)
        max-width (* view-width (dec (count slides)))]
    (fn []
      [react/view {:style {:margin-horizontal 32
                           :align-items :center}}
       [react/scroll-view {:horizontal true
                           :paging-enabled true
                           :ref #(reset! scroll-view-ref %)
                           :shows-vertical-scroll-indicator false
                           :shows-horizontal-scroll-indicator false
                           :pinch-gesture-enabled false
                           :on-scroll #(let [x (.-nativeEvent.contentOffset.x %)
                                             _ (log/info "#scroll" x view-width)]
                                         (cond (> x max-width)
                                               (.scrollTo @scroll-view-ref (clj->js {:x 0}))
                                               (< x 0)
                                               (.scrollTo @scroll-view-ref (clj->js {:x max-width}))
                                               :else (reset! scroll-x x)))
                           :style {:width view-width
                                   :margin-vertical 32}}
        (for [s slides]
          ^{:key (:title s)}
          [react/view {:style {:width view-width}}
           [react/view {:style styles/intro-logo-container}
            [components.common/image-contain
             {:container-style {}}
             {:image (:image s) :width view-width  :height view-width}]]
           [react/i18n-text {:style styles/wizard-title :key (:title s)}]
           [react/i18n-text {:style styles/wizard-text
                             :key   (:text s)}]])]
       [react/view {:style {:flex-direction :row
                            :justify-content :space-between
                            :align-items :center
                            :height 6
                            :width (+ 6 (* (+ 6 10) (dec (count slides))))}}
        (doall
         (for [i (range (count slides))]
           ^{:key i}
           [react/view {:style {:background-color
                                (if (= i (/ @scroll-x view-width)) colors/blue (colors/alpha colors/blue 0.2))
                                :width 6 :height 6
                                :border-radius 3}}]))]])))

(defview intro []
  (letsubs [;{window-width :width window-height :height} [:dimensions/window]
            window-width [:dimensions/window-width]]
    [react/view {:style styles/intro-view}
     [status-bar/status-bar {:flat? true}]
     [intro-viewer [{:image (:intro1 resources/ui)
                     :title :intro-title1
                     :text :intro-text1}
                    {:image (:intro2 resources/ui)
                     :title :intro-title2
                     :text :intro-text2}
                    {:image (:intro3 resources/ui)
                     :title :intro-title3
                     :text :intro-text3}] window-width]
     [react/view {:flex 1}]
     [react/view styles/buttons-container
      [components.common/button {:button-style {:flex-direction :row}
                                 :on-press     #(re-frame/dispatch [:accounts.create.ui/intro-wizard])
                                 :label        (i18n/label :t/get-started)}]
      [react/view styles/bottom-button-container
       [components.common/button {:on-press    #(re-frame/dispatch [:accounts.recover.ui/recover-account-button-pressed])
                                  :label       (i18n/label :t/access-key)
                                  :background? false}]]
      [react/i18n-text {:style styles/welcome-text-bottom-note :key :intro-privacy-policy-note}]
      #_[privacy-policy/privacy-policy-button]]]))

(defn generate-key []
  [components.common/image-contain
   {:container-style {}}
   {:image (:sample-key resources/ui)
    :width 154 :height 140}])

(defn choose-key [])

(defn select-key-storage [])

(defn create-code []
  [react/view
   [numpad/number-pad {:on-press #(re-frame/dispatch [:intro-wizard/code-digit-pressed %])}]
   [react/text {:style styles/wizard-bottom-note} (i18n/label :t/you-will-need-this-code)]])

(defn confirm-code []
  [react/view
   [numpad/number-pad {:on-press #(re-frame/dispatch [:intro-wizard/code-digit-pressed %])}]
   [react/text {:style styles/wizard-bottom-note} (i18n/label :t/you-will-need-this-code)]])

(defn enable-fingerprint [])

(defn enable-notifications [])

(defview wizard []
  (letsubs [{:keys [step]} [:intro-wizard]]
    [react/view {:style {:flex 1}}
     [toolbar/toolbar
      nil
      (when-not (= :finish step)
        (toolbar/nav-button
         (actions/back #(re-frame/dispatch
                         [:intro-wizard/step-back-pressed]))))
      nil]
     [react/view {:style {:flex 1
                          :margin-horizontal 32
                          :justify-content :space-between}}
      [react/view {:style {:margin-top   16}}

       [react/text {:style styles/wizard-title} (i18n/label (keyword (str "intro-wizard-title" step)))]
       [react/text {:style styles/wizard-text} (i18n/label (keyword (str "intro-wizard-text" step)))]]
      (case step
        1 [generate-key]
        2 [choose-key]
        3 [select-key-storage]
        4 [create-code]
        5 [confirm-code]
        6 [enable-fingerprint]
        7 [enable-notifications])
      [react/view {:style {:margin-bottom 32}}
       [components.common/button {:button-style styles/intro-button
                                  ;:disabled?    disable-button?
                                  :on-press     #(re-frame/dispatch
                                                  [:intro-wizard/step-forward-pressed])
                                  :label        (i18n/label :generate-a-key)}]
       [react/text {:style styles/wizard-bottom-note}
        (i18n/label :t/this-will-take-few-seconds)]]]]))
