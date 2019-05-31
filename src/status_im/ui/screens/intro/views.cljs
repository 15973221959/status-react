(ns status-im.ui.screens.intro.views
  (:require-macros [status-im.utils.views :refer [defview letsubs]])
  (:require [status-im.ui.components.react :as react]
            [re-frame.core :as re-frame]
            [status-im.react-native.resources :as resources]
            [status-im.ui.components.icons.vector-icons :as vector-icons]
            [status-im.utils.identicon :as identicon]
            [status-im.ui.components.radio :as radio]
            [taoensso.timbre :as log]
            [status-im.utils.gfycat.core :as gfy]
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
(defn dots-selector [{:keys [on-press n selected color]}]
  [react/view {:style {:flex-direction :row
                       :justify-content :space-between
                       :align-items :center
                       :height 6
                       :width (+ 6 (* (+ 6 10) (dec n)))}}
   (doall
    (for [i (range n)]
      ^{:key i}
      [react/view {:style {:background-color
                           (if (selected i)
                             color
                             (colors/alpha color 0.2))
                           :width 6 :height 6
                           :border-radius 3}}]))])
(defn intro-viewer [slides window-width]
  (let [view-width  (- window-width (* 2 margin))
        scroll-x (r/atom 0)
        scroll-view-ref (atom nil)
        max-width (* view-width (dec (count slides)))]
    (fn []
      [react/view {:style {:margin-horizontal 32
                           :align-items :center
                           :justify-content :flex-end}}
       [(react/scroll-view) {:horizontal true
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
          [react/view {:style {:width view-width
                               :padding-horizontal 16}}
           [react/view {:style styles/intro-logo-container}
            [components.common/image-contain
             {:container-style {}}
             {:image (:image s) :width view-width  :height view-width}]]
           [react/i18n-text {:style styles/wizard-title :key (:title s)}]
           [react/i18n-text {:style styles/wizard-text
                             :key   (:text s)}]])]
       (let [selected (hash-set (/ @scroll-x view-width))]
         [dots-selector {:selected selected :n (count slides)
                         :color colors/blue}])])))

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
     #_[react/view {:flex 1}]
     [react/view styles/buttons-container
      [components.common/button {:button-style styles/bottom-button
                                 :on-press     #(re-frame/dispatch [:accounts.create.ui/intro-wizard])
                                 :label        (i18n/label :t/get-started)}]
      [react/view styles/bottom-button-container
       [components.common/button {:button-style styles/bottom-button
                                  :on-press    #(re-frame/dispatch [:accounts.recover.ui/recover-account-button-pressed])
                                  :label       (i18n/label :t/access-key)
                                  :background? false}]]
      [react/i18n-text {:style styles/welcome-text-bottom-note :key :intro-privacy-policy-note}]
      #_[privacy-policy/privacy-policy-button]]]))

(defn generate-key []
  [components.common/image-contain
   {:container-style {:margin-horizontal 80}}
   {:image (resources/get-image :sample-key)
    :width 154 :height 140}])

(defn choose-key [{:keys [accounts selected-pubkey] :as wizard-state}]
  [react/view {:style {:margin-top 110}}
   (for [acc accounts]
     (let [selected? (= (:pubkey acc) selected-pubkey)]
       ^{:key (:pubkey acc)}
       [react/touchable-highlight
        {:on-press #(re-frame/dispatch [:intro-wizard/on-key-selected (:pubkey acc)])}
        [react/view {:style {:flex-direction :row
                             :align-items :center
                             :padding-left 16
                             :padding-right 10
                             :background-color (if selected? colors/blue-light colors/white)
                             :padding-vertical 10}}

         [react/image {:source {:uri (identicon/identicon (:pubkey acc))}
                       :style {:width 40 :height 40
                               :border-radius 20
                               :border-width 1
                               :border-color (colors/alpha colors/black 0.1)}}]
         [react/view {:style {:margin-horizontal 16 :flex 1}}
          [react/text {:style styles/account-name
                       :number-of-lines 1
                       :ellipsize-mode :middle}
           (gfy/generate-gfy (:pubkey acc))]
          [react/text {:style styles/account-address
                       :number-of-lines 1
                       :ellipsize-mode :middle}
           (:pubkey acc)]]
         [radio/radio selected?]]]))])

(defn select-key-storage [{:keys [accounts selected-storage-type] :as wizard-state}]
  (let [storage-types [{:type :default
                        :icon :main-icons/mobile
                        :title :this-device
                        :desc :this-device-desc}
                       {:type :advanced
                        :icon :main-icons/keycard
                        :title :keycard
                        :desc :keycard-desc}]]
    [react/view {:style {:margin-top 84}}
     (for [{:keys [type icon title desc]} storage-types]
       (let [selected? (= type selected-storage-type)]
         ^{:key type}
         [react/view
          [react/text {:style (assoc styles/account-address
                                     :margin-left 16)}
           (i18n/label type)]
          [react/touchable-highlight
           {:on-press #(re-frame/dispatch [:intro-wizard/on-key-storage-selected type])}
           [react/view {:style {:flex-direction :row
                                :align-items :center
                                :padding-left 16
                                :padding-right 10
                                :margin-top    4
                                :margin-bottom 30
                                :background-color (if selected? colors/blue-light colors/white)
                                :padding-vertical 10}}

            [react/view {:style {:align-self :flex-start}}
             [vector-icons/icon icon {:color (if selected? colors/blue colors/gray)}]]
            [react/view {:style {:margin-horizontal 16 :flex 1}}
             [react/text {:style styles/account-name
                          :number-of-lines 2
                          :ellipsize-mode :middle}
              (i18n/label title)]
             [react/text {:style styles/account-address
                          :number-of-lines 2
                          :ellipsize-mode :middle}
              (i18n/label desc)]]
            [radio/radio selected?]]]]))]))

(defn create-code [{:keys [key-code] :as wizard-state}]
  (let [selected (into (hash-set) (range (count key-code)))
        _ (log/info "key-code" key-code)]
    [react/view
     [react/view {:style {:margin-bottom 32 :align-items :center}}
      [dots-selector {:n 6 :selected selected :color colors/blue}]]
     [numpad/number-pad {:on-press #(re-frame/dispatch [:intro-wizard/code-digit-pressed %])}]
     [react/text {:style styles/wizard-bottom-note} (i18n/label :t/you-will-need-this-code)]]))

(defn confirm-code [{:keys [key-code confirm-failure?] :as wizard-state}]
  (log/info "confirm-code" key-code)
  (let [selected (into (hash-set) (range (count key-code)))
        _ (log/info "key-code" key-code)]
    [react/view
     [react/view {:style {:margin-bottom 32 :align-items :center}}
      (when confirm-failure?
        [react/text {:style (assoc styles/wizard-text :color colors/red
                                   :margin-bottom 16)}
         (i18n/label :t/passcode-error)])
      [dots-selector {:n 6 :selected selected :color (if confirm-failure? colors/red colors/blue)}]]
     [numpad/number-pad {:on-press #(re-frame/dispatch [:intro-wizard/code-digit-pressed %])}]
     [react/text {:style styles/wizard-bottom-note} (i18n/label :t/you-will-need-this-code)]]))

(defn enable-fingerprint [])

(defn enable-notifications [])

(defn bottom-bar [{:keys [step generating-keys?] :as wizard-state}]
  [react/view {:style {:margin-bottom 32}}
   (cond generating-keys?
         [react/activity-indicator {:animating true
                                    :size      :large}]
         (= step 1)
         [components.common/button {:button-style styles/intro-button
                                    ;:disabled?    disable-button?
                                    :on-press     #(re-frame/dispatch
                                                    [:intro-wizard/step-forward-pressed])
                                    :label        (i18n/label :generate-a-key)}]
         (#{4 5} step)
         [react/view (assoc styles/bottom-button-container :align-items :center)
          [components.common/button {:button-style styles/bottom-button
                                     :label (i18n/label :t/encrypt-with-password)
                                     :on-press #(re-frame/dispatch [:intro-wizard/on-encrypt-with-password-pressed])
                                     :background? false}]]

         :else
         [react/view {:style {:flex-direction :row
                              :justify-content :flex-end
                              :padding-top 16
                              :border-top-width 1
                              :border-top-color colors/gray-lighter
                              :margin-right 20}}
          [components.common/bottom-button {:on-press     #(re-frame/dispatch
                                                            [:intro-wizard/step-forward-pressed])
                                            :forward? true}]])
   (when (= 1 step)
     [react/text {:style styles/wizard-bottom-note}
      (i18n/label (if generating-keys? :t/generating-keys :t/this-will-take-few-seconds))])])

(defn top-bar [step]
  [react/view {:style {:margin-top   16
                       :margin-horizontal 32}}

   [react/text {:style styles/wizard-title} (i18n/label (keyword (str "intro-wizard-title" step)))]
   (cond (#{2 3} step)
         [react/nested-text {:style styles/wizard-text}
          (str (i18n/label (keyword (str "intro-wizard-text" step))) " ")
          [{:on-press #(re-frame/dispatch [:bottom-sheet/show-sheet :learn-more
                                           {:title (i18n/label (if (= step 2) :t/about-names-title :t/about-key-storage-title))
                                            :content  (i18n/label (if (= step 2) :t/about-names-content :t/about-key-storage-content))}])
            :style {:color colors/blue}}
           (i18n/label :learn-more)]]
         (not= step 5)
         [react/text {:style styles/wizard-text} (i18n/label (keyword (str "intro-wizard-text" step)))]
         :else nil)])

(defview wizard []
  (letsubs [{:keys [step generating-keys?] :as wizard-state} [:intro-wizard]]
    [react/view {:style {:flex 1}}
     [toolbar/toolbar
      nil
      (when-not (= :finish step)
        (toolbar/nav-button
         (actions/back #(re-frame/dispatch
                         [:intro-wizard/step-back-pressed]))))
      nil]
     [react/view {:style {:flex 1
                          :justify-content :space-between}}
      [top-bar step]
      (do
        (log/info "#wizard-state" wizard-state)
        (case step
          1 [generate-key]
          2 [choose-key wizard-state]
          3 [select-key-storage wizard-state]
          4 [create-code wizard-state]
          5 [confirm-code wizard-state]
          6 [enable-fingerprint]
          7 [enable-notifications]))
      [bottom-bar wizard-state]]]))
