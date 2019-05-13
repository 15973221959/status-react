(ns status-im.ui.components.numpad.views
  (:require [status-im.ui.components.numpad.styles :as styles]
            [status-im.ui.components.icons.vector-icons :as icons]
            [status-im.ui.components.colors :as colors]
            [status-im.ui.components.react :as react]))

(defn number-view
  [numpad-symbol {:keys [on-press]}]
  [react/touchable-opacity
   {:on-press #(on-press numpad-symbol)}
   [react/view {:style styles/number-container}
    (if (= numpad-symbol :remove)
      [icons/icon :main-icons/backspace {:color colors/blue}]
      [react/text {:style styles/number} numpad-symbol])]])

(defn number-row
  [[left middle right] opts]
  [react/view {:style styles/number-row}
   [number-view left opts]
   [react/view {:style styles/vertical-number-separator}]
   [number-view middle opts]
   [react/view {:style styles/vertical-number-separator}]
   [number-view right opts]])

(defn number-pad
  [opts]
  [react/view {:style styles/number-pad}
   [number-row [1 2 3] opts]
   [react/view {:style (styles/horizontal-separator 12 24)}]
   [number-row [4 5 6] opts]
   [react/view {:style (styles/horizontal-separator 12 24)}]
   [number-row [7 8 9] opts]
   [react/view {:style (styles/horizontal-separator 12 24)}]
   [number-row ["." 0 :remove] opts]])
