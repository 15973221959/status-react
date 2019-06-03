(ns status-im.events
  (:require [clojure.string :as string]
            [re-frame.core :as re-frame]
            [status-im.accounts.core :as accounts]
            [status-im.accounts.create.core :as accounts.create]
            [status-im.accounts.update.core :as update]
            [status-im.accounts.login.core :as accounts.login]
            [status-im.accounts.logout.core :as accounts.logout]
            [status-im.accounts.recover.core :as accounts.recover]
            [status-im.accounts.update.core :as accounts.update]
            [status-im.biometric-auth.core :as biomentric-auth]
            [status-im.bootnodes.core :as bootnodes]
            [status-im.browser.core :as browser]
            [status-im.browser.permissions :as browser.permissions]
            [status-im.chat.commands.core :as commands]
            [status-im.chat.commands.input :as commands.input]
            [status-im.chat.db :as chat.db]
            [status-im.chat.models :as chat]
            [status-im.chat.models.input :as chat.input]
            [status-im.chat.models.loading :as chat.loading]
            [status-im.chat.models.message :as chat.message]
            [status-im.contact-code.core :as contact-code]
            [status-im.contact-recovery.core :as contact-recovery]
            [status-im.contact.block :as contact.block]
            [status-im.contact.core :as contact]
            [status-im.ethereum.abi-spec :as abi-spec]
            [status-im.ethereum.core :as ethereum]
            [status-im.ethereum.resolver :as resolver]
            [status-im.ethereum.subscriptions :as ethereum.subscriptions]
            [status-im.ethereum.stateofus :as stateofus]
            [status-im.ethereum.transactions.core :as ethereum.transactions]
            [status-im.extensions.core :as extensions]
            [status-im.extensions.registry :as extensions.registry]
            [status-im.ethereum.ens :as ens]
            [status-im.fleet.core :as fleet]
            [status-im.group-chats.core :as group-chats]
            [status-im.hardwallet.core :as hardwallet]
            [status-im.i18n :as i18n]
            [status-im.init.core :as init]
            [status-im.log-level.core :as log-level]
            [status-im.mailserver.core :as mailserver]
            [status-im.network.core :as network]
            [status-im.node.core :as node]
            [status-im.notifications.core :as notifications]
            [status-im.pairing.core :as pairing]
            [status-im.privacy-policy.core :as privacy-policy]
            [status-im.protocol.core :as protocol]
            [status-im.qr-scanner.core :as qr-scanner]
            [status-im.search.core :as search]
            [status-im.signals.core :as signals]
            [status-im.stickers.core :as stickers]
            [status-im.transport.core :as transport]
            [status-im.transport.message.core :as transport.message]
            [status-im.ui.components.bottom-sheet.core :as bottom-sheet]
            [status-im.ui.components.react :as react]
            [status-im.ui.screens.add-new.new-chat.db :as new-chat.db]
            [status-im.ui.screens.currency-settings.models
             :as
             currency-settings.models]
            [status-im.ui.screens.navigation :as navigation]
            [status-im.utils.build :as build]
            [status-im.utils.config :as config]
            [status-im.utils.fx :as fx]
            [status-im.utils.handlers :as handlers]
            [status-im.utils.logging.core :as logging]
            [status-im.utils.money :as money]
            [status-im.utils.utils :as utils]
            [status-im.wallet.core :as wallet]
            [status-im.wallet.custom-tokens.core :as custom-tokens]
            [status-im.wallet.db :as wallet.db]
            [status-im.web3.core :as web3]
            [taoensso.timbre :as log]))

;; init module

(handlers/register-handler-fx
 :init.ui/data-reset-accepted
 (fn [cofx _]
   {:init/reset-data nil}))

(handlers/register-handler-fx
 :init.ui/account-data-reset-accepted
 (fn [_ [_ address]]
   {:init/reset-account-data address}))

(handlers/register-handler-fx
 :init.ui/data-reset-cancelled
 (fn [cofx [_ encryption-key]]
   (init/initialize-app cofx encryption-key)))

(handlers/register-handler-fx
 :init/app-started
 (fn [cofx _]
   (init/start-app cofx)))

(handlers/register-handler-fx
 :init.callback/get-encryption-key-success
 (fn [cofx [_ encryption-key]]
   (init/initialize-app cofx encryption-key)))

(handlers/register-handler-fx
 :init.callback/get-device-UUID-success
 (fn [cofx [_ device-uuid]]
   (init/set-device-uuid cofx device-uuid)))

(handlers/register-handler-fx
 :init.callback/init-store-success
 [(re-frame/inject-cofx :data-store/get-all-accounts)]
 (fn [cofx _]
   (init/load-accounts-and-initialize-views cofx)))

(handlers/register-handler-fx
 :init.callback/init-store-error
 (fn [cofx [_ encryption-key error]]
   (init/handle-init-store-error cofx encryption-key)))

(handlers/register-handler-fx
 :init-rest-of-chats
 [(re-frame/inject-cofx :web3/get-web3)
  (re-frame/inject-cofx :data-store/all-chats)]
 (fn [{:keys [db] :as cofx} [_]]
   (log/debug "PERF" :init-rest-of-chats (.now js/Date))
   (fx/merge cofx
             {:db (assoc db :chats/loading? false)}
             (chat.loading/initialize-chats {:from 10}))))

(defn account-change-success
  [{:keys [db] :as cofx} [_ address nodes]]
  (let [{:node/keys [status]} db]
    (fx/merge
     cofx
     (when nodes
       (fleet/set-nodes :eth.contract nodes))
     (if (= status :started)
       (accounts.login/login)
       (node/initialize (get-in db [:accounts/login :address])))
     (init/initialize-account address)
     (mailserver/initialize-ranges)
     (chat.loading/initialize-chats {:to 10}))))

(handlers/register-handler-fx
 :init.callback/account-change-success
 [(re-frame/inject-cofx :web3/get-web3)
  (re-frame/inject-cofx :data-store/get-all-contacts)
  (re-frame/inject-cofx :data-store/get-all-installations)
  (re-frame/inject-cofx :data-store/all-browsers)
  (re-frame/inject-cofx :data-store/all-dapp-permissions)
  (re-frame/inject-cofx :data-store/all-chats)
  (re-frame/inject-cofx :data-store/all-chat-requests-ranges)]
 account-change-success)

(handlers/register-handler-fx
 :init.callback/keychain-reset
 (fn [cofx _]
   (init/initialize-keychain cofx)))

(handlers/register-handler-fx
 :init.callback/account-db-removed
 (fn [{:keys [db]} _]
   {:db (assoc-in db [:accounts/login :processing] false)}))

;; home screen

(handlers/register-handler-fx
 :home.ui/sync-info-pressed
 (fn [cofx _]
   (node/display-les-debug-info cofx)))

;; accounts module

(handlers/register-handler-fx
 :accounts.ui/mainnet-warning-shown
 (fn [cofx _]
   (accounts.update/account-update cofx {:mainnet-warning-shown-version build/version} {})))

(handlers/register-handler-fx
 :accounts.update.callback/published
 (fn [{:keys [now] :as cofx} _]
   (accounts.update/account-update cofx {:last-updated now} {})))

(handlers/register-handler-fx
 :accounts.update.callback/failed-to-publish
 (fn [{:keys [now] :as cofx} [_ message]]
   (log/warn "failed to publish account update" message)
   (accounts.update/account-update cofx {:last-updated now} {})))

(handlers/register-handler-fx
 :accounts.ui/dev-mode-switched
 (fn [cofx [_ dev-mode?]]
   (accounts/switch-dev-mode cofx dev-mode?)))

(def CUD-url "https://chaos-unicorn-day.org")

(defn open-chaos-unicorn-day-link []
  (.openURL (react/linking) CUD-url))

(handlers/register-handler-fx
 :accounts.ui/chaos-mode-switched
 (fn [{:keys [db] :as cofx} [_ chaos-mode?]]
   (let [old-chaos-mode? (get-in db [:account/account :settings :chaos-mode?])]
     (fx/merge
      cofx
      (when (and chaos-mode?
                 (not= old-chaos-mode? chaos-mode?))
        {:ui/show-confirmation
         {:title               (i18n/label :t/chaos-unicorn-day)
          :content             (i18n/label :t/chaos-unicorn-day-details)
          :confirm-button-text (i18n/label :t/see-details)
          :cancel-button-text  (i18n/label :t/cancel)
          :on-accept           open-chaos-unicorn-day-link}})
      (accounts/switch-chaos-mode chaos-mode?)))))

(handlers/register-handler-fx
 :accounts.ui/biometric-auth-switched
 (fn [cofx [_ biometric-auth?]]
   (if biometric-auth?
     (biomentric-auth/authenticate-fx
      cofx
      (fn [{:keys [bioauth-success bioauth-message]}]
        (when bioauth-success
          (re-frame/dispatch [:accounts.ui/switch-biometric-auth true]))
        (when bioauth-message
          (utils/show-popup (i18n/label :t/biometric-auth-reason-verify) bioauth-message)))
      {:reason (i18n/label :t/biometric-auth-reason-verify)})
     (accounts/switch-biometric-auth cofx false))))

(handlers/register-handler-fx
 :accounts.ui/notifications-enabled
 (fn [cofx [_ desktop-notifications?]]
   (accounts/enable-notifications cofx desktop-notifications?)))

(handlers/register-handler-fx
 :accounts.ui/toggle-pfs
 (fn [cofx [_ enabled?]]
   (accounts/toggle-pfs cofx enabled?)))

(handlers/register-handler-fx
 :accounts.ui/web3-opt-in-mode-switched
 (fn [cofx [_ opt-in]]
   (accounts/switch-web3-opt-in-mode cofx opt-in)))

(handlers/register-handler-fx
 :accounts.ui/preview-privacy-mode-switched
 (fn [cofx [_ private?]]
   (accounts/switch-preview-privacy-mode cofx private?)))

(handlers/register-handler-fx
 :accounts.ui/wallet-set-up-confirmed
 (fn [cofx [_ modal?]]
   (accounts/confirm-wallet-set-up cofx modal?)))

;; accounts create module

(handlers/register-handler-fx
 :accounts.create.ui/next-step-pressed
 [(re-frame/inject-cofx :random-guid-generator)]
 (fn [cofx [_ step password password-confirm]]
   (accounts.create/next-step cofx step password password-confirm)))

(handlers/register-handler-fx
 :accounts.create.ui/step-back-pressed
 (fn [cofx [_ step password password-confirm]]
   (accounts.create/step-back cofx step)))

(handlers/register-handler-fx
 :accounts.create.ui/input-text-changed
 (fn [cofx [_ input-key text]]
   (accounts.create/account-set-input-text cofx input-key text)))

(handlers/register-handler-fx
 :accounts.create.callback/create-account-success
 [(re-frame/inject-cofx :random-guid-generator)
  (re-frame/inject-cofx :accounts.create/get-signing-phrase)
  (re-frame/inject-cofx :accounts.create/get-status)]
 (fn [cofx [_ result password]]
   (accounts.create/on-account-created cofx result password {:seed-backed-up? false
                                                             :new-account?    true})))

(handlers/register-handler-fx
 :accounts.create.ui/create-new-account-button-pressed
 (fn [cofx _]
   (hardwallet/navigate-to-authentication-method cofx)))

;; accounts recover module

(handlers/register-handler-fx
 :accounts.recover.ui/recover-account-button-pressed
 (fn [cofx _]
   (hardwallet/navigate-to-recover-method cofx)))

(handlers/register-handler-fx
 :accounts.recover.ui/passphrase-input-changed
 (fn [cofx [_ recovery-phrase]]
   (accounts.recover/set-phrase cofx recovery-phrase)))

(handlers/register-handler-fx
 :accounts.recover.ui/passphrase-input-blured
 (fn [cofx _]
   (accounts.recover/validate-phrase cofx)))

(handlers/register-handler-fx
 :accounts.recover.ui/password-input-changed
 (fn [cofx [_ masked-password]]
   (accounts.recover/set-password cofx masked-password)))

(handlers/register-handler-fx
 :accounts.recover.ui/password-input-blured
 (fn [cofx _]
   (accounts.recover/validate-password cofx)))

(handlers/register-handler-fx
 :accounts.recover.ui/sign-in-button-pressed
 [(re-frame/inject-cofx :random-guid-generator)]
 (fn [cofx _]
   (accounts.recover/recover-account-with-checks cofx)))

(handlers/register-handler-fx
 :accounts.recover.ui/recover-account-confirmed
 [(re-frame/inject-cofx :random-guid-generator)]
 (fn [cofx _]
   (accounts.recover/recover-account cofx)))

(handlers/register-handler-fx
 :accounts.recover.callback/recover-account-success
 [(re-frame/inject-cofx :random-guid-generator)
  (re-frame/inject-cofx :accounts.create/get-signing-phrase)
  (re-frame/inject-cofx :accounts.create/get-status)]
 (fn [cofx [_ result password]]
   (accounts.recover/on-account-recovered cofx result password)))

;; accounts login module

(handlers/register-handler-fx
 :accounts.login.callback/verify-success
 (fn [cofx [_ verify-result realm-error]]
   (accounts.login/verify-callback cofx verify-result realm-error)))

(handlers/register-handler-fx
 :init.callback/account-change-error
 (fn [cofx [_ error]]
   (accounts.login/handle-change-account-error cofx error)))

(handlers/register-handler-fx
 :accounts.login.ui/account-selected
 (fn [cofx [_ address photo-path name]]
   (accounts.login/open-login cofx address photo-path name)))

(handlers/register-handler-fx
 :accounts.login.callback/get-user-password-success
 (fn [{:keys [db] :as cofx} [_ password address]]
   (let [biometric-auth? (get-in db [:accounts/accounts address :settings :biometric-auth?])]
     (if (and password biometric-auth?)
       (accounts.login/do-biometric-auth cofx password)
       (accounts.login/open-login-callback cofx password {:bioauth-notrequired true})))))

;; accounts logout module

(handlers/register-handler-fx
 :accounts.logout.ui/logout-pressed
 (fn [cofx _]
   (accounts.logout/show-logout-confirmation cofx)))

(handlers/register-handler-fx
 :accounts.logout.ui/logout-confirmed
 (fn [cofx _]
   (accounts.logout/logout cofx)))

(handlers/register-handler-fx
 :accounts.logout/filters-removed
 [(re-frame/inject-cofx :data-store/get-all-accounts)]
 (fn [cofx]
   (accounts.logout/leave-account cofx)))

;; accounts update module

(handlers/register-handler-fx
 :accounts.update.callback/save-settings-success
 (fn [cofx _]
   (accounts.logout/logout cofx)))

;; mailserver module

(handlers/register-handler-fx
 :mailserver.ui/user-defined-mailserver-selected
 (fn [cofx [_ mailserver-id]]
   (mailserver/edit cofx mailserver-id)))

(handlers/register-handler-fx
 :mailserver.ui/default-mailserver-selected
 (fn [cofx [_ mailserver-id]]
   (mailserver/show-connection-confirmation cofx mailserver-id)))

(handlers/register-handler-fx
 :mailserver.ui/add-pressed
 (fn [cofx _]
   (navigation/navigate-to-cofx cofx :edit-mailserver nil)))

(handlers/register-handler-fx
 :mailserver.ui/save-pressed
 [(re-frame/inject-cofx :random-id-generator)]
 (fn [cofx _]
   (mailserver/upsert cofx)))

(handlers/register-handler-fx
 :mailserver.ui/input-changed
 (fn [cofx [_ input-key value]]
   (mailserver/set-input cofx input-key value)))

(handlers/register-handler-fx
 :mailserver.ui/delete-confirmed
 (fn [cofx [_ mailserver-id]]
   (mailserver/delete cofx mailserver-id)))

(handlers/register-handler-fx
 :mailserver.ui/delete-pressed
 (fn [cofx [_ mailserver-id]]
   (mailserver/show-delete-confirmation cofx mailserver-id)))

(handlers/register-handler-fx
 :mailserver.callback/qr-code-scanned
 (fn [cofx [_ _ url]]
   (mailserver/set-url-from-qr cofx url)))

(handlers/register-handler-fx
 :mailserver.callback/resend-request
 (fn [cofx [_ request]]
   (mailserver/resend-request cofx request)))

(handlers/register-handler-fx
 :mailserver.ui/connect-pressed
 (fn [cofx [_ mailserver-id]]
   (mailserver/show-connection-confirmation cofx mailserver-id)))

(handlers/register-handler-fx
 :mailserver.ui/connect-confirmed
 (fn [cofx [_ current-fleet mailserver-id]]
   (mailserver/save-settings cofx current-fleet mailserver-id)))

(handlers/register-handler-fx
 :mailserver.ui/reconnect-mailserver-pressed
 (fn [cofx _]
   (mailserver/connect-to-mailserver cofx)))

(handlers/register-handler-fx
 :mailserver.ui/unpin-pressed
 (fn [cofx _]
   (mailserver/unpin cofx)))

(handlers/register-handler-fx
 :mailserver.ui/pin-pressed
 (fn [cofx _]
   (mailserver/pin cofx)))

(handlers/register-handler-fx
 :mailserver.ui/request-error-pressed
 (fn [cofx _]
   (mailserver/show-request-error-popup cofx)))

(handlers/register-handler-fx
 :mailserver.ui/retry-request-pressed
 (fn [cofx [_ args]]
   (mailserver/retry-next-messages-request cofx)))

(handlers/register-handler-fx
 :mailserver/check-connection-timeout
 (fn [cofx _]
   (mailserver/check-connection cofx)))

(handlers/register-handler-fx
 :mailserver/fetch-history
 (fn [cofx [_ chat-id from-timestamp]]
   (mailserver/fetch-history cofx chat-id {:from from-timestamp})))

(handlers/register-handler-fx
 :mailserver.callback/generate-mailserver-symkey-success
 (fn [cofx [_ mailserver sym-key-id]]
   (mailserver/add-mailserver-sym-key cofx mailserver sym-key-id)))

(handlers/register-handler-fx
 :mailserver.callback/mark-trusted-peer-success
 (fn [cofx _]
   (mailserver/add-mailserver-trusted cofx)))

(handlers/register-handler-fx
 :mailserver.callback/mark-trusted-peer-error
 (fn [cofx [_ error]]
   (log/error "Error on mark-trusted-peer: " error)
   (mailserver/check-connection cofx)))

(handlers/register-handler-fx
 :mailserver.callback/request-error
 (fn [cofx [_ error]]
   (mailserver/handle-request-error cofx error)))

(handlers/register-handler-fx
 :mailserver.callback/request-success
 (fn [cofx [_ request-id]]
   (mailserver/handle-request-success cofx request-id)))

;; network module

(handlers/register-handler-fx
 :network.ui/save-network-pressed
 [(re-frame/inject-cofx :random-id-generator)]
 (fn [cofx]
   (network/save-network cofx)))

(handlers/register-handler-fx
 :network.ui/input-changed
 (fn [cofx [_ input-key value]]
   (network/set-input cofx input-key value)))

(handlers/register-handler-fx
 :network.ui/add-network-pressed
 (fn [cofx]
   (network/edit cofx)))

(handlers/register-handler-fx
 :network.callback/non-rpc-network-saved
 (fn [_ _]
   {:ui/close-application nil}))

(handlers/register-handler-fx
 :network.ui/save-non-rpc-network-pressed
 (fn [cofx [_ network]]
   (network/save-non-rpc-network cofx network)))

(handlers/register-handler-fx
 :network.ui/save-rpc-network-pressed
 (fn [cofx [_ network]]
   (network/save-rpc-network cofx network)))

(handlers/register-handler-fx
 :network.ui/remove-network-confirmed
 (fn [cofx [_ network]]
   (network/remove-network cofx network [:navigate-back])))

(handlers/register-handler-fx
 :network.ui/connect-network-pressed
 (fn [cofx [_ network-id]]
   (network/connect cofx {:network-id network-id
                          :on-failure (fn [{:keys [reason]} _]
                                        (utils/show-popup "Error" (str reason)))})))

(handlers/register-handler-fx
 :network.ui/delete-network-pressed
 (fn [cofx [_ network]]
   (network/delete cofx {:network network})))

(handlers/register-handler-fx
 :network.ui/network-entry-pressed
 (fn [cofx [_ network]]
   (network/open-network-details cofx network)))

(handlers/register-handler-fx
 :network/connection-status-changed
 (fn [{db :db :as cofx} [_ is-connected?]]
   (network/handle-connection-status-change cofx is-connected?)))

(handlers/register-handler-fx
 :network/network-status-changed
 (fn [cofx [_ data]]
   (network/handle-network-status-change cofx data)))

;; fleet module

(handlers/register-handler-fx
 :fleet.ui/save-fleet-confirmed
 (fn [cofx [_ fleet]]
   (fleet/save cofx fleet)))

(handlers/register-handler-fx
 :fleet.ui/fleet-selected
 (fn [cofx [_ fleet]]
   (fleet/show-save-confirmation cofx fleet)))

;; bootnodes module

(handlers/register-handler-fx
 :bootnodes.ui/custom-bootnodes-switch-toggled
 (fn [cofx [_ value]]
   (bootnodes/toggle-custom-bootnodes cofx value)))

(handlers/register-handler-fx
 :bootnodes.ui/add-bootnode-pressed
 (fn [cofx [_ bootnode-id]]
   (bootnodes/edit cofx bootnode-id)))

(handlers/register-handler-fx
 :bootnodes.callback/qr-code-scanned
 (fn [cofx [_ _ url]]
   (bootnodes/set-bootnodes-from-qr cofx url)))

(handlers/register-handler-fx
 :bootnodes.ui/input-changed
 (fn [cofx [_ input-key value]]
   (bootnodes/set-input cofx input-key value)))

(handlers/register-handler-fx
 :bootnodes.ui/save-pressed
 [(re-frame/inject-cofx :random-id-generator)]
 (fn [cofx _]
   (bootnodes/upsert cofx)))

(handlers/register-handler-fx
 :bootnodes.ui/delete-pressed
 (fn [cofx [_ id]]
   (bootnodes/show-delete-bootnode-confirmation cofx id)))

(handlers/register-handler-fx
 :bootnodes.ui/delete-confirmed
 (fn [cofx [_ bootnode-id]]
   (bootnodes/delete-bootnode cofx bootnode-id)))

;; extensions module

(handlers/register-handler-fx
 :extensions.callback/qr-code-scanned
 (fn [cofx [_ _ url]]
   (extensions/set-extension-url-from-qr cofx url)))

(handlers/register-handler-fx
 :extensions.ui/add-extension-pressed
 (fn [cofx [_ extension-key]]
   (extensions/edit cofx extension-key)))

(handlers/register-handler-fx
 :extensions.ui/uninstall-extension-pressed
 (fn [cofx [_ extension-key]]
   (extensions.registry/uninstall cofx extension-key)))

(handlers/register-handler-fx
 :extensions.ui/input-changed
 (fn [cofx [_ input-key value]]
   (extensions/set-input cofx input-key value)))

(handlers/register-handler-fx
 :extensions.ui/activation-checkbox-pressed
 (fn [cofx [_ extension-key active?]]
   (extensions.registry/change-state cofx extension-key active?)))

(handlers/register-handler-fx
 :extensions.ui/find-button-pressed
 (fn [cofx [_ url]]
   (extensions.registry/load cofx url false)))

(handlers/register-handler-fx
 :extensions.ui/install-extension-button-pressed
 (fn [cofx [_ url]]
   (extensions.registry/install-from-message cofx url true)))

(handlers/register-handler-fx
 :extensions.ui/install-button-pressed
 (fn [cofx [_ url data modal?]]
   (extensions.registry/install cofx url data modal?)))

;; logging module

(handlers/register-handler-fx
 :logging.ui/send-logs-pressed
 (fn [cofx _]
   (logging/send-logs cofx)))

;; log-level module

(handlers/register-handler-fx
 :log-level.ui/change-log-level-confirmed
 (fn [cofx [_ log-level]]
   (log-level/save-log-level cofx log-level)))

(handlers/register-handler-fx
 :log-level.ui/log-level-selected
 (fn [cofx [_ log-level]]
   (log-level/show-change-log-level-confirmation cofx log-level)))

(handlers/register-handler-fx
 :log-level.ui/logging-enabled
 (fn [cofx [_ enabled]]
   (log/debug "### :log-level.ui/logging-enabled" enabled)

   (log-level/show-logging-enabled-confirmation cofx enabled)))

(handlers/register-handler-fx
 :log-level.ui/logging-enabled-confirmed
 (fn [cofx [_ enabled]]
   (log-level/save-logging-enabled cofx enabled)))

;; Browser bridge module

(handlers/register-handler-fx
 :browser.bridge.callback/qr-code-scanned
 (fn [cofx [_ _ data qr-code-data]]
   (browser/handle-scanned-qr-code cofx data (:data qr-code-data))))

(handlers/register-handler-fx
 :browser.bridge.callback/qr-code-canceled
 (fn [cofx [_ _ qr-code-data]]
   (browser/handle-canceled-qr-code cofx (:data qr-code-data))))

;; qr-scanner module

(handlers/register-handler-fx
 :qr-scanner.ui/scan-qr-code-pressed
 (fn [cofx [_ identifier handler & [opts]]]
   (qr-scanner/scan-qr-code cofx identifier (merge {:handler handler} opts))))

(handlers/register-handler-fx
 :qr-scanner.callback/scan-qr-code-success
 (fn [cofx [_ context data]]
   (qr-scanner/set-qr-code cofx context data)))

(handlers/register-handler-fx
 :qr-scanner.callback/scan-qr-code-cancel
 (fn [cofx [_ context]]
   (qr-scanner/set-qr-code-cancel cofx context)))

;; privacy-policy module

(handlers/register-handler-fx
 :privacy-policy/privacy-policy-button-pressed
 (fn [cofx _]
   (privacy-policy/open-privacy-policy-link cofx)))

;; wallet modules

(handlers/register-handler-fx
 :wallet.settings.ui/currency-selected
 (fn [cofx [_ currency]]
   (currency-settings.models/set-currency cofx currency)))

;; chat module

(handlers/register-handler-fx
 :chat.ui/clear-history-pressed
 (fn [_ _]
   {:ui/show-confirmation {:title               (i18n/label :t/clear-history-title)
                           :content             (i18n/label :t/clear-history-confirmation-content)
                           :confirm-button-text (i18n/label :t/clear-history-action)
                           :on-accept           #(re-frame/dispatch [:chat.ui/clear-history])}}))

(handlers/register-handler-fx
 :chat.ui/fetch-history-pressed
 (fn [{:keys [now] :as cofx} [_ chat-id]]
   (mailserver/fetch-history cofx chat-id
                             {:from (- (quot now 1000) mailserver/one-day)})))

(handlers/register-handler-fx
 :chat.ui/fetch-history-pressed48-60
 (fn [{:keys [now] :as cofx} [_ chat-id]]
   (let [now (quot now 1000)]
     (mailserver/fetch-history cofx chat-id
                               {:from (- now (* 2.5 mailserver/one-day))
                                :to   (- now (* 2 mailserver/one-day))}))))

(handlers/register-handler-fx
 :chat.ui/fetch-history-pressed84-96
 (fn [{:keys [now] :as cofx} [_ chat-id]]
   (let [now (quot now 1000)]
     (mailserver/fetch-history cofx chat-id
                               {:from (- now (* 4 mailserver/one-day))
                                :to   (- now (* 3.5 mailserver/one-day))}))))

(handlers/register-handler-fx
 :chat.ui/fill-gaps
 (fn [{:keys [db] :as cofx} [_ gap-ids]]
   (let [chat-id           (:current-chat-id db)
         topic             (chat.db/topic-by-current-chat db)
         gaps              (keep
                            (fn [id]
                              (get-in db [:mailserver/gaps chat-id id]))
                            gap-ids)]
     (mailserver/fill-the-gap
      cofx
      {:gaps    gaps
       :topic   topic
       :chat-id chat-id}))))

(handlers/register-handler-fx
 :chat.ui/fetch-more
 (fn [{:keys [db] :as cofx}]
   (let [chat-id           (:current-chat-id db)

         {:keys [lowest-request-from]}
         (get-in db [:mailserver/ranges chat-id])

         topic             (chat.db/topic-by-current-chat db)
         gaps              [{:id   :first-gap
                             :to   lowest-request-from
                             :from (- lowest-request-from mailserver/one-day)}]]
     (mailserver/fill-the-gap
      cofx
      {:gaps    gaps
       :topic   topic
       :chat-id chat-id}))))

(handlers/register-handler-fx
 :chat.ui/remove-chat-pressed
 (fn [_ [_ chat-id]]
   {:ui/show-confirmation {:title               (i18n/label :t/delete-confirmation)
                           :content             (i18n/label :t/delete-chat-confirmation)
                           :confirm-button-text (i18n/label :t/delete)
                           :on-accept           #(re-frame/dispatch [:chat.ui/remove-chat chat-id])}}))

(handlers/register-handler-fx
 :chat.ui/set-chat-ui-props
 (fn [{:keys [db]} [_ kvs]]
   {:db (chat/set-chat-ui-props db kvs)}))

(handlers/register-handler-fx
 :chat.ui/join-time-messages-checked
 (fn [cofx [_ chat-id]]
   (chat/join-time-messages-checked cofx chat-id)))

(handlers/register-handler-fx
 :chat.ui/show-message-options
 (fn [{:keys [db]} [_ options]]
   {:db (chat/set-chat-ui-props db {:show-message-options? true
                                    :message-options       options})}))

(handlers/register-handler-fx
 :chat.ui/navigate-to-chat
 (fn [cofx [_ chat-id opts]]
   (chat/navigate-to-chat cofx chat-id opts)))

(handlers/register-handler-fx
 :chat.ui/load-more-messages
 [(re-frame/inject-cofx :data-store/get-messages)
  (re-frame/inject-cofx :data-store/get-referenced-messages)
  (re-frame/inject-cofx :data-store/get-unviewed-message-ids)
  (re-frame/inject-cofx :data-store/all-gaps)]
 (fn [cofx _]
   (chat.loading/load-more-messages cofx)))

(handlers/register-handler-fx
 :chat.ui/start-chat
 (fn [cofx [_ contact-id opts]]
   (chat/start-chat cofx contact-id opts)))

(handlers/register-handler-fx
 :chat.ui/start-public-chat
 (fn [cofx [_ topic opts]]
   (fx/merge
    cofx
    (chat/start-public-chat topic opts)
    (pairing/sync-public-chat topic))))

(handlers/register-handler-fx
 :chat.ui/remove-chat
 (fn [cofx [_ chat-id]]
   (chat/remove-chat cofx chat-id)))

(handlers/register-handler-fx
 :chat.ui/clear-history
 (fn [{{:keys [current-chat-id]} :db :as cofx} _]
   (chat/clear-history cofx current-chat-id)))

(handlers/register-handler-fx
 :chat.ui/resend-message
 (fn [cofx [_ chat-id message-id]]
   (chat.message/resend-message cofx chat-id message-id)))

(handlers/register-handler-fx
 :chat.ui/delete-message
 (fn [cofx [_ chat-id message-id]]
   (chat.message/delete-message cofx chat-id message-id)))

(handlers/register-handler-fx
 :chat.ui/message-expand-toggled
 (fn [cofx [_ chat-id message-id]]
   (chat.message/toggle-expand-message cofx chat-id message-id)))

(handlers/register-handler-fx
 :chat.ui/set-chat-input-text
 (fn [cofx [_ text]]
   (chat.input/set-chat-input-text cofx text)))

(handlers/register-handler-fx
 :chat.ui/select-chat-input-command
 (fn [cofx [_ command params previous-command-message]]
   (chat.input/select-chat-input-command cofx command params previous-command-message)))

(handlers/register-handler-fx
 :chat.ui/set-command-prefix
 (fn [cofx _]
   (chat.input/set-command-prefix cofx)))

(handlers/register-handler-fx
 :chat.ui/cancel-message-reply
 (fn [cofx _]
   (chat.input/cancel-message-reply cofx)))

(handlers/register-handler-fx
 :chat.ui/reply-to-message
 (fn [cofx [_ message-id old-message-id]]
   (chat.input/reply-to-message cofx message-id old-message-id)))

(handlers/register-handler-fx
 :chat.ui/send-current-message
 (fn [cofx _]
   (chat.input/send-current-message cofx)))

(handlers/register-handler-fx
 :chat.ui/set-command-parameter
 (fn [{{:keys [chats current-chat-id chat-ui-props id->command access-scope->command-id]} :db :as cofx} [_ value]]
   (let [current-chat   (get chats current-chat-id)
         selection      (get-in chat-ui-props [current-chat-id :selection])
         commands       (commands/chat-commands id->command access-scope->command-id current-chat)
         {:keys [current-param-position params]} (commands.input/selected-chat-command
                                                  (:input-text current-chat) selection commands)
         last-param-idx (dec (count params))]
     (commands.input/set-command-parameter cofx
                                           (= current-param-position last-param-idx)
                                           current-param-position
                                           value))))

(handlers/register-handler-fx
 :chat/send-plain-text-message
 (fn [{{:keys [current-chat-id]} :db :as cofx} [_ message-text]]
   (chat.input/send-plain-text-message-fx cofx message-text current-chat-id)))

(handlers/register-handler-fx
 :chat/send-sticker
 (fn [{{:keys [current-chat-id] :account/keys [account]} :db :as cofx} [_ {:keys [uri] :as sticker}]]
   (fx/merge
    cofx
    (accounts/update-recent-stickers (conj (remove #(= uri %) (:recent-stickers account)) uri))
    (chat.input/send-sticker-fx sticker current-chat-id))))

(handlers/register-handler-fx
 :chat/disable-cooldown
 (fn [cofx _]
   (chat/disable-chat-cooldown cofx)))

(handlers/register-handler-fx
 :message/add
 (fn [cofx [_ messages]]
   (chat.message/receive-many cofx messages)))

(handlers/register-handler-fx
 :message/update-message-status
 (fn [cofx [_ chat-id message-id status]]
   (chat.message/update-message-status cofx chat-id message-id status)))

(handlers/register-handler-fx
 :message/messages-persisted
 (fn [cofx [_ raw-messages]]
   (apply fx/merge
          cofx
          (map
           (fn [raw-message]
             (chat.message/confirm-message-processed raw-message))
           raw-messages))))

;; signal module

(handlers/register-handler-fx
 :signals/signal-received
 (fn [cofx [_ event-str]]
   (log/debug :event-str event-str)
   (signals/process cofx event-str)))

;; protocol module

(handlers/register-handler-fx
 :protocol.ui/close-app-confirmed
 (fn [cofx _]
   (protocol/handle-close-app-confirmed cofx)))

(handlers/register-handler-fx
 :protocol/state-sync-timed-out
 (fn [cofx _]
   (protocol/check-sync-state cofx)))

;; notifications module

(handlers/register-handler-fx
 :notifications/notification-open-event-received
 (fn [cofx [_ decoded-payload ctx]]
   (notifications/handle-push-notification-open cofx decoded-payload ctx)))

(handlers/register-handler-fx
 :notifications.callback/get-fcm-token-success
 (fn [{:keys [db]} [_ fcm-token]]
   {:db (assoc-in db [:notifications :fcm-token] fcm-token)}))

(handlers/register-handler-fx
 :notifications.callback/request-notifications-permissions-granted
 (fn [cofx _]
   (accounts/show-mainnet-is-default-alert cofx)))

(handlers/register-handler-fx
 :notifications.callback/request-notifications-permissions-denied
 (fn [cofx _]
   (accounts/show-mainnet-is-default-alert cofx)))

(handlers/register-handler-fx
 :notifications.callback/on-message
 (fn [cofx [_ decoded-payload opts]]
   (notifications/handle-on-message cofx decoded-payload opts)))

;; hardwallet module

(handlers/register-handler-fx
 :hardwallet.callback/on-register-card-events
 (fn [cofx [_ listeners]]
   (hardwallet/on-register-card-events cofx listeners)))

(handlers/register-handler-fx
 :hardwallet/get-application-info
 (fn [cofx _]
   (hardwallet/get-application-info cofx nil nil)))

(handlers/register-handler-fx
 :hardwallet.callback/on-get-application-info-success
 (fn [cofx [_ info on-success]]
   (hardwallet/on-get-application-info-success cofx info on-success)))

(handlers/register-handler-fx
 :hardwallet.callback/on-get-application-info-error
 (fn [cofx [_ error]]
   (hardwallet/on-get-application-info-error cofx error)))

(handlers/register-handler-fx
 :hardwallet.callback/check-nfc-support-success
 (fn [cofx [_ supported?]]
   (hardwallet/set-nfc-support cofx supported?)))

(handlers/register-handler-fx
 :hardwallet.callback/check-nfc-enabled-success
 (fn [cofx [_ enabled?]]
   (hardwallet/set-nfc-enabled cofx enabled?)))

(handlers/register-handler-fx
 :hardwallet.callback/on-card-connected
 (fn [cofx [_ data]]
   (hardwallet/on-card-connected cofx data)))

(handlers/register-handler-fx
 :hardwallet.callback/on-card-disconnected
 (fn [cofx [_ data]]
   (hardwallet/on-card-disconnected cofx data)))

(handlers/register-handler-fx
 :hardwallet.callback/on-init-card-success
 (fn [cofx [_ secrets]]
   (hardwallet/on-init-card-success cofx secrets)))

(handlers/register-handler-fx
 :hardwallet.callback/on-init-card-error
 (fn [cofx [_ error]]
   (hardwallet/on-init-card-error cofx error)))

(handlers/register-handler-fx
 :hardwallet.callback/on-install-applet-and-init-card-success
 (fn [cofx [_ secrets]]
   (hardwallet/on-install-applet-and-init-card-success cofx secrets)))

(handlers/register-handler-fx
 :hardwallet.callback/on-install-applet-and-init-card-error
 (fn [cofx [_ error]]
   (hardwallet/on-install-applet-and-init-card-error cofx error)))

(handlers/register-handler-fx
 :hardwallet.callback/on-pairing-success
 (fn [cofx [_ pairing]]
   (hardwallet/on-pairing-success cofx pairing)))

(handlers/register-handler-fx
 :hardwallet.callback/on-pairing-error
 (fn [cofx [_ error]]
   (hardwallet/on-pairing-error cofx error)))

(handlers/register-handler-fx
 :hardwallet.callback/on-generate-mnemonic-success
 (fn [cofx [_ mnemonic]]
   (hardwallet/on-generate-mnemonic-success cofx mnemonic)))

(handlers/register-handler-fx
 :hardwallet.callback/on-generate-mnemonic-error
 (fn [cofx [_ error]]
   (hardwallet/on-generate-mnemonic-error cofx error)))

(handlers/register-handler-fx
 :hardwallet.callback/on-generate-and-load-key-success
 [(re-frame/inject-cofx :random-guid-generator)
  (re-frame/inject-cofx :accounts.create/get-signing-phrase)
  (re-frame/inject-cofx :accounts.create/get-status)]
 (fn [cofx [_ data]]
   (hardwallet/on-generate-and-load-key-success cofx data)))

(handlers/register-handler-fx
 :hardwallet.callback/on-generate-and-load-key-error
 (fn [cofx [_ error]]
   (hardwallet/on-generate-and-load-key-error cofx error)))

(handlers/register-handler-fx
 :hardwallet.callback/on-unblock-pin-success
 (fn [cofx _]
   (hardwallet/on-unblock-pin-success cofx)))

(handlers/register-handler-fx
 :hardwallet.callback/on-unblock-pin-error
 (fn [cofx [_ error]]
   (hardwallet/on-unblock-pin-error cofx error)))

(handlers/register-handler-fx
 :hardwallet.callback/on-verify-pin-success
 (fn [cofx _]
   (hardwallet/on-verify-pin-success cofx)))

(handlers/register-handler-fx
 :hardwallet.callback/on-verify-pin-error
 (fn [cofx [_ error]]
   (hardwallet/on-verify-pin-error cofx error)))

(handlers/register-handler-fx
 :hardwallet.callback/on-change-pin-success
 (fn [cofx _]
   (hardwallet/on-change-pin-success cofx)))

(handlers/register-handler-fx
 :hardwallet.callback/on-change-pin-error
 (fn [cofx [_ error]]
   (hardwallet/on-change-pin-error cofx error)))

(handlers/register-handler-fx
 :hardwallet.callback/on-unpair-success
 (fn [cofx _]
   (hardwallet/on-unpair-success cofx)))

(handlers/register-handler-fx
 :hardwallet.callback/on-unpair-error
 (fn [cofx [_ error]]
   (hardwallet/on-unpair-error cofx error)))

(handlers/register-handler-fx
 :hardwallet.callback/on-delete-success
 (fn [cofx _]
   (hardwallet/on-delete-success cofx)))

(handlers/register-handler-fx
 :hardwallet.callback/on-delete-error
 (fn [cofx [_ error]]
   (hardwallet/on-delete-error cofx error)))

(handlers/register-handler-fx
 :hardwallet.callback/on-remove-key-success
 (fn [cofx _]
   (hardwallet/on-remove-key-success cofx)))

(handlers/register-handler-fx
 :hardwallet.callback/on-remove-key-error
 (fn [cofx [_ error]]
   (hardwallet/on-remove-key-error cofx error)))

(handlers/register-handler-fx
 :hardwallet.callback/on-get-keys-success
 (fn [cofx [_ data]]
   (hardwallet/on-get-keys-success cofx data)))

(handlers/register-handler-fx
 :hardwallet.callback/on-sign-success
 (fn [cofx [_ data]]
   (hardwallet/on-sign-success cofx data)))

(handlers/register-handler-fx
 :hardwallet/auto-login
 (fn [cofx _]
   (hardwallet/login-with-keycard cofx true)))

(handlers/register-handler-fx
 :hardwallet/login-with-keycard
 (fn [cofx _]
   (hardwallet/login-with-keycard cofx false)))

(handlers/register-handler-fx
 :hardwallet/check-card-state
 (fn [cofx _]
   (hardwallet/check-card-state cofx)))

(handlers/register-handler-fx
 :hardwallet.callback/on-get-keys-error
 (fn [cofx [_ error]]
   (hardwallet/on-get-keys-error cofx error)))

(handlers/register-handler-fx
 :hardwallet.callback/on-sign-error
 (fn [cofx [_ error]]
   (hardwallet/on-sign-error cofx error)))

(handlers/register-handler-fx
 :hardwallet.ui/status-hardwallet-option-pressed
 (fn [cofx _]
   (hardwallet/status-hardwallet-option-pressed cofx)))

(handlers/register-handler-fx
 :hardwallet.ui/password-option-pressed
 (fn [cofx _]
   (hardwallet/password-option-pressed cofx)))

(handlers/register-handler-fx
 :hardwallet.ui/go-to-settings-button-pressed
 (fn [_ _]
   {:hardwallet/open-nfc-settings nil}))

(handlers/register-handler-fx
 :hardwallet.ui/begin-setup-button-pressed
 (fn [cofx _]
   (hardwallet/begin-setup-button-pressed cofx)))

(handlers/register-handler-fx
 :hardwallet/start-installation
 (fn [cofx _]
   (hardwallet/start-installation cofx)))

(handlers/register-handler-fx
 :hardwallet.ui/pair-card-button-pressed
 (fn [{:keys [db]} _]
   {:db (assoc-in db [:hardwallet :setup-step] :enter-pair-code)}))

(handlers/register-handler-fx
 :hardwallet.ui/pair-code-input-changed
 (fn [{:keys [db]} [_ pair-code]]
   {:db (assoc-in db [:hardwallet :secrets :password] pair-code)}))

(handlers/register-handler-fx
 :hardwallet.ui/pair-code-next-button-pressed
 (fn [cofx]
   (hardwallet/pair-code-next-button-pressed cofx)))

(handlers/register-handler-fx
 :hardwallet.ui/recovery-phrase-next-button-pressed
 (fn [cofx _]
   (hardwallet/recovery-phrase-next-button-pressed cofx)))

(handlers/register-handler-fx
 :hardwallet.ui/recovery-phrase-confirm-word-next-button-pressed
 (fn [cofx _]
   (hardwallet/recovery-phrase-confirm-word cofx)))

(handlers/register-handler-fx
 :hardwallet.ui/recovery-phrase-confirm-word-back-button-pressed
 (fn [{:keys [db]} _]
   {:db (assoc-in db [:hardwallet :setup-step] :recovery-phrase)}))

(handlers/register-handler-fx
 :hardwallet.ui/recovery-phrase-confirm-word-input-changed
 (fn [{:keys [db]} [_ input]]
   {:db (assoc-in db [:hardwallet :recovery-phrase :input-word] input)}))

(handlers/register-handler-fx
 :hardwallet.ui/recovery-phrase-confirm-pressed
 (fn [cofx _]
   (hardwallet/load-loading-keys-screen cofx)))

(handlers/register-handler-fx
 :hardwallet/load-loading-keys-screen
 (fn [cofx _]
   (hardwallet/load-loading-keys-screen cofx)))

(handlers/register-handler-fx
 :hardwallet.ui/recovery-phrase-cancel-pressed
 (fn [{:keys [db]} _]
   {:db (assoc-in db [:hardwallet :setup-step] :recovery-phrase)}))

(handlers/register-handler-fx
 :hardwallet/load-preparing-screen
 (fn [cofx _]
   (hardwallet/load-preparing-screen cofx)))

(handlers/register-handler-fx
 :hardwallet/connection-error
 (fn [_ _]
   {:utils/show-popup {:title      (i18n/label :t/cant-read-card)
                       :content    (i18n/label :t/cant-read-card-error-explanation)
                       :on-dismiss #(re-frame/dispatch [:hardwallet.ui/connection-error-confirm-button-pressed])}}))

(handlers/register-handler-fx
 :hardwallet.ui/connection-error-confirm-button-pressed
 (fn [{:keys [db] :as cofx} _]
   (fx/merge cofx
             {:db (assoc-in db [:hardwallet :setup-step] :begin)}
             (navigation/navigate-to-cofx :hardwallet-setup nil))))

(handlers/register-handler-fx
 :hardwallet.ui/secret-keys-next-button-pressed
 (fn [_ _]
   {:ui/show-confirmation {:title               (i18n/label :t/secret-keys-confirmation-title)
                           :content             (i18n/label :t/secret-keys-confirmation-text)
                           :confirm-button-text (i18n/label :t/secret-keys-confirmation-confirm)
                           :cancel-button-text  (i18n/label :t/secret-keys-confirmation-cancel)
                           :on-accept           #(re-frame/dispatch [:hardwallet.ui/secret-keys-dialog-confirm-pressed])
                           :on-cancel           #()}}))

(handlers/register-handler-fx
 :hardwallet.ui/secret-keys-dialog-confirm-pressed
 (fn [cofx _]
   (hardwallet/load-pairing-screen cofx)))

(handlers/register-handler-fx
 :hardwallet/load-pairing-screen
 (fn [cofx _]
   (hardwallet/load-pairing-screen cofx)))

(handlers/register-handler-fx
 :hardwallet/load-generating-mnemonic-screen
 (fn [cofx _]
   (hardwallet/load-generating-mnemonic-screen cofx)))

(handlers/register-handler-fx
 :hardwallet/pair
 (fn [cofx _]
   (hardwallet/pair cofx)))

(handlers/register-handler-fx
 :hardwallet/verify-pin
 (fn [cofx _]
   (hardwallet/verify-pin cofx)))

(handlers/register-handler-fx
 :hardwallet/change-pin
 (fn [cofx _]
   (hardwallet/change-pin cofx)))

(handlers/register-handler-fx
 :hardwallet.ui/success-button-pressed
 (fn [cofx _]
   (hardwallet/success-button-pressed cofx)))

(handlers/register-handler-fx
 :hardwallet.ui/pin-numpad-button-pressed
 (fn [cofx [_ number step]]
   (hardwallet/update-pin cofx number step)))

(handlers/register-handler-fx
 :hardwallet.ui/enter-pin-navigate-back-button-clicked
 (fn [cofx _]
   (hardwallet/enter-pin-navigate-back-button-clicked cofx)))

(handlers/register-handler-fx
 :hardwallet.ui/hardwallet-connect-navigate-back-button-clicked
 (fn [cofx _]
   (hardwallet/hardwallet-connect-navigate-back-button-clicked cofx)))

(handlers/register-handler-fx
 :hardwallet/process-pin-input
 (fn [cofx _]
   (hardwallet/process-pin-input cofx)))

(handlers/register-handler-fx
 :hardwallet.ui/pin-numpad-delete-button-pressed
 (fn [{:keys [db]} [_ step]]
   (when-not (empty? (get-in db [:hardwallet :pin step]))
     {:db (update-in db [:hardwallet :pin step] pop)})))

(handlers/register-handler-fx
 :hardwallet.ui/card-ready-next-button-pressed
 (fn [cofx _]
   (hardwallet/card-ready-next-button-pressed cofx)))

(handlers/register-handler-fx
 :hardwallet/proceed-to-generate-mnemonic
 (fn [cofx _]
   (hardwallet/proceed-to-generate-mnemonic cofx)))

(handlers/register-handler-fx
 :hardwallet.ui/import-account-back-button-pressed
 (fn [cofx _]
   (hardwallet/import-account-back-button-pressed cofx)))

(handlers/register-handler-fx
 :hardwallet.ui/import-account-next-button-pressed
 (fn [cofx _]
   (hardwallet/import-account-next-button-pressed cofx)))

(handlers/register-handler-fx
 :hardwallet/load-importing-account-screen
 (fn [cofx _]
   (hardwallet/load-importing-account-screen cofx)))

(handlers/register-handler-fx
 :hardwallet/import-account
 (fn [cofx _]
   (hardwallet/import-account cofx)))

(handlers/register-handler-fx
 :hardwallet/generate-mnemonic
 (fn [cofx _]
   (hardwallet/generate-mnemonic cofx)))

(handlers/register-handler-fx
 :hardwallet/generate-and-load-key
 (fn [cofx _]
   (hardwallet/generate-and-load-key cofx)))

(handlers/register-handler-fx
 :hardwallet.ui/create-pin-button-pressed
 (fn [{:keys [db]} _]
   {:db (-> db
            (assoc-in [:hardwallet :setup-step] :pin)
            (assoc-in [:hardwallet :pin :enter-step] :original))}))

(handlers/register-handler-fx
 :hardwallet.ui/error-button-pressed
 (fn [cofx _]
   (hardwallet/error-button-pressed cofx)))

(handlers/register-handler-fx
 :keycard-settings.ui/change-pin-pressed
 (fn [cofx _]
   (hardwallet/change-pin-pressed cofx)))

(handlers/register-handler-fx
 :hardwallet/proceed-to-change-pin
 (fn [cofx _]
   (hardwallet/proceed-to-change-pin cofx)))

(handlers/register-handler-fx
 :keycard-settings.ui/unpair-card-pressed
 (fn [cofx _]
   (hardwallet/unpair-card-pressed cofx)))

(handlers/register-handler-fx
 :keycard-settings.ui/unpair-card-confirmed
 (fn [cofx _]
   (hardwallet/unpair-card-confirmed cofx)))

(handlers/register-handler-fx
 :hardwallet/unpair
 (fn [cofx _]
   (hardwallet/unpair cofx)))

(handlers/register-handler-fx
 :keycard-settings.ui/reset-card-pressed
 (fn [cofx _]
   (hardwallet/reset-card-pressed cofx)))

(handlers/register-handler-fx
 :keycard-settings.ui/reset-card-next-button-pressed
 (fn [cofx _]
   (hardwallet/reset-card-next-button-pressed cofx)))

(handlers/register-handler-fx
 :hardwallet/proceed-to-reset-card
 (fn [cofx _]
   (hardwallet/proceed-to-reset-card cofx)))

(handlers/register-handler-fx
 :hardwallet/unpair-and-delete
 (fn [cofx _]
   (hardwallet/unpair-and-delete cofx)))

(handlers/register-handler-fx
 :hardwallet/remove-key-with-unpair
 (fn [cofx _]
   (hardwallet/remove-key-with-unpair cofx)))

(handlers/register-handler-fx
 :hardwallet/navigate-to-enter-pin-screen
 (fn [cofx _]
   (hardwallet/navigate-to-enter-pin-screen cofx)))

(handlers/register-handler-fx
 :hardwallet/navigate-to-reset-card-screen
 (fn [cofx _]
   (hardwallet/navigate-to-reset-card-screen cofx)))

(handlers/register-handler-fx
 :hardwallet/sign
 (fn [cofx _]
   (hardwallet/sign cofx)))

(handlers/register-handler-fx
 :hardwallet/prepare-to-sign
 (fn [cofx _]
   (hardwallet/prepare-to-sign cofx)))

(handlers/register-handler-fx
 :hardwallet/unblock-pin
 (fn [cofx _]
   (hardwallet/unblock-pin cofx)))

;; browser module

(handlers/register-handler-fx
 :browser.ui/browser-item-selected
 (fn [cofx [_ browser-id]]
   (browser/open-existing-browser cofx browser-id)))

(handlers/register-handler-fx
 :browser.ui/url-input-pressed
 (fn [cofx _]
   (browser/update-browser-option cofx :url-editing? true)))

(handlers/register-handler-fx
 :browser.ui/url-input-blured
 (fn [cofx _]
   (browser/update-browser-option cofx :url-editing? false)))

(handlers/register-handler-fx
 :browser.ui/url-submitted
 (fn [cofx [_ url]]
   (browser/open-url-in-current-browser cofx url)))

(handlers/register-handler-fx
 :browser.ui/message-link-pressed
 (fn [cofx [_ link]]
   (browser/handle-message-link cofx link)))

(handlers/register-handler-fx
 :browser.ui/remove-browser-pressed
 (fn [cofx [_ browser-id]]
   (browser/remove-browser cofx browser-id)))

(handlers/register-handler-fx
 :browser.ui/lock-pressed
 (fn [cofx [_ secure?]]
   (browser/update-browser-option cofx :show-tooltip (if secure? :secure :not-secure))))

(handlers/register-handler-fx
 :browser.ui/close-tooltip-pressed
 (fn [cofx _]
   (browser/update-browser-option cofx :show-tooltip nil)))

(handlers/register-handler-fx
 :browser.ui/previous-page-button-pressed
 (fn [cofx _]
   (browser/navigate-to-previous-page cofx)))

(handlers/register-handler-fx
 :browser.ui/next-page-button-pressed
 (fn [cofx _]
   (browser/navigate-to-next-page cofx)))

(handlers/register-handler-fx
 :browser/navigation-state-changed
 (fn [cofx [_ event error?]]
   (browser/navigation-state-changed cofx event error?)))

(handlers/register-handler-fx
 :browser/bridge-message-received
 (fn [cofx [_ message]]
   (browser/process-bridge-message cofx message)))

(handlers/register-handler-fx
 :browser/error-occured
 (fn [cofx _]
   (browser/handle-browser-error cofx)))

(handlers/register-handler-fx
 :browser/loading-started
 (fn [cofx _]
   (browser/update-browser-options cofx {:error? false :loading? true})))

(handlers/register-handler-fx
 :browser.callback/resolve-ens-multihash-success
 (fn [cofx [_ m]]
   (browser/resolve-ens-multihash-success cofx m)))

(handlers/register-handler-fx
 :browser.callback/resolve-ens-multihash-error
 (fn [cofx _]
   (browser/resolve-ens-multihash-error cofx)))

(handlers/register-handler-fx
 :browser.callback/resolve-ens-contenthash
 (fn [cofx _]
   (browser/resolve-ens-contenthash cofx)))

(handlers/register-handler-fx
 :browser.callback/call-rpc
 (fn [cofx [_ message]]
   (browser/send-to-bridge cofx message)))

(handlers/register-handler-fx
 :browser.permissions.ui/dapp-permission-allowed
 (fn [cofx _]
   (browser.permissions/allow-permission cofx)))

(handlers/register-handler-fx
 :browser.permissions.ui/dapp-permission-denied
 (fn [cofx _]
   (browser.permissions/deny-permission cofx)))

(handlers/register-handler-fx
 :browser.permissions.ui/permission-animation-finished
 (fn [cofx [_ dapp-name]]
   (browser.permissions/process-next-permission cofx dapp-name)))

(handlers/register-handler-fx
 :browser.ui/open-in-status-option-selected
 (fn [cofx [_ url]]
   (browser/open-url cofx url)))

(handlers/register-handler-fx
 :browser.ui/open-dapp-button-pressed
 (fn [cofx [_ dapp-url]]
   (browser/open-url cofx dapp-url)))

(handlers/register-handler-fx
 :browser.ui/dapp-url-submitted
 (fn [cofx [_ dapp-url]]
   (browser/open-url cofx dapp-url)))

(handlers/register-handler-fx
 :browser.ui/open-modal-chat-button-pressed
 (fn [cofx [_ host]]
   (browser/open-chat-from-browser cofx host)))

(handlers/register-handler-fx
 :dapps/revoke-access
 (fn [cofx [_ dapp]]
   (browser.permissions/revoke-dapp-permissions cofx dapp)))

;; group-chats module

(handlers/register-handler-fx
 :group-chats.ui/create-pressed
 [(re-frame/inject-cofx :random-guid-generator)]
 (fn [cofx [_ chat-name]]
   (group-chats/create cofx chat-name)))

(handlers/register-handler-fx
 :group-chats.ui/name-changed
 (fn [cofx [_ chat-name]]
   (group-chats/handle-name-changed cofx chat-name)))

(handlers/register-handler-fx
 :group-chats.ui/save-pressed
 (fn [cofx _]
   (group-chats/save cofx)))

(handlers/register-handler-fx
 :group-chats.ui/add-members-pressed
 (fn [cofx _]
   (group-chats/add-members cofx)))

(handlers/register-handler-fx
 :group-chats.ui/remove-member-pressed
 (fn [cofx [_ chat-id public-key]]
   (group-chats/remove-member cofx chat-id public-key)))

(handlers/register-handler-fx
 :group-chats.ui/make-admin-pressed
 (fn [cofx [_ chat-id public-key]]
   (group-chats/make-admin cofx chat-id public-key)))

(handlers/register-handler-fx
 :group-chats.ui/remove-chat-pressed
 (fn [_ [_ chat-id group?]]
   {:ui/show-confirmation {:title               (i18n/label :t/delete-confirmation)
                           :content             (i18n/label :t/delete-chat-confirmation)
                           :confirm-button-text (i18n/label :t/delete)
                           :on-accept           #(re-frame/dispatch [:group-chats.ui/remove-chat-confirmed chat-id])}}))

(handlers/register-handler-fx
 :group-chats.ui/remove-chat-confirmed
 (fn [cofx [_ chat-id]]
   (group-chats/remove cofx chat-id)))

(handlers/register-handler-fx
 :group-chats.ui/join-pressed
 (fn [cofx [_ chat-id]]
   (group-chats/join-chat cofx chat-id)))

(handlers/register-handler-fx
 :group-chats.callback/sign-success
 [(re-frame/inject-cofx :random-guid-generator)]
 (fn [cofx [_ group-update]]
   (group-chats/handle-sign-success cofx group-update)))

(handlers/register-handler-fx
 :group-chats.callback/extract-signature-success
 (fn [cofx [_ group-update message-info sender-signature]]
   (group-chats/handle-membership-update cofx group-update message-info sender-signature)))

;; profile module

(handlers/register-handler-fx
 :profile.ui/keycard-settings-button-pressed
 (fn [cofx]
   (hardwallet/navigate-to-keycard-settings cofx)))

;; transport module

(handlers/register-handler-fx
 :transport/messages-received
 [handlers/logged-in (re-frame/inject-cofx :random-id-generator)]
 (fn [cofx [_ js-error js-messages chat-id]]
   (transport.message/receive-whisper-messages cofx js-error js-messages chat-id)))

(handlers/register-handler-fx
 :transport/send-status-message-error
 (fn [{:keys [db] :as cofx} [_ err]]
   (log/error :send-status-message-error err)))

(handlers/register-handler-fx
 :transport/message-sent
 (fn [cofx [_ chat-id message-id message-type envelope-hash-js messages-count]]
   (transport.message/set-message-envelope-hash cofx chat-id message-id message-type envelope-hash-js messages-count)))

(handlers/register-handler-fx
 :transport/contact-message-sent
 (fn [cofx [_ chat-id envelope-hash]]
   (transport.message/set-contact-message-envelope-hash cofx chat-id envelope-hash)))

(handlers/register-handler-fx
 :transport.callback/node-info-fetched
 (fn [cofx [_ node-info]]
   (transport/set-node-info cofx node-info)))

;; contact module

(handlers/register-handler-fx
 :contact.ui/add-to-contact-pressed
 [(re-frame/inject-cofx :random-id-generator)]
 (fn [cofx [_ public-key]]
   (if config/partitioned-topic-enabled?
     (contact/add-contacts-filter cofx public-key :add-contact)
     (contact/add-contact cofx public-key))))

(handlers/register-handler-fx
 :contact.ui/block-contact-pressed
 (fn [cofx [_ public-key]]
   (contact.block/block-contact-confirmation cofx public-key)))

(handlers/register-handler-fx
 :contact.ui/block-contact-confirmed
 [(re-frame/inject-cofx :data-store/get-user-messages)]
 (fn [cofx [_ public-key]]
   (contact.block/block-contact cofx public-key)))

(handlers/register-handler-fx
 :contact.ui/unblock-contact-pressed
 (fn [cofx [_ public-key]]
   (contact.block/unblock-contact cofx public-key)))

(handlers/register-handler-fx
 :contact/qr-code-scanned
 [(re-frame/inject-cofx :random-id-generator)]
 (fn [{:keys [db] :as cofx}  [_ _ contact-identity]]
   (let [current-account (:account/account db)
         fx              {:db (assoc db :contacts/new-identity contact-identity)}
         validation-result (new-chat.db/validate-pub-key db contact-identity)]
     (if (some? validation-result)
       {:utils/show-popup {:title (i18n/label :t/unable-to-read-this-code)
                           :content validation-result
                           :on-dismiss #(re-frame/dispatch [:navigate-to-clean :home])}}
       (fx/merge cofx
                 fx
                 (if config/partitioned-topic-enabled?
                   (contact/add-contacts-filter contact-identity :open-chat)
                   (chat/start-chat contact-identity {:navigation-reset? true})))))))

(handlers/register-handler-fx
 :contact/filters-added
 (fn [cofx [_ contact-identity]]
   (chat/start-chat cofx contact-identity {:navigation-reset? true})))

(handlers/register-handler-fx
 :contact.ui/start-group-chat-pressed
 (fn [{:keys [db] :as cofx} _]
   (contact/open-contact-toggle-list cofx)))

(handlers/register-handler-fx
 :contact.ui/send-message-pressed
 [(re-frame/inject-cofx :random-id-generator)]
 (fn [cofx [_ {:keys [public-key]}]]
   (chat/start-chat cofx public-key {:navigation-reset? true})))

(handlers/register-handler-fx
 :contact.ui/contact-code-submitted
 [(re-frame/inject-cofx :random-id-generator)]
 (fn [{{:contacts/keys [new-identity]} :db :as cofx} _]
   (chat/start-chat cofx new-identity {:navigation-reset? true})))

;; search module

(handlers/register-handler-fx
 :search/filter-changed
 (fn [cofx [_ search-filter]]
   (search/filter-changed cofx search-filter)))

;; pairing module

(handlers/register-handler-fx
 :pairing.ui/pair-devices-pressed
 (fn [cofx _]
   (pairing/pair-installation cofx)))

(handlers/register-handler-fx
 :pairing.ui/set-name-pressed
 (fn [cofx [_ installation-name]]
   (pairing/set-name cofx installation-name)))

(handlers/register-handler-fx
 :pairing.ui/synchronize-installation-pressed
 (fn [cofx _]
   (pairing/send-installation-messages cofx)))

(handlers/register-handler-fx
 :set-initial-props
 (fn [cofx [_ initial-props]]
   {:db (assoc (:db cofx) :initial-props initial-props)}))

(handlers/register-handler-fx
 :contact-code.callback/contact-code-published
 (fn [cofx arg]
   (contact-code/published cofx)))

(handlers/register-handler-fx
 :contact-code.callback/contact-code-publishing-failed
 (fn [cofx _]
   (contact-code/publishing-failed cofx)))

(handlers/register-handler-fx
 :pairing.ui/enable-installation-pressed
 (fn [cofx [_ installation-id]]
   (pairing/enable-fx cofx installation-id)))

(handlers/register-handler-fx
 :pairing.ui/disable-installation-pressed
 (fn [cofx [_ installation-id]]
   (pairing/disable-fx cofx installation-id)))

(handlers/register-handler-fx
 :pairing.ui/prompt-dismissed
 (fn [cofx _]
   (pairing/prompt-dismissed cofx)))

(handlers/register-handler-fx
 :pairing.ui/prompt-accepted
 (fn [cofx _]
   (pairing/prompt-accepted cofx)))

(handlers/register-handler-fx
 :pairing.callback/enable-installation-success
 (fn [cofx [_ installation-id]]
   (fx/merge cofx
             (pairing/enable installation-id)
             (accounts.update/send-account-update))))

(handlers/register-handler-fx
 :pairing.callback/disable-installation-success
 (fn [cofx [_ installation-id]]
   (fx/merge cofx
             (pairing/disable installation-id)
             (accounts.update/send-account-update))))

;; Contact recovery module

(handlers/register-handler-fx
 :contact-recovery.ui/prompt-accepted
 [(re-frame/inject-cofx :random-id-generator)]
 (fn [cofx [_ public-key]]
   (contact-recovery/prompt-accepted cofx public-key)))

(handlers/register-handler-fx
 :contact-recovery.ui/prompt-dismissed
 (fn [cofx [_ public-key]]
   (contact-recovery/prompt-dismissed cofx public-key)))

(handlers/register-handler-fx
 :contact-recovery.callback/handle-recovery
 [(re-frame/inject-cofx :random-id-generator)]
 (fn [cofx [_ public-key]]
   (contact-recovery/handle-recovery cofx public-key)))

(handlers/register-handler-fx
 :stickers/load-sticker-pack-success
 (fn [cofx [_ edn-string id price open?]]
   (stickers/load-sticker-pack-success cofx edn-string id price open?)))

(handlers/register-handler-fx
 :stickers/install-pack
 (fn [cofx [_ id]]
   (stickers/install-stickers-pack cofx id)))

(handlers/register-handler-fx
 :stickers/load-packs
 (fn [cofx _]
   (stickers/load-packs cofx)))

(handlers/register-handler-fx
 :stickers/load-pack
 (fn [cofx [_ proto-code hash id price open?]]
   (stickers/load-pack cofx proto-code hash id price open?)))

(handlers/register-handler-fx
 :stickers/select-pack
 (fn [{:keys [db]} [_ id]]
   {:db (assoc db :stickers/selected-pack id)}))

(handlers/register-handler-fx
 :stickers/open-sticker-pack
 (fn [cofx [_ id]]
   (stickers/open-sticker-pack cofx id)))

(handlers/register-handler-fx
 :stickers/buy-pack
 (fn [cofx [_ id price]]
   (stickers/approve-pack cofx id price)))

(handlers/register-handler-fx
 :stickers/get-owned-packs
 (fn [cofx _]
   (stickers/get-owned-pack cofx)))

(handlers/register-handler-fx
 :stickers/pack-owned
 (fn [cofx [_ id]]
   (stickers/pack-owned cofx id)))

(handlers/register-handler-fx
 :stickers/pending-pack
 (fn [cofx [_ id]]
   (stickers/pending-pack cofx id)))

(handlers/register-handler-fx
 :stickers/pending-timout
 (fn [cofx _]
   (stickers/pending-timeout cofx)))

;; Tribute to Talk


;; bottom-sheet events
(handlers/register-handler-fx
 :bottom-sheet/show-sheet
 (fn [cofx [_ view options]]
   (bottom-sheet/show-bottom-sheet
    cofx
    {:view view
     :options options})))

(handlers/register-handler-fx
 :bottom-sheet/hide-sheet
 (fn [cofx _]
   (bottom-sheet/hide-bottom-sheet cofx)))

;;custom tokens

(handlers/register-handler-fx
 :wallet.custom-token/decimals-result
 (fn [cofx [_ result]]
   (custom-tokens/decimals-result cofx result)))

(handlers/register-handler-fx
 :wallet.custom-token/symbol-result
 (fn [cofx [_ contract result]]
   (custom-tokens/symbol-result cofx contract result)))

(handlers/register-handler-fx
 :wallet.custom-token/name-result
 (fn [cofx [_ contract result]]
   (custom-tokens/name-result cofx contract result)))

(handlers/register-handler-fx
 :wallet.custom-token/balance-result
 (fn [cofx [_ contract result]]
   (custom-tokens/balance-result cofx contract result)))

(handlers/register-handler-fx
 :wallet.custom-token/total-supply-result
 (fn [cofx [_ contract result]]
   (custom-tokens/total-supply-result cofx contract result)))

(handlers/register-handler-fx
 :wallet.custom-token/contract-address-is-pasted
 (fn [cofx [_ contract]]
   (custom-tokens/contract-address-is-changed cofx contract)))

(handlers/register-handler-fx
 :wallet.custom-token.ui/contract-address-paste
 (fn [_ _]
   {:wallet.custom-token/contract-address-paste nil}))

(handlers/register-handler-fx
 :wallet.custom-token.ui/field-is-edited
 (fn [cofx [_ field-key value]]
   (custom-tokens/field-is-edited cofx field-key value)))

(handlers/register-handler-fx
 :wallet.custom-token.ui/add-pressed
 (fn [cofx _]
   (fx/merge cofx
             (custom-tokens/add-custom-token)
             (navigation/navigate-back))))

(handlers/register-handler-fx
 :wallet.custom-token.ui/remove-pressed
 (fn [cofx [_ token navigate-back?]]
   (fx/merge cofx
             (custom-tokens/remove-custom-token token)
             (when navigate-back?
               (navigation/navigate-back)))))

;; ethereum subscriptions events

(handlers/register-handler-fx
 :ethereum.callback/subscription-success
 (fn [cofx [_ id handler]]
   (ethereum.subscriptions/register-subscription cofx id handler)))

(handlers/register-handler-fx
 :ethereum.signal/new-block
 (fn [cofx [_ block]]
   (ethereum.subscriptions/new-block cofx block)))

;; ethereum transactions events
(handlers/register-handler-fx
 :ethereum.transactions.callback/fetch-history-success
 (fn [cofx [_ transactions]]
   (ethereum.transactions/handle-history cofx transactions)))

(handlers/register-handler-fx
 :ethereum.transactions.callback/etherscan-error
 (fn [cofx [event error]]
   (log/info event error)))

(handlers/register-handler-fx
 :ethereum.transactions.callback/fetch-token-history-success
 (fn [cofx [_ transactions]]
   (ethereum.transactions/handle-token-history cofx transactions)))

(handlers/register-handler-fx
 :ethereum.transactions/new
 (fn [cofx [_ transaction]]
   (ethereum.transactions/new cofx transaction)))

;; wallet events

(handlers/register-handler-fx
 :wallet.ui/pull-to-refresh
 (fn [cofx _]
   (wallet/update-prices cofx)))

(handlers/register-handler-fx
 :wallet.transactions/add-filter
 (fn [{:keys [db]} [_ id]]
   {:db (update-in db [:wallet :filters] conj id)}))

(handlers/register-handler-fx
 :wallet.transactions/remove-filter
 (fn [{:keys [db]} [_ id]]
   {:db (update-in db [:wallet :filters] disj id)}))

(handlers/register-handler-fx
 :wallet.transactions/add-all-filters
 (fn [{:keys [db]} _]
   {:db (assoc-in db [:wallet :filters]
                  wallet.db/default-wallet-filters)}))

(handlers/register-handler-fx
 :wallet.settings/toggle-visible-token
 (fn [cofx [_ symbol checked?]]
   (wallet/toggle-visible-token cofx symbol checked?)))

(handlers/register-handler-fx
 :wallet/token-found
 (fn [cofx [_ symbol balance]]
   (wallet/configure-token-balance-and-visibility cofx symbol balance)))

(handlers/register-handler-fx
 :wallet.settings.ui/navigate-back-pressed
 (fn [cofx [_ on-close]]
   (fx/merge cofx
             (when on-close
               {:dispatch on-close})
             (navigation/navigate-back)
             (wallet/update-balances))))

(handlers/register-handler-fx
 :wallet.callback/update-balance-success
 (fn [cofx [_ balance]]
   (wallet/update-balance cofx balance)))

(handlers/register-handler-fx
 :wallet.callback/update-balance-fail
 (fn [cofx [_ err]]
   (wallet/on-update-balance-fail cofx err)))

(handlers/register-handler-fx
 :wallet.callback/update-token-balance-success
 (fn [cofx [_ symbol balance]]
   (wallet/update-token-balance cofx symbol balance)))

(handlers/register-handler-fx
 :wallet.callback/update-token-balance-fail
 (fn [cofx [_ symbol err]]
   (wallet/on-update-token-balance-fail cofx symbol err)))

(handlers/register-handler-fx
 :wallet.callback/update-prices-success
 (fn [cofx [_ prices]]
   (wallet/on-update-prices-success cofx prices)))

(handlers/register-handler-fx
 :wallet.callback/update-prices-fail
 (fn [cofx [_ err]]
   (wallet/on-update-prices-fail cofx err)))

(handlers/register-handler-fx
 :wallet.ui/show-transaction-details
 (fn [cofx [_ hash]]
   (wallet/open-transaction-details cofx hash)))

(handlers/register-handler-fx
 :wallet/show-sign-transaction
 (fn [cofx [_ {:keys [id method]} from-chat?]]
   (wallet/open-send-transaction-modal cofx id method from-chat?)))

(handlers/register-handler-fx
 :wallet/update-gas-price-success
 (fn [cofx [_ price edit?]]
   (wallet/update-gas-price cofx price edit?)))

(handlers/register-handler-fx
 :TODO.remove/update-estimated-gas
 (fn [{:keys [db]} [_ obj]]
   {:wallet/update-estimated-gas
    {:obj           obj
     :success-event :wallet/update-estimated-gas-success}}))

(handlers/register-handler-fx
 :wallet/update-estimated-gas-success
 (fn [cofx [_ gas]]
   (wallet/update-estimated-gas-price cofx gas)))

(handlers/register-handler-fx
 :wallet.setup.ui/navigate-back-pressed
 (fn [{:keys [db] :as cofx}]
   (fx/merge cofx
             {:db (assoc-in db [:wallet :send-transaction] {})}
             (navigation/navigate-back))))

(handlers/register-handler-fx
 :shake-event
 (fn [cofx _]
   (logging/show-logs-dialog cofx)))

(re-frame/reg-fx
 ::ens/resolve
 (fn [[registry name cb]]
   (ens/get-addr registry name cb)))

(defn- on-resolve [registry custom-domain? username address public-key s]
  (cond
    (= (ethereum/normalized-address address) (ethereum/normalized-address s))
    (resolver/pubkey registry username
                     (fn [ss]
                       (if (= ss public-key)
                         (re-frame/dispatch [:ens/set-state :connected])
                         (re-frame/dispatch [:ens/set-state :owned]))))

    (and (nil? s) (not custom-domain?)) ;; No address for a stateofus subdomain: it can be registered
    (re-frame/dispatch [:ens/set-state :registrable])

    :else
    (re-frame/dispatch [:ens/set-state :unregistrable])))

(defn- assoc-state [db state]
  (assoc-in db [:ens :state] state))

(defn- assoc-username [db username]
  (assoc-in db [:ens :username] username))

(defn- valid-custom-domain? [username]
  (and (ens/is-valid-eth-name? username)
       (stateofus/lower-case? username)))

(defn- valid-username? [custom-domain? username]
  (if custom-domain?
    (valid-custom-domain? username)
    (stateofus/valid-username? username)))

(handlers/register-handler-fx
 :ens/set-state
 (fn [{:keys [db]} [_ state]]
   {:db (assoc-state db state)}))

(defn- state [valid? username]
  (cond
    (string/blank? username) :initial
    valid? :typing
    :else
    :invalid))

(handlers/register-handler-fx
 :ens/set-username
 (fn [{:keys [db]} [_ custom-domain? username]]
   (let [valid? (valid-username? custom-domain? username)]
     (merge
      {:db (-> db
               (assoc-username username)
               (assoc-state (state valid? username)))}
      (when valid?
        (let [{:keys [account/account]}        db
              {:keys [address public-key]}     account
              registry (get ens/ens-registries (ethereum/chain-keyword db))
              name     (if custom-domain? username (stateofus/subdomain username))]
          {::ens/resolve [registry name #(on-resolve registry custom-domain? name address public-key %)]}))))))

(handlers/register-handler-fx
 :ens/navigate-back
 (fn [{:keys [db] :as cofx} _]
   (fx/merge cofx
             {:db (-> db
                      (assoc-state :initial)
                      (assoc-username ""))}
             (navigation/navigate-back))))

(handlers/register-handler-fx
 :ens/switch-domain-type
 (fn [{:keys [db]} _]
   {:db (-> (update-in db [:ens :custom-domain?] not)
            (assoc-state :initial)
            (assoc-username ""))}))

(fx/defn save-username-and-navigate-back
  [{:keys [db] :as cofx} username]
  (let [db (update-in db [:account/account :usernames] #((fnil conj []) %1 %2) username)]
    (fx/merge
     cofx
     {:db       db
      :dispatch [:navigate-back]}
     (update/account-update {:usernames (get-in db [:account/account :usernames])}
                            {:success-event [:ens/set-state :saved]}))))

(handlers/register-handler-fx
 :ens/save-username
 (fn [cofx [_ username]]
   (save-username-and-navigate-back cofx username)))

(handlers/register-handler-fx
 :ens/on-registration-failure
 (fn [{:keys [db]} m]
   {:db (assoc-state db :registration-failed)}))

(handlers/register-handler-fx
 :ens/register
 (fn [cofx [_ {:keys [contract username address public-key] :as args}]]
   (let [{:keys [x y]} (ethereum/coordinates public-key)]
     (wallet/eth-transaction-call
      cofx
      {:contract   "0x744d70fdbe2ba4cf95131626614a1763df805b9e"
       :method     "approveAndCall(address,uint256,bytes)"
       :params     [contract
                    (money/unit->token 10 18)
                    (abi-spec/encode "register(bytes32,address,bytes32,bytes32)"
                                     [(ethereum/sha3 username) address x y])]
       :to-name    "Stateofus registrar"
       :amount     (money/bignumber 0)
       :gas        (money/bignumber 200000)
       :on-result  [:ens/save-username username]
       :on-error   [:ens/on-registration-failure]}))))