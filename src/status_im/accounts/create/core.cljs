(ns status-im.accounts.create.core
  (:require [clojure.string :as string]
            [re-frame.core :as re-frame]
            [status-im.accounts.core :as accounts.core]
            [status-im.accounts.login.core :as accounts.login]
            [status-im.accounts.statuses :as statuses]
            [status-im.accounts.update.core :as accounts.update]
            [status-im.constants :as constants]
            [status-im.data-store.accounts :as accounts-store]
            [status-im.i18n :as i18n]
            [status-im.native-module.core :as status]
            [status-im.ui.screens.navigation :as navigation]
            [status-im.utils.config :as config]
            [status-im.utils.random :as random]
            [status-im.utils.gfycat.core :as gfycat]
            [status-im.utils.hex :as utils.hex]
            [status-im.utils.identicon :as identicon]
            [status-im.utils.signing-phrase.core :as signing-phrase]
            [status-im.utils.types :as types]
            [status-im.utils.utils :as utils]
            [taoensso.timbre :as log]
            [status-im.utils.fx :as fx]
            [status-im.node.core :as node]
            [status-im.ui.screens.mobile-network-settings.events :as mobile-network]
            [status-im.utils.platform :as platform]))

(defn get-signing-phrase [cofx]
  (assoc cofx :signing-phrase (signing-phrase/generate)))

(defn get-status [cofx]
  (assoc cofx :status (rand-nth statuses/data)))

(defn create-account! [password]
  (log/info "#create-account!" password)
  (status/create-account
   password
   #(re-frame/dispatch [:accounts.create.callback/create-account-success (types/json->clj %) password])))

;;;; Handlers
(defn create-account
  [{:keys [db] :as   cofx}]
  (log/info "creating account with password:" (get-in db [:intro-wizard :key-code]))
  (fx/merge
   cofx
   {:db (-> db
            (update :accounts/create assoc
                    :password (get-in db [:intro-wizard :key-code])
                    :step :account-creating
                    :error nil)
            (assoc :node/on-ready :create-account
                   :accounts/new-installation-id (random/guid)))}
   (node/initialize nil)))

(fx/defn add-account
  "Takes db and new account, creates map of effects describing adding account to database and realm"
  [cofx {:keys [address] :as account}]
  (let [db (:db cofx)
        {:networks/keys [networks]} db
        enriched-account (assoc account
                                :network config/default-network
                                :networks networks
                                :address address)]
    {:db                 (assoc-in db [:accounts/accounts address] enriched-account)
     :data-store/base-tx [(accounts-store/save-account-tx enriched-account)]}))

(defn reset-account-creation [{db :db}]
  {:db (update db :accounts/create assoc
               :step :enter-password
               :password nil
               :password-confirm nil
               :error nil)})

(fx/defn account-set-input-text
  [{db :db} input-key text]
  {:db (update db :accounts/create merge {input-key text :error nil})})

(defn account-set-name [{{:accounts/keys [create] :as db} :db now :now :as cofx}]
  (fx/merge cofx
            {:db                                              db
             :notifications/request-notifications-permissions nil
             :dispatch-n                                      [[:navigate-to :home]
                                                               (when-not platform/desktop?
                                                                 [:navigate-to :welcome])]}
            ;; We set last updated as we are actually changing a field,
            ;; unlike on recovery where the name is not set
            (accounts.update/account-update {:last-updated now
                                             :name         (:name create)} {})
            (mobile-network/on-network-status-change)))

(fx/defn next-step
  [{:keys [db] :as cofx} step password password-confirm]
  (case step
    :enter-password {:db (assoc-in db [:accounts/create :step] :confirm-password)}
    :confirm-password (if (= password password-confirm)
                        (create-account cofx)
                        {:db (assoc-in db [:accounts/create :error] (i18n/label :t/password_error1))})
    :enter-name (account-set-name cofx)))

(fx/defn step-back
  [cofx step]
  (case step
    :enter-password (navigation/navigate-back cofx)
    :confirm-password (reset-account-creation cofx)))

(fx/defn navigate-to-create-account-screen
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            {:db (update db :accounts/create
                         #(-> %
                              (assoc :step :enter-password)
                              (dissoc :password :password-confirm :name :error)))}
            (navigation/navigate-to-cofx :create-account nil)))

(fx/defn intro-wizard [{:keys [db] :as cofx}]
  (fx/merge {:db (assoc db :intro-wizard {:step 1})}
            (navigation/navigate-to-cofx :intro-wizard nil)))

(fx/defn intro-step-back [{:keys [db] :as cofx}]
  (let  [step (get-in db [:intro-wizard :step])]
    (if (< 1 step)
      (fx/merge {:db (cond-> (assoc-in db [:intro-wizard :step] (dec step))
                       (#{4 5} step)
                       (assoc-in [:intro-wizard :key-code] nil)
                       (= step 4)
                       (assoc-in [:intro-wizard :encrypt-with-password?] false)
                       (= step 5)
                       (assoc-in [:intro-wizard :confirm-failure?] false))}
                (navigation/navigate-to-cofx :intro-wizard nil))

      (fx/merge {:db (dissoc db :intro-wizard)}
                (navigation/navigate-to-clean :intro nil)))))

(fx/defn intro-step-forward [{:keys [db] :as cofx} {:keys [skip?] :as opts}]
  (let  [step (get-in db [:intro-wizard :step])
         _ (log/info "#step-forward" step)]

    (cond (= step 7)
          (create-account cofx)
          #_(fx/merge cofx
                      (navigation/navigate-to-cofx :welcome nil)
                      create-account)
          #_(fx/merge {:db (dissoc db :intro-wizard)}
                      (navigation/navigate-to-cofx :welcome nil))
          (= step 1)
          {:db (assoc-in db [:intro-wizard :generating-keys?] true)
           :intro-wizard/new-onboarding {:n 5 :mnemonic-length 12}}

          (and (= step 5)
               (not (:accounts/login db))
               (get-in db [:intro-wizard :encrypt-with-password?])
               (not= (get-in db [:intro-wizard :stored-key-code]) (get-in db [:intro-wizard :key-code])))
          {:db (assoc-in db [:intro-wizard :confirm-failure?] true)}

          (= step 4)
          {:db (-> db
                   (assoc-in [:intro-wizard :stored-key-code] (get-in db [:intro-wizard :key-code]))
                   (assoc-in [:intro-wizard :key-code] nil)
                   (assoc-in [:intro-wizard :step] 5))}
          :else (do
                  (log/info "step-forward else")
                  (fx/merge {:db (assoc-in db [:intro-wizard :step]
                                           (inc step))}
                            (navigation/navigate-to-cofx :intro-wizard nil))))))

(fx/defn on-account-created
  [{:keys [signing-phrase
           status
           db] :as cofx}
   {:keys [pubkey address mnemonic installation-id
           keycard-instance-uid keycard-key-uid keycard-pairing keycard-paired-on] :as result}
   password
   {:keys [seed-backed-up? login? new-account?] :or {login? true}}]
  (log/info "#on-account-created" result)
  (let [normalized-address (utils.hex/normalize-hex address)
        account            {:public-key             pubkey
                            :installation-id        (or installation-id (get-in db [:accounts/new-installation-id]))
                            :address                normalized-address
                            :name                   (gfycat/generate-gfy pubkey)
                            :status                 status
                            :signed-up?             true
                            :desktop-notifications? false
                            :photo-path             (identicon/identicon pubkey)
                            :signing-phrase         signing-phrase
                            :seed-backed-up?        seed-backed-up?
                            :mnemonic               mnemonic
                            :keycard-instance-uid   keycard-instance-uid
                            :keycard-key-uid        keycard-key-uid
                            :keycard-pairing        keycard-pairing
                            :keycard-paired-on      keycard-paired-on
                            :settings               (constants/default-account-settings)
                            :syncing-on-mobile-network? false
                            :remember-syncing-choice? false
                            :new-account?           new-account?}]
    (log/info "account-created" account)
    (when-not (string/blank? pubkey)
      (fx/merge cofx
                {:db (assoc db :accounts/login {:address    normalized-address
                                                :password   password
                                                :processing true})}
                (add-account account)
                (when login?
                  (log/info "#before user-login" (:intro-wizard db))
                  (accounts.login/user-login true))))))

(re-frame/reg-fx
 :intro-wizard/new-onboarding
 (fn [{:keys [n mnemonic-length]}]
   (log/info "#:intro-wizard/new-onboarding")
   (status/new-onboarding n mnemonic-length
                          #(re-frame/dispatch [:intro-wizard/on-keys-generated (types/json->clj %)]))))

(fx/defn on-keys-generated [{:keys [db] :as cofx} result]
  (log/info "#on-keys-generated" (count (:accounts result)))
  (fx/merge
   {:db (update db :intro-wizard
                (fn [data]
                  (-> data
                      (dissoc :generating-keys?)
                      (assoc :accounts (:accounts result))
                      (assoc :selected-storage-type :default)
                      (assoc :selected-pubkey (-> result :accounts first :pubkey))
                      (assoc :step 2))))}
   (navigation/navigate-to-cofx :intro-wizard nil)))

(fx/defn on-key-selected [{:keys [db] :as cofx} pubkey]
  {:db (assoc-in db [:intro-wizard :selected-pubkey] pubkey)})

(fx/defn on-key-storage-selected [{:keys [db] :as cofx} storage-type]
  (log/info "#on-key-storage-selected" storage-type)
  {:db (assoc-in db [:intro-wizard :selected-storage-type] storage-type)})

(fx/defn on-encrypt-with-password-pressed [{:keys [db] :as cofx}]
  {:db (assoc-in db [:intro-wizard :encrypt-with-password?] true)})

(fx/defn on-learn-more-pressed [{:keys [db] :as cofx}]
  {:db (assoc-in db [:intro-wizard :show-learn-more?] true)})

(defn get-new-key-code [current-code sym encrypt-with-password?]
  (cond (= sym :remove)
        (subs current-code 0 (dec (count current-code)))
        (and (not encrypt-with-password?) (= (count current-code) 6))
        current-code
        :else (str current-code sym)))

(fx/defn code-symbol-pressed [{:keys [db] :as cofx} sym]
  (log/info "code-symbol-pressed" sym)
  (let [encrypt-with-password? (get-in db [:intro-wizard :encrypt-with-password?])
        new-key-code (get-new-key-code (get-in db [:intro-wizard :key-code]) sym encrypt-with-password?)
        stored-key-code (get-in db [:intro-wizard :stored-key-code])
        _ (log/info "new-key-code" new-key-code)
        step (get-in db [:intro-wizard :step])
        confirm-failure? (and (= step 5)
                              (not encrypt-with-password?)
                              (= (count new-key-code) 6)
                              (not= new-key-code stored-key-code))]
    (when confirm-failure?
      (utils/vibrate))
    (fx/merge {:db (-> db
                       (assoc-in [:intro-wizard :key-code] new-key-code)
                       (assoc-in [:intro-wizard :confirm-failure?] confirm-failure?))}
              (when (and (not encrypt-with-password?)
                         (= (count new-key-code) 6)
                         (not confirm-failure?))
                (intro-step-forward {})))))

;;;; COFX

(re-frame/reg-cofx
 :accounts.create/get-signing-phrase
 (fn [cofx _]
   (get-signing-phrase cofx)))

(re-frame/reg-cofx
 :accounts.create/get-status
 (fn [cofx _]
   (get-status cofx)))

;;;; FX

(re-frame/reg-fx
 :accounts.create/create-account
 create-account!)
