(ns status-im.ens.core
  (:require [clojure.string :as string]
            [re-frame.core :as re-frame]
            [status-im.accounts.update.core :as accounts.update]
            [status-im.ethereum.abi-spec :as abi-spec]
            [status-im.ethereum.core :as ethereum]
            [status-im.ethereum.ens :as ens]
            [status-im.ethereum.resolver :as resolver]
            [status-im.ethereum.stateofus :as stateofus]
            [status-im.utils.fx :as fx]
            [status-im.utils.money :as money]
            [status-im.wallet.core :as wallet])
  (:refer-clojure :exclude [name]))

(defn name [custom-domain? username]
  (if custom-domain?
    username
    (stateofus/subdomain username)))

(re-frame/reg-fx
 :ens/resolve-address
 (fn [[registry name cb]]
   (ens/get-addr registry name cb)))

(re-frame/reg-fx
 :ens/resolve-pubkey
 (fn [[registry name cb]]
   (resolver/pubkey registry name cb)))

(fx/defn save-username
  [{:keys [db] :as cofx} custom-domain? username]
  (let [name (name custom-domain? username)
        db   (update-in db [:account/account :usernames] #((fnil conj []) %1 %2) name)]
    (accounts.update/account-update {:usernames (get-in db [:account/account :usernames])}
                                    {:success-event [:ens/set-state username :saved]})))

(defn assoc-state-for [db username state]
  (assoc-in db [:ens/registration :states username] state))

(defn assoc-details-for [db username k v]
  (assoc-in db [:ens/registration :details username k] v))

(defn assoc-username-candidate [db username]
  (assoc-in db [:ens/registration :username-candidate] username))

(defn empty-username-candidate [db] (assoc-username-candidate db ""))

(fx/defn set-state
  {:events [:ens/set-state]}
  [{:keys [db]} username state]
  {:db (assoc-state-for db username state)})

(defn- on-resolve [registry custom-domain? username address public-key s]
  (cond
    (= (ethereum/normalized-address address) (ethereum/normalized-address s))
    (resolver/pubkey registry (name custom-domain? username)
                     (fn [ss]
                       (if (= ss public-key)
                         (re-frame/dispatch [:ens/set-state username :connected])
                         (re-frame/dispatch [:ens/set-state username :owned]))))

    (and (nil? s) (not custom-domain?)) ;; No address for a stateofus subdomain: it can be registered
    (re-frame/dispatch [:ens/set-state username :registrable])

    :else
    (re-frame/dispatch [:ens/set-state username :unregistrable])))

(fx/defn register-name
  [cofx contract custom-domain? username address public-key]
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
      :on-result  [:ens/save-username-and-navigate-back custom-domain? username]
      :on-error   [:ens/on-registration-failure]})))

(defn- valid-custom-domain? [username]
  (and (ens/is-valid-eth-name? username)
       (stateofus/lower-case? username)))

(defn- valid-username? [custom-domain? username]
  (if custom-domain?
    (valid-custom-domain? username)
    (stateofus/valid-username? username)))

(defn- state [custom-domain? username]
  (cond
    (string/blank? username) :initial
    (> 4 (count username)) :too-short
    (valid-username? custom-domain? username) :valid
    :else :invalid))

(fx/defn set-username-candidate
  {:events [:ens/set-username-candidate]}
  [{:keys [db]} custom-domain? username]
  (let [state  (state custom-domain? username)
        valid? (valid-username? custom-domain? username)]
    (merge
     {:db (-> db
              (assoc-username-candidate username)
              (assoc-state-for username state))}
     (when (= :valid state)
       (let [{:keys [account/account]}        db
             {:keys [address public-key]}     account
             registry (get ens/ens-registries (ethereum/chain-keyword db))]
         {:ens/resolve-address [registry (name custom-domain? username) #(on-resolve registry custom-domain? username address public-key %)]})))))