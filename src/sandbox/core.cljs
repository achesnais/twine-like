(ns sandbox.core
  (:require [rum.core :as rum]
            [clojure.string :as str]))

(defonce *db (atom {:selected-word nil
                    :words         {}}))

(defn- add-sentence-to-db [db sentence]
  (let [words (str/split sentence #"[^a-zA-Z-_]")]
    (update db :words #(merge-with merge % (zipmap words (repeat {}))))))

(rum/defcs text-input <
  rum/reactive
  (rum/local "" ::value)
  {:did-mount
   (fn [state]
     (.addEventListener (rum/dom-node state)
                        "keyup"
                        (fn [e]
                          (when (= (.. e -keyCode) 13)
                            (let [*value (::value state)]
                              (when-not (str/blank? @*value)
                                (let [[old _] (reset-vals! *value "")]
                                  (swap! *db #(let [selected-word (:selected-word %)]
                                                (cond-> (add-sentence-to-db % old)
                                                  (some? selected-word) (assoc-in [:words selected-word :sentence] old)))))))))
                        {:once false})
     state)}
  [state]
  (let [db     (rum/react *db)
        *value (::value state)]
    (let [selected-word (:selected-word db)]
      [:div {}
       [:h2 {}
        (if (some? selected-word)
          (str "Text for " selected-word)
          "New entry")]
       (when-some [sent (get-in db [:words selected-word :sentence])]
         [:p "current sentence: " sent])
       [:input {:type      "text"
                :name      "text-input"
                :value     @*value
                :on-change #(reset! *value (.. % -target -value))}]
       [:div {}
        [:h3 "Known"]
        (for [w     (keys (:words db))
              :when (and
                     (not (str/blank? @*value))
                     (str/starts-with? w @*value))]
          [:li {:key w} w])]])))

(def ^:private link-style
  {:color           "blue"
   :text-decoration "underline"
   :cursor          "pointer"})

(rum/defc existing-words
  < rum/reactive
  []
  (let [db    (rum/react *db)
        words (keys (:words db))]
    [:ul {}
     (for [word words]
       [:li {:key word} [:span
                         word
                         " "
                         [:a {:style    link-style
                              :on-click #(swap! *db assoc :selected-word word)}
                          "[select]"]
                         [:a {:style    link-style
                              :on-click #(swap! *db update :words dissoc word)} "[X]"]
                         (when-not (get-in db [:words word :sentence])
                           [:span {:style {:font-style "bold"
                                           :color      "red"}}
                            "NO SENTENCE"])]])]))

(rum/defc debug < rum/reactive
  []
  (let [db (rum/react *db)]
    [:div {}
     (str db)]))

(rum/defc root []
  [:div {}
   [:div [:h1 "Input"]
    (text-input)]
   (existing-words)
   (debug)])

(rum/mount (root) (js/document.getElementById "app"))
