(ns ^:figwheel-always todo.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! alts! chan]]))

(enable-console-print!)

(defonce app-state (atom {:items []}))

(defn add-item [data owner]
  (let [new-item (.-value (om/get-node owner "new-item"))]
    (when new-item
      (om/transact! data :items #(conj % {:name new-item}))
      (om/set-state! owner :text ""))))

(defn delete-item [item]
  (fn [items] (vec (remove #(= item %) items))))

(defn toggle-done [item]
  (fn [items]
    (replace {item (assoc item :done (not (:done item)))} items)))

(defn handle-change [e owner]
  (om/set-state! owner :text (.. e -target -value)))

(defn handle-enter [e data owner]
  (when (= (.-key e) "Enter")
    (add-item data owner)))


(defn items-left [items]
  (let [active-left (filter (comp not :done) items)]
    (str (count active-left) " items left")))

(defn clear-button [data]
  (let [done-items (filter :done (:items data))]
    (dom/button
     #js {:onClick #(doseq [item done-items]
                      (om/transact! data :items (delete-item item)))}
     (str "clear done (" (count done-items) ")"))))

(defn filter-category [name owner]
  (let [filter      (om/get-state owner :filter)
        font-weight (when (= filter name) "bold")]
    (dom/li
     #js {:style #js {:display "inline" :margin-right "5px"}}
     (dom/a
      #js {:href    "#"
           :style   #js {:color           "black"
                         :text-decoration "none"
                         :font-weight     font-weight}
           :value   name
           :onClick #(om/set-state! owner :filter (.. % -target -text))}
      name))))

(defn items-filter [owner]
  (apply dom/ul
         #js {:style #js {:list-style-type "none"
                          :margin "10px 0 0 0"
                          :padding 0}}
         (map #(filter-category % owner) ["all" "active" "done"])))

(defn filtered-items [items category]
  (condp = category
   "active" (filter (comp not :done) items)
   "done"   (filter :done items)
   items))

(defn todo-item [item owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [delete done]}]
      (dom/div
       nil
       (dom/input
        #js {:type "checkbox"
             :onClick #(put! done @item)
             :checked (:done item)})
       (dom/span
        #js {:style #js {:text-decoration (when (:done item) "line-through")
                         :display         "inline-block"
                         :width           "300px"
                         :margin          "0 10px"}}
        (:name item))
       (dom/button #js {:onClick #(put! delete @item)} "delete")))))

(defn todo-list [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:delete (chan)
       :done   (chan)
       :text   ""
       :filter "all"})
    om/IWillMount
    (will-mount [_]
      (let [delete (om/get-state owner :delete)
            done   (om/get-state owner :done)]
        (go (loop []
              (let [[item ch] (alts! [delete done])]
                (condp = ch
                  delete (om/transact! data :items (delete-item item))
                  done   (om/transact! data :items (toggle-done item)))
                (recur))))))
    om/IRenderState
    (render-state [_ state]
      (dom/div
       nil
       (dom/h2 nil "todos")
       (dom/div
        nil
        (dom/input
         #js {:type     "text"
              :ref      "new-item"
              :value    (:text state)
              :onChange #(handle-change % owner)
              :onKeyUp  #(handle-enter % data owner)}))
       (dom/p nil (items-left (:items data)))
       (clear-button data)
       (items-filter owner)
       (apply dom/div
              #js {:style #js {:margin-top "10px"}}
              (om/build-all todo-item
                            (filtered-items (:items data) (:filter state))
                            {:init-state state}))))))

(om/root
  todo-list
  app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload [])
