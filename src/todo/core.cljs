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

(defn item-display [done]
  (when done #js {:text-decoration "line-through"}))

(defn items-left [items]
  (let [active-left (filter (comp not :done) items)]
    (str (count active-left) " items left")))

(defn todo-item [item owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [delete done]}]
      (dom/li
       nil
       (dom/span #js {:style (item-display (:done item))} (:name item))
       (dom/button #js {:onClick #(put! done @item)} (if (:done item) "open" "done"))
       (dom/button #js {:onClick #(put! delete @item)} "delete")))))

(defn todo-list [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:delete (chan)
       :done   (chan)
       :text   ""})
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
       (apply dom/ul
              nil
              (om/build-all todo-item (:items data) {:init-state state}))))))

(om/root
  todo-list
  app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload [])
