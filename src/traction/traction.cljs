; traction -- presentations without slides
; Copyright 2011 (c) Chris Houser. All rights reserved.
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

(ns traction.traction
  (:import (goog.events KeyHandler)
           (goog.net XhrIo))
  (:require [goog.dom :as dom]
            [goog.style :as style]
            [goog.window :as gwin]
            [goog.fx]
            [goog.fx.anim :as anim]
            [goog.events :as events]
            [goog.events.Keys :as k]))

(def svg js/document.documentElement)

(defn parameterize [start end]
  (let [diff (- end start)]
    (fn [t] (+ start (* t diff)))))

(defn client-rect [elem]
  (let [bb (.getBBox elem)]
    {:x (.-x bb)
     :y (.-y bb)
     :width (.-width bb)
     :height (.-height bb)}))

(defn tags [elem tag-name]
  ; (dom/$$ tag-name nil elem) ?
  (prim-seq (.getElementsByTagName elem tag-name) 0))

(defn elem-style [sets]
  (reduce (fn [out elem]
            (if-let [duration (.getAttribute elem "duration")]
              (assoc out :duration (js/parseFloat duration))
              (let [id (.getAttribute elem "i")]
                (if (dom/getElement id)
                  (update-in out [id] assoc :opacity
                             (js/parseFloat (.getAttribute elem "opacity")))
                  (js/alert (str "Couldn't find elem " id ))))))
          {} sets))

(defn compute-steps [config-dom]
  (let [init (elem-style (-> config-dom (tags "init") first (tags "set")))
        steps (tags config-dom "step")]
    (loop [rtn [], default init, [step & more-steps] steps]
      (if-not step
        rtn
        (let [view-rect (if-let [view-id (.getAttribute step "view")]
                          (client-rect
                            (or (dom/getElement view-id)
                                (js/alert (str "Couldn't find view " view-id))))
                          (or (:view default)
                              (js/alert "First step requires a view attr")))
              comp-step (-> (into default (elem-style (tags step "set")))
                            (assoc :view view-rect))
              new-default (assoc (->> (tags step "set")
                                      (remove #(.getAttribute % "once"))
                                      elem-style
                                      (into default))
                                 :view view-rect)]
          (recur
            (conj rtn comp-step)
            new-default
            more-steps))))))

(defn new-transition [start end]
  (if (number? start)
    (if (= start end)
      start
      (parameterize start end))
    (zipmap (keys start)
            (map new-transition (vals start) (vals end)))))

(defn compute-animation [obj t]
  (cond
    (fn? obj) (obj t)
    (map? obj) (zipmap (keys obj) (map #(compute-animation % t) (vals obj)))
    :else obj))

(defn limit-step [old-step steps f]
  (max 0 (min (dec (count steps)) (f old-step))))

; --- end pure functions ---

(defn apply-world [w]
  (doseq [[k v] w]
    (cond
      (= :view k)
        (let [{:keys [x y width height]} v]
          (.setAttribute svg "viewBox" (str x "," y "," width "," height)))
      (= :duration k)
        nil
      :else
        (let [elem (dom/getElement k)]
          (when (:opacity v)
            (set! (.-opacity (.-style elem)) (:opacity v)))))))

(def notes-window (atom nil))

(def computed-steps (atom nil))

(def world (atom nil))

(def step (atom 0))

(def transition (atom nil))

(defn alter-step [f]
  (let [i (swap! step limit-step @computed-steps f)
        current-world @world
        target-world (nth @computed-steps i)]
    (when (not= current-world target-world)
      ; update main window
      (set! js/document.location.hash (str \# (pr-str {'step i})))
      (reset! transition
              (with-meta
                (new-transition current-world target-world)
                {:start (js/Date.) :duration (:duration target-world)}))
      (anim/registerAnimation transition)

      ; update notes window
      (when @notes-window
        (let [id (str "step" i)
              notes-body (.-body (.-document @notes-window))
              notes-dom (dom/getDomHelper notes-body)]
          (set! (.-scrollTop notes-body)
                (style/getPageOffsetTop (.getElement notes-dom id))))))))

(defn set-step [i]
  (alter-step (constantly i)))

(defn open-notes [config-dom]
  ; hack to get around blank notes on reload:
  (. (gwin/openBlank "" (js* "{target: 'traction-notes'}")) (close))

  (let [win (gwin/openBlank
              "" (js* "{target: 'traction-notes',
                        width: ~{}, height: ~{},
                        scrollbars: true, resizable: true}"
                      gwin/DEFAULT_POPUP_WIDTH gwin/DEFAULT_POPUP_HEIGHT))
        body (.-body (.-document win))
        notesdom (dom/getDomHelper body)]
    (reset! notes-window win)
    (dom/append body
      (.createDom notesdom "style" (.strobj {"type" "text/css"})
        "body{ background: #000; color: #eee; padding-bottom: 100%; }
        a { color: #88f; text-decoration: underline; cursor: pointer; }")
      (.createDom notesdom "h4" nil "Speaker's Notes [TractionSVG]"))
    (doseq [[i step] (map-indexed vector (tags config-dom "step"))]
      (let [a (.createDom notesdom "a" (.strobj {"id" (str "step" i)})
                          (str "Step " i ": " (.getAttribute step "view")))]
        (events/listen a "click" ((fn [i] #(set-step i)) i)) ; CLJS-59
        (dom/append body (.createDom notesdom "div" nil a)
                    (.cloneNode step true))))
    (events/listen
      (KeyHandler. body true) (-> KeyHandler
                                  (.-EventType)
                                  (.-KEY))
      (fn [e]
          (condp = (.-key e)
            k/SPACE (alter-step inc)
            k/RIGHT (alter-step inc)
            k/LEFT  (alter-step dec)
            nil)))
    (. win (focus))))

(set! (.-onAnimationFrame transition)
  (fn [t]
    (let [trans @transition
          {:keys [start duration]} (meta trans)
          t (min 1 (/ (- (js/Date.) start) (max duration 1)))]
      (reset! world (compute-animation trans t))
      (apply-world @world)
      (when (>= t 1)
        (anim/unregisterAnimation transition)))))

(set! (.-onload (js* "window"))
  (fn []
    ((.-send XhrIo) "config.xml"
      (fn [x]
        (try
          (let [config (if-let [xml (. (.-target x) (getResponseXml))]
                         (.-documentElement xml)
                         (if-let [elems (.getElementsByTagName svg "steps")]
                            (aget elems 0)
                            (js/alert "No traction steps found")))]
            (reset! computed-steps (compute-steps config))
            (reset! world (nth @computed-steps 0))
            (apply-world @world)
            #_(open-notes config))
          (catch js/Error e
            (js/alert (str "Config not loaded: " e))))))

    ; Hide view boxes
    (doseq [rect (prim-seq (.getElementsByTagName svg "rect") 0)]
      (when (re-find #"^view-" (.-id rect))
        (set! (-> rect .-style .-visibility) "hidden")))

    (events/listen svg "click"
      #(alter-step (if (< 512 (.-clientX %)) inc dec)))

    (events/listen
      (KeyHandler. svg true) (-> KeyHandler
                                 (.-EventType)
                                 (.-KEY))
      (fn [e]
        (condp = (.-key e)
          k/SPACE (alter-step inc)
          k/RIGHT (alter-step inc)
          k/LEFT  (alter-step dec)
          nil)))))
