(ns traction.explore
  (:require [clojure.string :as str]))

(defn movement-svg [ev svg]
  (let [from-screen (.inverse (.getScreenCTM svg))
        cx (.-clientX ev)
        cy (.-clientY ev)
        mx (or (some-> (.-deltaX ev) -)
               (.-movementX ev))
        my (or (some-> (.-deltaY ev) -)
               (.-movementY ev))
        start (js/DOMPoint.fromPoint #js{:x (- cx mx)
                                         :y (- cy my)})
        end (js/DOMPoint.fromPoint #js{:x cx
                                       :y cy})
        a (.matrixTransform start from-screen)
        b (.matrixTransform end from-screen)
        dx (- (.-x b) (.-x a))
        dy (- (.-y b) (.-y a))]
    [dx dy]))

(defn set-viewbox! [svg x y width height]
  (.setAttribute svg "viewBox" (str x "," y "," width "," height)))

(defn get-viewbox [svg]
  (map js/parseFloat (str/split (.getAttribute svg "viewBox") #",")))

(defn pan-viewbox [ev svg]
  (when-let [[x y width height] (get-viewbox svg)]
    (let [[dx dy] (movement-svg ev svg)
          x' (- x dx)
          y' (- y dy)]
      (set-viewbox! svg x' y' width height))))

(defn zoom-viewbox [svg [dx dy] point]
  (when-let [[x y width height] (get-viewbox svg)]
    (let [delta (if (> (abs dx) (abs dy))
                  dx
                  dy)]
      (when (not (zero? delta))
        (let [;; Wheel event sizes are not standardized
              scale-factor 1.05
              scale (cond
                      (pos? delta) scale-factor
                      (neg? delta) (/ 1 scale-factor))
              w' (* width scale)
              h' (* height scale)

              ;; Keep the point under the cursor fixed in place.
              ;; Distance from view-box to cursor determines how much to pan.
              ;; Consider 0 distance (cursor is at origin), no pan.
              ;; Consider cursor is at (width,height), full pan.
              ;; Normalized to the same coord system as x,y,width,height
              dw (- width w')
              dh (- height h')
              [pdx pdy] (if point
                          [(* (/ (- (.-x point) x) width) dw)
                           (* (/ (- (.-y point) y) height) dh)]
                          [(/ dw 2.0)
                           (/ dh 2.0)])
              x' (+ x pdx)
              y' (+ y pdy)]
          (set-viewbox! svg x' y' w' h'))))))

(defn coords
  "Normalize a mouse event to the svg layout coord system (x,y,width,height)"
  [point svg]
  (let [from-screen (.inverse (.getScreenCTM svg))
        dom-point (js/DOMPoint.fromPoint point)]
    (.matrixTransform dom-point from-screen)))

(defn listen* [t f]
  (js/window.addEventListener t f #js{:passive false}))

(defn listen [svg]
  (listen* "wheel"
           (fn [ev]
             (.preventDefault ev)
             (let [touchpad? (if (.-wheelDeltaY ev)
                               (= (.-wheelDeltaY ev) (* -3 (.-deltaY ev)))
                               (zero? (.-deltaMode ev)))]
               (if (or (.-ctrlKey ev)
                       (not touchpad?))
                 (zoom-viewbox svg [(.-deltaX ev) (.-deltaY ev)] (coords ev svg))
                 (pan-viewbox ev svg)))))
  (listen* "pointermove"
           (fn [ev]
             (cond (or (.-altKey ev)
                       (not= 0 (.-buttons ev)))
                   (pan-viewbox ev svg)

                   (.-ctrlKey ev)
                   (zoom-viewbox svg (movement-svg ev svg) nil)))))
