(ns primitive.core
  (:require [mikera.image.core :refer [ fill! sub-image new-image graphics width height get-pixel get-pixels copy load-image fill-rect! get-pixels show]])
  (:require [mikera.image.colours :refer [ to-java-color color argb argb-from-components components-argb values-argb extract-alpha with-components rand-colour]])
  (:require [gifclj.core :refer (write-gif)])
  (:import  (java.awt.image BufferedImage))
  (:import (java.awt.geom Rectangle2D Rectangle2D$Double Ellipse2D$Double))
  (:import (java.awt Color Shape Graphics2D AlphaComposite))

  (:gen-class))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions

(defn- add-pixel-values
  "add argb channel values"
  [[^long r1 ^long g1 ^long b1 ^long a1] [^long r2 ^long g2 ^long b2 ^long a2]]
  [(+ r1 r2) (+ g1 g2) (+ b1 b2) (+ a1 a2)])


;(defn- get-nonzero-pixels
;  "filter zeros from integer array of pixel values. returns a vector"
;  [^BufferedImage img]
;  (let [pixels (get-pixels img)]
;    (areduce pixels i ret []
;             (if (zero? (aget pixels i)) ret (conj ret i)))))


(defn- get-channel-values
  "add the values of an images' argb channels"
  [^BufferedImage img]
  (let [;pixels     (get-nonzero-pixels img)
        pixels     (get-pixels img)
        pixel-argb (map components-argb pixels)
        pcount     (count pixel-argb)
        [r g b a]  (reduce add-pixel-values pixel-argb)]
    [(/ r pcount) (/ g pcount) (/ b pcount ) (/ a pcount)]))


(defn image-difference
  "calculate the differences between images"
  (^double  [^BufferedImage img1 ^BufferedImage img2]
    (let [[r1 g1 b1 a1] (get-channel-values img1)
          [r2 g2 b2 a2] (get-channel-values img2)
          [dr dg db da] [(- r1 r2) (- g1 g2) (- b1 b2) (- a1 a2)]
          total         (+ (* dr dr) (* dg dg) (* db db) (* da da))
          w             (width img1)
          h             (height img1)]
      ;(do (println total))
      ;(/ (Math/sqrt (/ total (* w h 4))) 255)
      (Math/sqrt total))))


(defn clip-image-with-shape
  "clip an image with a shape"
  (^BufferedImage [^BufferedImage img ^Shape shape]
   (let [bounds (.getBounds shape)
         dst    (new-image (width img) (height img))
         g      (.getGraphics dst)]
     ;; draw the image on the full canvas then take the sub image
     (.setClip g shape)
     (.drawImage g img 0 0 nil)
     (.dispose g)
     (sub-image dst (.-x bounds) (.-y bounds) (.-width bounds) (.-height bounds)))))


(defn average-color
  "calculate the average color of an image. If a shape is provided,
   calculate the average color of that shape"
  (^Color [^BufferedImage img]
   (let [[r g b a] (get-channel-values img)]
     ;(color (argb-from-components r g b a))
     (color (argb-from-components r g b))))
  (^Color [^BufferedImage img ^Shape shape]
   (let [masked-image (clip-image-with-shape img shape)]
     (average-color masked-image))))


(defn set-alpha
  "set alpha of a color. alpha should be between 0 and 255 inclusive"
  (^Color [^Color col alpha]
    (let [[r g b _] (components-argb (argb col))]
      (to-java-color (argb-from-components r g b alpha)))))


(defn clamp-int
  "ensure int between range"
  [x low high]
  (cond
    (< x low) low
    (> x high) high
    :else x))


(defn rand-mutate
  "mutate and clamp"
  (^long [x]
   (let [posneg (if (= 0 (Math/round (Math/random))) -1 1)
         num    (Math/round (* 16 (Math/random)))]
     (+ x (* posneg num)))))


(defn safe-mutate
  "mutate a number but enforce max/min"
  [x min max]
  (let [num (rand-mutate x)]
    (clamp-int num min max)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control Structures & Protocols

(defrecord Rectangle [x1 y1 x2 y2 xmax ymax angle color])

(defrecord Worker [src dest w h shape score counter])

(defrecord Model [target current Score Shapes Colors Scores Workers])

(defprotocol IShape
  "shape functions"
  (mutate [m])
  (to-java2d [m]))

(defprotocol IWorker
  "worker functions
  extract-color: retrieves the average color of the shape from the source image
  apply-shape:   adds the colored shape to the destination image
  calc-energy:   calculates the difference between the src and dest images
  climb:         mutate the shape and update the image"
  (extract-color [w])
  (apply-shape   [w])
  (calc-energy   [w])
  (climb         [w])
  (climb-n-times [w n]))

(extend-protocol IShape
   Rectangle
   (mutate [R]
     (let [point (rand-int 2)]
       (cond
         (= point 0)
         (-> R (assoc :x1 (safe-mutate (:x1 R) 0 (- (:xmax R) 1)))
               (assoc :y1 (safe-mutate (:y1 R) 0 (- (:ymax R) 1))))
         (= point 1)
         (-> R (assoc :x2 (safe-mutate (:x2 R) 0 (- (:xmax R) 1)))
               (assoc :y2 (safe-mutate (:y2 R) 0 (- (:ymax R) 1)))))))
  (to-java2d [R]
    (let [x1   (get R :x1)   x2 (get R :x2)
          y1   (get R :y1)   y2 (get R :y2)
          minx (min x1 x2)   maxx (max x1 x2)
          miny (min y1 y2)   maxy (max y1 y2)
          w    (- maxx minx) wmin (if (= w 0) 1 w)
          h  (- maxy miny)   hmin (if (= h 0) 1 h)]
      (Rectangle2D$Double. minx miny wmin hmin))))


(extend-protocol IWorker
  Worker
  (extract-color [w]
    (let [jshape (to-java2d (get w :shape))
          color  (average-color (get w :src) jshape)
          color-alpha (set-alpha color 100)]
      color-alpha))
  (apply-shape [w]
    (let [color  (extract-color w)
          g2     (graphics (get w :dest))
          jshape (to-java2d (get w :shape))
          ac     (AlphaComposite/getInstance AlphaComposite/SRC_OVER)]

      (.setComposite g2 ac);
      (.setColor g2 color)
      (.draw g2 jshape)
      (.fill g2 jshape)
      (.dispose g2)
      w))
  (calc-energy [w]
    (let [energy (image-difference (get w :src) (get w :dest))]
      (assoc w :score energy)))
  (climb [w]
    (let [new-image (copy (get w :dest))
          new-worker (-> w
                       (update :shape mutate)
                       (assoc  :dest new-image)
                       (apply-shape)
                       (calc-energy))]
         (do (println (get new-worker :shape)))
         (if (< (:score new-worker) (:score w))
           new-worker
           w)))
  (climb-n-times [w n]
    (loop [i n, worker w]
      (if (= 0 i)
        worker
        (recur (dec i) (climb worker))))))


(< 0 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialization function

(defn initialize-rectangle
  "create a new rectangle"
  (^Rectangle [maxx maxy]
    (let [x1 (rand-int maxx) x2 (rand-int maxx)
          y1 (rand-int maxy) y2 (rand-int maxy)]
      (Rectangle. (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)  maxx maxy nil  nil))))


(defn initialize-shape
  "shape initializer function"
  [shape-type maxx maxy]
  (cond
    (= shape-type :rectangle) (initialize-rectangle maxx maxy)))


(defn initialize-worker
  "create a worker"
  (^Worker [^BufferedImage img shape rounds]
   (let [w         (width img)
         h         (height img)
         shape-rec (initialize-shape shape w h)
         avg-color (-> (average-color img) (set-alpha 200))
         img2      (-> (new-image w h ) (fill! avg-color))
         worker    (Worker. (copy img) img2 w h shape-rec 0 rounds)]
     (-> worker
         (apply-shape)
         (calc-energy)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Algo

(defn primitive-model
  "hold all of the data neede by the primitive app"
  [img rounds shape]
  {:base-image   img
   :rounds       rounds
   :currentround rounds
   :shape        shape
   :frames       []
   :scores       []})


(defn place-initial-shape
  "find a good starting space by making random shapes and choosing the best one"
  (^Worker [model round]
    (let [img    (if (= round 0) (get model :base-image) (last (get model :frames)))
          shape  (get model :shape)]
      (do (println round))
      (loop [loopcount   50
             old-score   100000
             old-worker  nil]
        (let [new-worker (initialize-worker img shape 0)
              new-score  (get new-worker :score)]
          (if (= loopcount 0)
             old-worker
            (recur (dec loopcount)
                   (min old-score new-score)
                   (if (< new-score old-score) new-worker old-worker))))))))


(defn run-primitive
  "run primitive"
  [model rounds]
  (loop [round 0,  mdl model]
    (do (println round) (println mdl))
    (if (= round rounds)
      mdl
      (let [best-starting-point (place-initial-shape mdl round)
            best-worker         (climb-n-times best-starting-point 50)
            new-model           (-> mdl
                                  (update :frames conj (get best-worker :dest))
                                  (update :scores conj (get best-worker :score)))]
          (println "new model!")
          (println new-model)
          (recur (inc round) new-model)))))


(comment
  (def turtle (load-image "resources/turtle.jpg"))
  (def pm (primitive-model turtle 10 :rectangle))

  ;(def mdl3 (run-primitive pm 0))
  ;(def mdl3 (run-primitive pm 1))
  ;(def mdl3 (run-primitive pm 2))
  (def mdl3 (run-primitive pm 6))

  ;(show (nth (get mdl3 :frames) 0))
  ;(show (nth (get mdl3 :frames) 1))
  ;(show (nth (get mdl3 :frames) 2))
  ;(show (nth (get mdl3 :frames) 6))
  ;(show (nth (get mdl3 :frames) 9))

  (write-gif "testgif1.gif" (get mdl3 :frames) :loops 0 :delay 10 :lastdelay 50)

  (def tcolor (average-color turtle))
  (show turtle)
  (show (-> turtle (copy) (fill! Color/RED)))
  (show (-> turtle (copy) (fill! tcolor)))

  (average-color fungi)
  (def w1 (assoc w1 :dest (copy (:src w1))))
  (def w1 (assoc w1 :dest (fill! (:dest w1) Color/RED)))

  (def w1 (initialize-worker turtle :rectangle 1))
  (def w2 (-> w1 (apply-shape) (calc-energy)))
  (show (get w2 :src))
  (show (get w2 :dest))

  (image-difference (get w2 :src) (get w2 :dest))
  (get-channel-values (get w2 :src))
  (get-channel-values (get w2 :dest))
  (get-channel-values (-> (new-image 200 500) (fill! Color/RED)))
  (get w2 :score)
  (def pM (primitive-model fungi 20 :rectangle))

  (def fungi (load-image "resources/Meripilus.jpg"))
  (def turtle (load-image "resources/turtle.jpg"))
  (show fungi)
  (def rect1 (Rectangle2D$Double. 10 100 100 100))
  (def ellipse1  (Ellipse2D$Double. 0 0 200 100))
  (show (clip-image fungi rect1))
  (show (clip-image fungi ellipse1))


  (count (remove (clip-image-with-shape fungi ellipse1)))
  (count (get-pixels fungi))
  (count (get-nonzero-pixels (clip-image-with-shape fungi ellipse1)))
  (count (get-nonzero-pixels fungi))

  (remove-zero pixvalues)

  (show fungi)
  (show fungi1)
  (show fungi2)

  (difference-full fungi fungi1)
  (difference-full fungi fungi2)
  (difference-full fungi1 fungi2))
