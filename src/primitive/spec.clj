(ns primitive.spec
  (:require [clojure.spec.alpha :as s]))


;; currently only rectangles supported
(s/def ::Shape #{:Rect})
(s/def ::image java.awt.image.BufferedImage)
(s/def ::color java.awt.Color)



x y width height angle color

(s/def ::Rect
  (s/keys :req [::x1 ::y1 ::width ::height :color]
          :opt [::phone]))

(s/def ::worker
  (s/keys :req [::first-name ::last-name ::email]
          :opt [::phone]))



; First Data structure takes the initial paramaters:
; an image, a shape type, and will keep track of
;
; after intial state create
;
;  First round creates a nubmer of workers which need:
  ;  the original image.
  ;  a copy of the original image.
  ;  a shape.
  ;
  ; create a random shape score each worker and keep the best initial condition
  ; the best worker then takes the original AND the best starting point it then
    ; hill climbs over x # of possiblites and returns the best
    ; the best image is then added to the list along with the score

(defn primitive-model
  "hold all of the data neede by the primitive app"
  [img rounds shape]
  {:base-image  img
   :rounds      rounds
   :curentround rounds
   :shape       shape
   :frames      []
   :scores      []})


(defrecord Worker [w h target current rnd score counter])


(defn initialize-new-round
  "create a worker structure which will hold one round's worth of workers"
  [primitive-model])



(last [1 2 3])
(last 1 2 3)
(first [1 2 3])

;; Functions
(defmulti area :Shape)
(defmulti mutate :Shape)


;; Members
(defn rectangle [wd ht]
  {:Shape :Rect
   :wd wd
   :ht ht})

(defn circle [radius]
  {:Shape :Circle
   :radius radius})


;; methods
(defmethod area :Rect [r]
  (* (:wd r) (:ht r)))

(defmethod area :Circle [c]
  (* (. Math PI) (* (:radius c) (:radius c))))


(defmethod area :Rect [r]
  (* (:wd r) (:ht r)))

(defmethod area :Circle [c]
  (let [newX ()
        newY ()
        newWidth])
  (* (. Math PI) (* (:radius c) (:radius c))))



