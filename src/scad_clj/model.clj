(ns scad-clj.model
  (:refer-clojure :exclude [import use])
  (:require [clojure.walk :refer [postwalk]]
            [clojure.core.match :refer [match]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special variables

(defn fa! [x]
  `(:fa ~x))

(defn fn! [x]
  `(:fn ~x))

(defn fs! [x]
  `(:fs ~x))

(def ^:dynamic *fa* false)
(def ^:dynamic *fn* false)
(def ^:dynamic *fs* false)
(def ^:dynamic *center* true)

(defn with-f* [f x block]
  `(binding [~f ~x]
     (postwalk identity (list ~@block))))

(defmacro with-fa [x & block]
  (with-f* 'scad-clj.model/*fa* x block))

(defmacro with-fn [x & block]
  (with-f* 'scad-clj.model/*fn* x block))

(defmacro with-fs [x & block]
  (with-f* 'scad-clj.model/*fs* x block))

(defmacro with-center [x & block]
  (with-f* 'scad-clj.model/*center* x block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifier

(defn modifier [modifier & block]
  (if (some #{modifier} [:# :% :* :!])
    `(:modifier ~(name modifier) ~@block)))

(defn -# [& block] (modifier :# block))
(defn -% [& block] (modifier :% block))
(defn -* [& block] (modifier :* block))
(defn -! [& block] (modifier :! block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include & call into Scad libraries

(defn import [file]
  `(:import ~file))

(defn include [library]
  `(:include {:library ~library}))

(defn use [library]
  `(:use {:library ~library}))

(defn libraries [& {uses :use includes :include}]
  (concat
   (map use uses)
   (map include includes)))

(defn call [function & args]
  `(:call {:function ~(name function)} ~args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2D

(defn square [x y & {:keys [center] :or {center *center*}}]
  `(:square ~{:x x, :y y, :center center}))

(defn circle [r]
  (let [args (merge {:r r}
                    (if *fa* {:fa *fa*})
                    (if *fn* {:fn *fn*})
                    (if *fs* {:fs *fs*}))]
    `(:circle ~args)))

(defn polygon
  ([points]
     `(:polygon {:points ~points}))
  ([points paths & {:keys [convexity]}]
     `(:polygon {:points ~points, :paths ~paths, :convexity ~convexity})))

(defn text [text & {:as args}]
  (let [args (merge {:text text} args)]
    `(:text ~args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3D

(defn sphere [r]
  (let [args (merge {:r r}
                    (if *fa* {:fa *fa*})
                    (if *fn* {:fn *fn*})
                    (if *fs* {:fs *fs*}))]
    `(:sphere ~args)))

(defn cube [x y z & {:keys [center] :or {center *center*}}]
  `(:cube ~{:x x, :y y, :z z, :center center}))

(defn cylinder [rs h & {:keys [center] :or {center *center*}}]
  (let [fargs (merge (if *fa* {:fa *fa*})
                     (if *fn* {:fn *fn*})
                     (if *fs* {:fs *fs*}))]
    (match [rs]
      [[r1 r2]] `(:cylinder ~(merge fargs {:h h, :r1 r1, :r2 r2, :center center}))
      [r]       `(:cylinder ~(merge fargs {:h h, :r r, :center center})))))

(defn polyhedron
  ([points faces]
    `(:polyhedron {:points ~points :faces ~faces}))
  ([points faces & {:keys [convexity]}]
    `(:polyhedron {:points ~points :faces ~faces :convexity ~convexity})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transformations

(defn resize [[x y z] & block]
  (let [is-auto (and (keyword? (first block))
                     (= :auto (first block)))
        auto (if is-auto (second block))
        block (if is-auto (rest (rest block)) block)]
    `(:resize {:x ~x :y ~y :z ~z :auto ~auto} ~@block)))

(defn translate [[x y z] & block]
  `(:translate [~x ~y ~z] ~@block))

; multi-arity can't have more than one signature with variable arity. '&'.
(defn rotatev [a [x y z] & block]
  `(:rotatev [~a [~x ~y ~z]] ~@block))

(defn rotatec [[x y z] & block]
  `(:rotatec [~x ~y ~z] ~@block))

(defn rotate [& block]
  (if (number? (first block))
    (rotatev (first block) (second block) (rest (rest block)))
    (rotatec (first block) (rest block))))

(defn scale [[x y z] & block]
  `(:scale [~x ~y ~z] ~@block))

(defn mirror [[x y z] & block]
  `(:mirror [~x ~y ~z] ~@block))

(defn color [[r g b a] & block]
  `(:color [~r ~g ~b ~a] ~@block))

(defn hull [ & block]
  `(:hull  ~@block))

(defn offset [r & block]
  `(:offset {:r ~r} ~@block))

(defn minkowski [ & block]
  `(:minkowski ~@block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boolean operations

(defn union [ & block]
  `(:union  ~@block))

(defn intersection [ & block]
  `(:intersection  ~@block))

(defn difference [ & block]
  `(:difference  ~@block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other

(defn extrude-linear [{:keys [height twist convexity center] :or {center *center*}} & block]
  `(:extrude-linear {:height ~height :twist ~twist :convexity ~convexity :center ~center} ~@block))

(defn extrude-rotate
  ([ block ] `(:extrude-rotate {} ~block))
  ([{:keys [convexity]} block] `(:extrude-rotate {:convexity ~convexity} ~block))
  )

(defn projection [cut & block]
  `(:projection {:cut cut} ~@block))

(defn project [& block]
  `(:projection {:cut false} ~@block))

(defn cut [& block]
  `(:projection {:cut true} ~@block))

(defn render [& block]
  (if (and (seq block)
           (number? (first block)))
    (let [[c & bl] block]
      `(:render {:convexity ~c} ~@bl))
    `(:render {:convexity 1} ~@block)))
