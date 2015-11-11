(ns scad-clj.write
  (:require [clojure.string :refer [join]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multimethod

(defmulti write-expr
  (fn [depth [form & args]]
    (if (keyword? form) form :list)))

(defmethod write-expr :default [depth [form & args]]
  `("//(" ~form ~args ")"))

(defmethod write-expr :list [depth [& args]]
  (mapcat #(write-expr depth %1) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility

(defn indent [depth]
  (join (repeat depth "  ")))

(defn write-block [depth block]
  (mapcat #(write-expr (inc depth) %1) block))

(defn write-level [operation depth block]
  (concat
    (list (indent depth) (str operation " {\n"))
    (write-block depth block)
    (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note/Comment

(defmethod write-expr :comment [depth [form text]]
  (list (indent depth) "// " text "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifier

(defmethod write-expr :modifier [depth [form modifier & block]]
  (write-level (str modifier "union()") depth block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; include and call into scad libraries.

(declare map-to-arg-string)

(defn make-arguments [args]
  (let [arg (first args)
        rest (rest args)
        piece (cond
               (map? arg) (map-to-arg-string arg)
               (coll? arg) (str "[" (make-arguments arg) "]")
               :else arg)]
    (if (empty? rest)
      piece
      (join ", " [piece (make-arguments rest)]))))

(defn map-to-arg-string [m]
  (join ", " (map (fn [[k v]] (str (name k) "=" (make-arguments [v])) ) m)))

(defmethod write-expr :include [depth [form {:keys [library]}]]
  (list (indent depth) "include <" library">\n"))

(defmethod write-expr :use [depth [form {:keys [library]}]]
  (list (indent depth) "use <" library">\n"))

(defmethod write-expr :import [depth [form file]]
  (list (indent depth) "import(\"" file "\");\n"))

(defmethod write-expr :call [depth [form {:keys [function]} & args]]
  (list (indent depth) function "(" (make-arguments (apply vec args)) ");\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2D

(defmethod write-expr :circle [depth [form {:keys [r fa fn fs center]}]]
  (let [fargs (str (and fa (str "$fa=" fa ", "))
                   (and fn (str "$fn=" fn ", "))
                   (and fs (str "$fs=" fs ", ")))]
    (list (indent depth) "circle(" fargs "r=" r ");\n")))

(defmethod write-expr :square [depth [form {:keys [x y center]}]]
  (list (indent depth) "square([" x ", " y "]"
        (when center ", center=true") ");\n"))

(defmethod write-expr :polygon [depth [form {:keys [points paths convexity]}]]
  `(~@(indent depth) "polygon("
    "points=[[" ~(join "], [" (map #(join ", " %1) points)) "]]"
    ~@(when paths [", paths=[[" (join "], [" (map #(join "," %1) paths)) "]]"])
    ~@(when convexity [", convexity=" convexity])
    ");\n"))

(defmethod write-expr :text [depth [form {:keys [text size font halign valign spacing direction language script]}]]
  (list (indent depth) "text(\"" text "\""
        (when size (str ", size=" size))
        (when font (str ", font=\"" font "\""))
        (when halign (str ", halign=\"" halign "\""))
        (when valign (str ", valign=\"" valign "\""))
        (when spacing (str ", spacing=" spacing))
        (when direction (str ", direction=\"" direction "\""))
        (when language (str ", language=\"" language "\""))
        (when script (str ", script=\"" script "\""))");\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3D

(defmethod write-expr :sphere [depth [form {:keys [r fa fn fs center]}]]
  (let [fargs (str (and fa (str "$fa=" fa ", "))
                   (and fn (str "$fn=" fn ", "))
                   (and fs (str "$fs=" fs ", ")))]
    (list (indent depth) "sphere(" fargs "r=" r ");\n")))

(defmethod write-expr :cube [depth [form {:keys [x y z center]}]]
  (list (indent depth) "cube([" x ", " y ", " z "]"
        (when center ", center=true") ");\n"))

(defmethod write-expr :cylinder [depth [form {:keys [h r r1 r2 fa fn fs center]}]]
  (let [fargs (str (and fa (str "$fa=" fa ", "))
                   (and fn (str "$fn=" fn ", "))
                   (and fs (str "$fs=" fs ", ")))]
    (concat
     (list (indent depth) "cylinder(" fargs "h=" h)
     (if r (list ", r=" r) (list ", r1=" r1 ", r2=" r2))
     (when center (list ", center=true"))
     (list ");\n"))))

(defmethod write-expr :polyhedron [depth [form {:keys [points faces convexity]}]]
  `(~@(indent depth) "polyhedron("
    "points=[[" ~(join "], [" (map #(join ", " %1) points)) "]], "
    "faces=[[" ~(join "], [" (map #(join ", " %1) faces)) "]]"
    ~@(if (nil? convexity) [] [", convexity=" convexity])
    ");\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transformations

(defmethod write-expr :resize [depth [form {:keys [x y z auto]} & block]]
  (concat
   (list (indent depth) "resize([" x ", " y ", " z "]")
   (list (when-not (nil? auto)
           (str ", auto="
                (if (coll? auto)
                  (str "[" (join ", " (map true? auto)) "]")
                  (true? auto)))))
   ") {\n"
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :translate [depth [form [x y z] & block]]
  (write-level (str "translate([" x ", " y ", " z "])") depth block))

(defmethod write-expr :rotatev [depth [form [a [x y z]] & block]]
  (write-level (str "rotate(a=" a ", v=[" x ", " y ", " z "])") depth block))

(defmethod write-expr :rotatec [depth [form [x y z] & block]]
  (write-level (str "rotate([" x ", " y ", " z "])") depth block))

(defmethod write-expr :scale [depth [form [x y z] & block]]
  (write-level (str "scale([" x ", " y ", " z "])") depth block))

(defmethod write-expr :mirror [depth [form [x y z] & block]]
  (write-level (str "mirror([" x ", " y ", " z "])") depth block))

(defmethod write-expr :color [depth [form [r g b a] & block]]
  (write-level (str "color([" r ", " g ", " b ", " a"])") depth block))

(defmethod write-expr :hull [depth [form & block]]
  (write-level "hull()" depth block))

(defmethod write-expr :offset [depth [form {:keys [r]} & block]]
  (write-level (str "offset(r=" r ")") depth block))

(defmethod write-expr :minkowski [depth [form & block]]
  (write-level "minkowski()" depth block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boolean operations

(defmethod write-expr :union [depth [form & block]]
  (write-level "union()" depth block))

(defmethod write-expr :difference [depth [form & block]]
  (write-level "difference()" depth block))

(defmethod write-expr :intersection [depth [form & block]]
  (write-level "intersection()" depth block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other

(defmethod write-expr :projection [depth [form {:keys [cut]} & block]]
  (write-level (str "projection(cut=" cut ")") depth block))

(defmethod write-expr :extrude-linear [depth [form {:keys [height twist convexity center]} & block]]
  (concat
   (list (indent depth) "linear_extrude(height=" height)
   (if (nil? twist) [] (list ", twist=" twist))
   (if (nil? convexity) [] (list ", convexity=" convexity))
   (when center (list ", center=true"))
   (list ") {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :extrude-rotate [depth [form {:keys [convexity]} & block]]
  (concat
   (list (indent depth) "rotate_extrude(")
   (if (nil? convexity) [] (list "convexity=" convexity))
   (list ") {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :render [depth [form {:keys [convexity]} & block]]
  (write-level (str "render(convexity=" convexity ")") depth block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special variables

(defmethod write-expr :fa [depth [form x]]
  (list (indent depth) "$fa=" x ";\n"))

(defmethod write-expr :fn [depth [form x]]
  (list (indent depth) "$fn=" x ";\n"))

(defmethod write-expr :fs [depth [form x]]
  (list (indent depth) "$fs=" x ";\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; output

(defn write-scad [& block]
  (join (write-expr 0 block)))
