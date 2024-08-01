(ns ptera
	(:refer-clojure :exclude [use import])
	(:require
		[scad-clj.scad :refer [write-scad]]
		[scad-clj.model :refer :all]
	)
)

(defn deg2rad [degrees] (* (/ degrees 180) pi))

(defn half [num] (/ num 2))

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 5)
(def ncols 6)

(def col-curve (/ pi 12)) ; curvature of the columns
(def centerrow (- nrows 3)) ; controls front-back tilt
(def tenting-angle (deg2rad 13)) ; change this for precise tenting control

(defn column-offset [column]
	(cond
		(= column 2) [0 2.82 -4.5]
		(>= column 4) [0 -8 5.64]
		:else [0 0 0]
	)
)

(def keyboard-z-offset 25) ; controls overall height

(def extra-width 2.5) ; extra space between the base of keys
(def extra-height 1.0)

(def wall-z-offset -15) ; length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 8) ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 2) ; wall thickness parameter

;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def lastcol (dec ncols))
(def cornerrow (if (>= ncols 4) (dec lastrow) lastrow))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def sa-profile-key-height 12.7)

(def plate-thickness 4)

(def keyswitch-width 14.4)
(def keyswitch-height 14.4)

(def mount-space 3)

(def mount-width (+ keyswitch-width mount-space))
(def mount-height (+ keyswitch-height mount-space))

(def single-plate
	(let [
		nub-width 2.75
		nub-radius 1
		plate-half
			(union
				; Top Wall
				(translate [0 (+ (/ mount-space 4) (half keyswitch-height)) (half plate-thickness)]
					(cube mount-width (half mount-space) plate-thickness)
				)
				; Left Wall
				(translate [(+ (/ mount-space 4) (half keyswitch-width)) 0 (half plate-thickness)]
					(cube (half mount-space) mount-height plate-thickness)
				)
				; "Side Nub"
				(hull
					(translate [(+ (/ mount-space 4) (half keyswitch-width)) 0 (half plate-thickness)]
						(cube (half mount-space) nub-width plate-thickness)
					)
					(translate [(+ (half keyswitch-width)) 0 nub-radius]
						(rotate (half pi) [1 0 0]
							(with-fn 100 (cylinder nub-radius nub-width))
						)
					)
				)
			)
	] (union
		plate-half
		(rotate pi [0 0 1] plate-half)
	))
)

;;;;;;;;;;;;;;;
;; SA Keycap ;;
;;;;;;;;;;;;;;;

(def sa-cap
	(let [
		bl2 (half 18.5)
		m (half 17)
		key-cap
			(hull
				(->>
					(polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
					(extrude-linear {:height 0.1 :twist 0 :convexity 0})
					(translate [0 0 0.05])
				)
				(->>
					(polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
					(extrude-linear {:height 0.1 :twist 0 :convexity 0})
					(translate [0 0 6])
				)
				(->>
					(polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
					(extrude-linear {:height 0.1 :twist 0 :convexity 0})
					(translate [0 0 12])
				)
			)
		] (->>
			key-cap
			(translate [0 0 (+ 5 plate-thickness)])
			(color [220/255 163/255 163/255 1])
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range 0 ncols))
(def rows (range 0 nrows))

; Puts the shape at the correct position and rotation for the specified key
(defn key-place [column row shape]
	(let [
		cap-top-height (+ plate-thickness sa-profile-key-height)
		row-radius (+ (/ (half (+ mount-height extra-height)) (Math/sin (half col-curve))) cap-top-height)
	] (->>
		shape
		(translate [0 0 (- row-radius)])
		(rotate (* col-curve (- centerrow row)) [1 0 0])
		(translate [0 0 row-radius])
		(translate [(* column (+ mount-width extra-width)) 0 0])
		(translate (column-offset column))
		(rotate tenting-angle [0 1 0])
		(translate [0 0 keyboard-z-offset])
	))
)

(defn valid-key [col row]
	(and
		(>= col 0)
		(>= row 0)
		(< col ncols)
		(< row nrows)
		(not (and (.contains [4 5] col) (= row lastrow)))
	)
)

(defn all-keys-for
	([block] (for [
		col columns
		row rows
		:when (valid-key col row)
	] (block col row)))
	([condition block] (for [
		col columns
		row rows
		:when (valid-key col row)
		:when (condition col row)
	] (block col row)))
)

;;;;;;;;;;;;;;;
;; Main Keys ;;
;;;;;;;;;;;;;;;

(def main-keys
	(union
		(all-keys-for #(key-place %1 %2 single-plate))
	)
)

(def main-caps
	(union
		(all-keys-for #(key-place %1 %2 sa-cap))
	)
)

;;;;;;;;;;;;;;;;
;; Thumb Keys ;;
;;;;;;;;;;;;;;;;

(defn place-thumb [row shape]
	(->>
		shape
		(translate [(- (half mount-width)) 0 0])
		(rotate (deg2rad -60) [0 1 0])
		(translate [(- (half mount-width)) 0 0])
		(key-place 0 row)
	)
)

(def thumb-keys
	(union
		(for [y (range 0 nrows)]
			(place-thumb y single-plate)
		)
	)
)

(def thumb-caps
	(union
		(for [y (range 0 nrows)]
			(place-thumb y sa-cap)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;
;; Main Connectors ;;
;;;;;;;;;;;;;;;;;;;;;

(def web-posts
	(let [
		post-size 0.1
		web-post
			(translate [0 0 (half plate-thickness)]
				(cube post-size post-size plate-thickness)
			)
		post-adj (half post-size)
		half-mw (half mount-width)
		half-mh (half mount-height)
	] [
		(translate [(- half-mw post-adj) (- half-mh post-adj) 0] web-post)
		(translate [(- post-adj half-mw) (- half-mh post-adj) 0] web-post)
		(translate [(- post-adj half-mw) (- post-adj half-mh) 0] web-post)
		(translate [(- half-mw post-adj) (- post-adj half-mh) 0] web-post)
	])
)

(def web-post-tr (get web-posts 0))
(def web-post-tl (get web-posts 1))
(def web-post-bl (get web-posts 2))
(def web-post-br (get web-posts 3))

(def connectors
	(union
		;; Row connections
		(all-keys-for
			(fn [col row] (valid-key (inc col) row))
			(fn [col row] (hull
				(key-place (inc col) row web-post-tl)
				(key-place col row web-post-tr)
				(key-place (inc col) row web-post-bl)
				(key-place col row web-post-br)
			))
		)
		;; Column connections
		(all-keys-for
			(fn [col row] (valid-key col (inc row)))
			(fn [col row] (hull
				(key-place col row web-post-bl)
				(key-place col row web-post-br)
				(key-place col (inc row) web-post-tl)
				(key-place col (inc row) web-post-tr)
			))
		)
		;; Diagonal connections
		(all-keys-for
			(fn [col row] (valid-key (inc col) (inc row)))
			(fn [col row] (hull
				(key-place col row web-post-br)
				(key-place col (inc row) web-post-tr)
				(key-place (inc col) row web-post-bl)
				(key-place (inc col) (inc row) web-post-tl)
			))
		)
		; Special connection because of the missing row
		(if (>= ncols 4)
			(union
				(hull
					(key-place 3 (- lastrow 1) web-post-br)
					(key-place 4 (- lastrow 1) web-post-bl)
					(key-place 3 lastrow web-post-tr)
				)
				(hull
					(key-place 4 (- lastrow 1) web-post-bl)
					(key-place 3 lastrow web-post-tr)
					(key-place 3 lastrow web-post-br)
				)
				(hull
					(key-place 4 (- lastrow 1) web-post-bl)
					(key-place 4 (- lastrow 1) web-post-br)
					(key-place 3 lastrow web-post-br)
				)
			) ()
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;
;; Thumb Connectors ;;
;;;;;;;;;;;;;;;;;;;;;;

(def thumb-connectors
	(union
		; Between thumb buttons
		(for [y (range 1 nrows)]
			(hull
				(place-thumb y web-post-tl)
				(place-thumb y web-post-tr)
				(place-thumb (dec y) web-post-bl)
				(place-thumb (dec y) web-post-br)
			)
		)
		; Between thumb buttons and main buttons
		(for [y (range 0 nrows)]
			(hull
				(place-thumb y web-post-tr)
				(place-thumb y web-post-br)
				(key-place 0 y web-post-tl)
				(key-place 0 y web-post-bl)
			)
		)
		; Diagonal connections
		(for [y (range 1 nrows)]
			(hull
				(place-thumb y web-post-tr)
				(place-thumb (dec y) web-post-br)
				(key-place 0 y web-post-tl)
				(key-place 0 (dec y) web-post-bl)
			)
		)
	)
)

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(defn bottom [height p]
	(->>
		(project p)
		(extrude-linear {:height height :twist 0 :convexity 0})
		(translate [0 0 (- (half height) 10)])
	)
)

(defn bottom-hull [& p]
	(hull p (bottom 0.001 p))
)

(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) -1])
(defn wall-locate2 [dx dy] [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])
(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset])

(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
	(union
		(hull
			(place1 post1)
			(place1 (translate (wall-locate1 dx1 dy1) post1))
			(place1 (translate (wall-locate2 dx1 dy1) post1))
			(place1 (translate (wall-locate3 dx1 dy1) post1))
			(place2 post2)
			(place2 (translate (wall-locate1 dx2 dy2) post2))
			(place2 (translate (wall-locate2 dx2 dy2) post2))
			(place2 (translate (wall-locate3 dx2 dy2) post2))
		)
		(bottom-hull
			(place1 (translate (wall-locate2 dx1 dy1) post1))
			(place1 (translate (wall-locate3 dx1 dy1) post1))
			(place2 (translate (wall-locate2 dx2 dy2) post2))
			(place2 (translate (wall-locate3 dx2 dy2) post2))
		)
	)
)

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
	(wall-brace
		(partial key-place x1 y1) dx1 dy1 post1
		(partial key-place x2 y2) dx2 dy2 post2
	)
)

(defn half-wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
	(bottom-hull
		(place1 post1)
		(place1 (translate (wall-locate1 dx1 dy1) post1))
		(place2 post2)
		(place2 (translate (wall-locate1 dx2 dy2) post2))
	)
)

(defn half-key-wall-brace [row1 dx1 dy1 post1 row2 dx2 dy2 post2]
	(half-wall-brace
		(partial place-thumb row1) dx1 dy1 post1
		(partial place-thumb row2) dx2 dy2 post2
	)
)

(defn half-corner [row dy post alt-post]
	(union
		(hull
			(place-thumb row alt-post)
			(key-place 0 row post)
			(key-place 0 row (translate (wall-locate1 0 dy) post))
		)
		(hull
			(place-thumb row alt-post)
			(place-thumb row post)
			(key-place 0 row (translate (wall-locate1 0 dy) post))
			(key-place 0 row (translate (wall-locate2 0 dy) post))
			(key-place 0 row (translate (wall-locate3 0 dy) post))
		)
		(hull
			(place-thumb row post)
			(place-thumb row (translate (wall-locate1 -1 0) post))
			(key-place 0 row (translate (wall-locate2 0 dy) post))
			(key-place 0 row (translate (wall-locate3 0 dy) post))
		)
		(bottom-hull
			(place-thumb row post)
			(place-thumb row (translate (wall-locate1 -1 0) post))
			(key-place 0 row (translate (wall-locate2 0 dy) post))
			(key-place 0 row (translate (wall-locate3 0 dy) post))
		)
	)
)

(def case-walls
	(union
		; Back wall
		(for [x (range 0 ncols)] (key-wall-brace
			x 0 0 1 web-post-tl
			x 0 0 1 web-post-tr
		))
		(for [x (range 1 ncols)] (key-wall-brace
			x 0 0 1 web-post-tl
			(dec x) 0 0 1 web-post-tr
		))
		; Front wall
		(for [x (range 0 (min 4 ncols))] (key-wall-brace
			x lastrow 0 -1 web-post-bl
			x lastrow 0 -1 web-post-br
		))
		(for [x (range 1 (min 4 ncols))] (key-wall-brace
			x lastrow 0 -1 web-post-bl
			(dec x) lastrow 0 -1 web-post-br
		))
		(key-wall-brace
			5 cornerrow 0 -1 web-post-bl
			5 cornerrow 0 -1 web-post-br
		)
		(key-wall-brace
			5 cornerrow 0 -1 web-post-bl
			4 cornerrow 0 -1 web-post-br
		)
		(key-wall-brace
			3 lastrow 0 -1 web-post-br
			4 cornerrow 0 -1 web-post-br
		)
		; Left wall
		(for [y (range 0 nrows)] (half-key-wall-brace
			y -1 0 web-post-tl
			y -1 0 web-post-bl
		))
		(for [y (range 1 nrows)] (half-key-wall-brace
			y -1 0 web-post-tl
			(dec y) -1 0 web-post-bl
		))
		; Right wall
		(for [y (range 0 (inc cornerrow))] (key-wall-brace
			lastcol y 1 0 web-post-tr
			lastcol y 1 0 web-post-br
		))
		(for [y (range 1 (inc cornerrow))] (key-wall-brace
			lastcol y 1 0 web-post-tr
			lastcol (dec y) 1 0 web-post-br
		))
		; Corners
		(half-corner 0 1 web-post-tl web-post-tr)
		(half-corner lastrow -1 web-post-bl web-post-br)
		(key-wall-brace
			lastcol 0 0 1 web-post-tr
			lastcol 0 1 0 web-post-tr
		)
		(key-wall-brace
			lastcol (dec lastrow) 0 -1 web-post-br
			lastcol (dec lastrow) 1 0 web-post-br
		)
	)
)

(spit "things/right.scad"
	(write-scad
		(difference
			(union
				main-keys
				thumb-keys
				connectors
				thumb-connectors
				case-walls
			)
			(translate [0 0 -20] (cube 350 350 40))
		)
	)
)

(spit "things/right-test.scad"
	(write-scad
		(union
			(color [0.2 0.2 0.8 1] main-keys)
			(color [0.2 0.8 0.2 1] thumb-keys)
			(color [0.2 0.8 0.8 1] connectors)
			(color [0.8 0.2 0.2 1] thumb-connectors)
			(color [0.8 0.2 0.8 1] case-walls)
			(color [0.8 0.8 0.2 1] main-caps)
			(color [0.8 0.8 0.8 1] thumb-caps)
		)
	)
)

(defn -main [dum] 1) ; dummy to make it easier to batch
