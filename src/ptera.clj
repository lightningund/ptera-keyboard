(ns ptera
	(:refer-clojure :exclude [use import])
	(:require
		[clojure.core.matrix :refer [mmul]]
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
(def tenting-angle (deg2rad 7)) ; or, change this for more precise tenting control

(defn column-offset [column]
	(cond
		(= column 2) [0 2.82 -4.5]
		(>= column 4) [0 -8 5.64] ; original [0 -5.8 5.64]
		:else [0 0 0]
	)
)

(def keyboard-z-offset 25) ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

(def extra-width 2.5) ; extra space between the base of keys; original= 2
(def extra-height 1.0) ; original= 0.5

(def wall-z-offset -15) ; length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 8) ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 2) ; wall thickness parameter; originally 5

;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def lastcol (dec ncols))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14.4) ; Was 14.1, then 14.25
(def keyswitch-width 14.4)

(def sa-profile-key-height 12.7)

(def plate-thickness 4)
(def mount-width (+ keyswitch-width 3))
(def mount-height (+ keyswitch-height 3))

(def single-plate
	(let [
		plate-half
			(union
				; Top Wall
				(translate [0 (+ 0.75 (half keyswitch-height)) (half plate-thickness)]
					(cube (+ keyswitch-width 3) 1.5 plate-thickness)
				)
				; Left Wall
				(translate [(+ 0.75 (half keyswitch-width)) 0 (half plate-thickness)]
					(cube 1.5 (+ keyswitch-height 3) plate-thickness)
				)
				; "Side Nub"
				(hull
					(translate [(+ 0.75 (half keyswitch-width)) 0 (half plate-thickness)]
						(cube 1.5 2.75 plate-thickness)
					)
					(translate [(+ (half keyswitch-width)) 0 1]
						(rotate (half pi) [1 0 0]
							(with-fn 100 (cylinder 1 2.75))
						)
					)
				)
			)
	] (union
		plate-half
		(mirror [0 1 0] (mirror [1 0 0] plate-half))
	))
)

;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-cap {
	1 (let [
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
	1.5 (let [
		bl2 (half 18.25)
		bw2 (half 28)
		key-cap
			(hull
				(->>
					(polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
					(extrude-linear {:height 0.1 :twist 0 :convexity 0})
					(translate [0 0 0.05]))
				(->>
					(polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
					(extrude-linear {:height 0.1 :twist 0 :convexity 0})
					(translate [0 0 12])
				)
			)
		] (->>
			key-cap
			(translate [0 0 (+ 5 plate-thickness)])
			(color [240/255 223/255 175/255 1])
		)
	)
})

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range 0 ncols))
(def rows (range 0 nrows))

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
	(let [
		cap-top-height (+ plate-thickness sa-profile-key-height)
		row-radius (+ (/ (half (+ mount-height extra-height)) (Math/sin (half col-curve))) cap-top-height)
	] (->>
		shape
		(translate-fn [0 0 (- row-radius)])
		(rotate-x-fn (* col-curve (- centerrow row)))
		(translate-fn [0 0 row-radius])
		(translate-fn [(* column (+ mount-width extra-width)) 0 0])
		(translate-fn (column-offset column))
		(rotate-y-fn tenting-angle)
		(translate-fn [0 0 keyboard-z-offset])
	))
)

; Puts the shape at the correct position and rotation for the specified key
(defn key-place [column row shape]
	(apply-key-geometry
		translate
		(fn [angle obj] (rotate angle [1 0 0] obj))
		(fn [angle obj] (rotate angle [0 1 0] obj))
		column
		row
		shape
	)
)

(defn rotate-around-x [angle position]
	(mmul [
		[1 0 0]
		[0 (Math/cos angle) (- (Math/sin angle))]
		[0 (Math/sin angle) (Math/cos angle)]
	] position)
)

(defn rotate-around-y [angle position]
	(mmul [
		[(Math/cos angle) 0 (Math/sin angle)]
		[0 1 0]
		[(- (Math/sin angle)) 0 (Math/cos angle)]
	] position)
)

; Sets(?) position to the position of the specified key
(defn key-position [column row position]
	(apply-key-geometry
		(partial map +)
		rotate-around-x
		rotate-around-y
		column
		row
		position
	)
)

(defn valid-key [col row]
	(and
		(>= col 0)
		(>= row 0)
		(< col ncols)
		(< row nrows)
		;; (not (and (.contains [4 5] col) (= row lastrow)))
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

(def key-holes
	(union
		(all-keys-for #(key-place %1 %2 single-plate))
	)
)

(def caps
	(apply union
		(all-keys-for #(key-place %1 %2 (sa-cap 1)))
	)
)

;;;;;;;;;;;;;;;;
;; Thumb Keys ;;
;;;;;;;;;;;;;;;;

(defn thumb-col [row shape]
	(->>
		shape
		(translate [-13 0 7.5])
		(rotate (deg2rad -60) [0 1 0])
		(key-place 0 row)
		(color [1 0 0 1])
	)
)

(def thumb
	(union
		(for [y (range 0 nrows)]
			(thumb-col y single-plate)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;
;; Main Connectors ;;
;;;;;;;;;;;;;;;;;;;;;

(def post-size 0.1)
(def web-post
	(translate [0 0 (half plate-thickness)]
		(cube post-size post-size plate-thickness)
	)
)

(def post-adj (half post-size))
(def half-mw (half mount-width))
(def half-mh (half mount-height))
(def web-post-tr (translate [(- half-mw post-adj) (- half-mh post-adj) 0] web-post))
(def web-post-tl (translate [(- post-adj half-mw) (- half-mh post-adj) 0] web-post))
(def web-post-bl (translate [(- post-adj half-mw) (- post-adj half-mh) 0] web-post))
(def web-post-br (translate [(- half-mw post-adj) (- post-adj half-mh) 0] web-post))
(ns-unmap *ns* 'half-mw)
(ns-unmap *ns* 'half-mh)

; Splits the list into groups of 3, with a step of 1
; Then applies `hull` to each, to make a full triangle
; Then creates the union of all the triangles
(defn triangle-hulls [& shapes] (union (map hull (partition 3 1 shapes))))

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
				(thumb-col y web-post-tl)
				(thumb-col y web-post-tr)
				(thumb-col (dec y) web-post-bl)
				(thumb-col (dec y) web-post-br)
			)
		)
		; Between thumb buttons and main buttons
		(for [y (range 0 nrows)]
			(hull
				(thumb-col y web-post-tr)
				(thumb-col y web-post-br)
				(key-place 0 y web-post-tl)
				(key-place 0 y web-post-bl)
			)
		)
		; Diagonal connections
		(for [y (range 1 nrows)]
			(hull
				(thumb-col y web-post-tr)
				(thumb-col (dec y) web-post-br)
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
		;; (translate [0 0 (- (half height) 10)])
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
		(partial thumb-col row1) dx1 dy1 post1
		(partial thumb-col row2) dx2 dy2 post2
	)
)

(defn half-corner [row dy post alt-post]
	(union
		(hull
			(thumb-col row alt-post)
			(key-place 0 row post)
			(key-place 0 row (translate (wall-locate1 0 dy) post))
		)
		(hull
			(thumb-col row alt-post)
			(thumb-col row post)
			(key-place 0 row (translate (wall-locate1 0 dy) post))
			(key-place 0 row (translate (wall-locate2 0 dy) post))
			(key-place 0 row (translate (wall-locate3 0 dy) post))
		)
		(hull
			(thumb-col row post)
			(thumb-col row (translate (wall-locate1 -1 0) post))
			(key-place 0 row (translate (wall-locate2 0 dy) post))
			(key-place 0 row (translate (wall-locate3 0 dy) post))
		)
		(bottom-hull
			(thumb-col row post)
			(thumb-col row (translate (wall-locate1 -1 0) post))
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
		(for [x (range 0 ncols)] (key-wall-brace
			x lastrow 0 -1 web-post-bl
			x lastrow 0 -1 web-post-br
		))
		(for [x (range 1 ncols)] (key-wall-brace
			x lastrow 0 -1 web-post-bl
			(dec x) lastrow 0 -1 web-post-br
		))
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
		(for [y (range 0 nrows)] (key-wall-brace
			lastcol y 1 0 web-post-tr
			lastcol y 1 0 web-post-br
		))
		(for [y (range 1 nrows)] (key-wall-brace
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
			lastcol lastrow 0 -1 web-post-br
			lastcol lastrow 1 0 web-post-br
		)
	)
)

(def model-right
	(difference
		(union
			key-holes
			thumb
			connectors
			thumb-connectors
			case-walls
		)
		;; (translate [0 0 -20] (cube 350 350 40))
	)
)

(spit "things/right.scad" (write-scad model-right))

;; (spit "things/left.scad" (write-scad (mirror [-1 0 0] model-right)))

;; (spit "things/right-test.scad"
;; 	(write-scad
;; 		(union
;; 			key-holes
;; 			connectors
;; 			case-walls
;; 			thumbcaps
;; 			caps
;; 		)
;; 	)
;; )

(defn -main [dum] 1) ; dummy to make it easier to batch
