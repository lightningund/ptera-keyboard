// ;;;;;;;;;;;;;;;;;;;;;;
// ;; Shape parameters ;;
// ;;;;;;;;;;;;;;;;;;;;;;

nrows = 5;
ncols = 6;

col_curve = 180 / 12; // curvature of the columns
centerrow = nrows - 3; // controls front-back tilt
tenting_angle = 13; // change this for precise tenting control

function column_offset(column) =
	(column == 2) ? [0, -2.82, -4.5] :
	(column >= 4) ? [0, 8, 5.64] : [0, 0, 0];

keyboard_z_offset = 25; // controls overall height

extra_width = 2.5; // extra space between the base of keys
extra_height = 1;

wall_z_offset = -15; // length of the first downward-sloping part of the wall (negative)
wall_xy_offset = 8; // offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
wall_thickness = 2; // wall thickness parameter

// ;;;;;;;;;;;;;;;;;;;;;;;
// ;; General variables ;;
// ;;;;;;;;;;;;;;;;;;;;;;;

lastrow = nrows - 1;
lastcol = ncols - 1;
cornerrow = (ncols >= 4) ? (lastrow - 1) : lastrow;

// ;;;;;;;;;;;;;;;;;
// ;; Switch Hole ;;
// ;;;;;;;;;;;;;;;;;

sa_profile_key_height = 12.7;
plate_thickness = 4;
keyswitch_width = 14.4;
keyswitch_height = 14.4;

mount_space = 3;
mount_width = keyswitch_width + mount_space;
mount_height = keyswitch_height + mount_space;

module single_plate() {
	nub_width = 2.75;
	nub_radius = 1;

	module plate_half() {
		union() {
			// Top Wall
			translate([0, (mount_space / 4) + (keyswitch_height / 2), plate_thickness / 2]) {
				cube([mount_width, mount_space / 2, plate_thickness], center=true);
			}
			// Left Wall
			translate([(mount_space / 4) + (keyswitch_width / 2), 0, plate_thickness / 2]) {
				cube([mount_space / 2, mount_height, plate_thickness], center=true);
			}
			// "Side Nub"
			hull() {
				translate([(mount_space / 4) + (keyswitch_width / 2), 0, plate_thickness / 2]) {
					cube([mount_space / 2, nub_width, plate_thickness], center=true);
				}
				translate([keyswitch_width / 2, 0, nub_radius]) {
					rotate(90, [1, 0, 0]) {
						cylinder(h=nub_width, r=nub_radius, $fn=100, center=true);
					}
				}
			}
		}
	}

	union() {
		plate_half();
		rotate(180, [0, 0, 1]) plate_half();
	}
}

// ;;;;;;;;;;;;;;;
// ;; SA Keycap ;;
// ;;;;;;;;;;;;;;;

module sa_cap() {
	bl2 = 18.5 / 2;
	m = 17 / 2;

	module key_cap() {
		hull() {
			translate([0, 0, 0.05]) {
				linear_extrude(height=0.1) {
					polygon([
						[bl2, bl2],
						[bl2, -bl2],
						[-bl2, -bl2],
						[-bl2, bl2]
					]);
				}
			}
			translate([0, 0, 6]) {
				linear_extrude(height=0.1) {
					polygon([
						[m, m],
						[m, -m],
						[-m, -m],
						[-m, m]
					]);
				}
			}
			translate([0, 0, 12]) {
				linear_extrude(height=0.1) {
					polygon([
						[6, 6],
						[6, -6],
						[-6, -6],
						[-6, 6]
					]);
				}
			}
		}
	}

	color([220/255, 163/255, 163/255, 1]) {
		translate([0, 0, plate_thickness + 5]) {
			key_cap();
		}
	}
}

// ;;;;;;;;;;;;;;;;;;;;;;;;;
// ;; Placement Functions ;;
// ;;;;;;;;;;;;;;;;;;;;;;;;;

// Puts the shape at the correct position and rotation for the specified key
module key_place(col, row) {
	cap_top_height = plate_thickness + sa_profile_key_height;
	row_radius = (((mount_height + extra_height) / 2) / sin(col_curve / 2)) + cap_top_height;

	translate([0, 0, keyboard_z_offset]) {
		rotate(tenting_angle, [0, 1, 0]) {
			translate(column_offset(col)) {
				translate([(mount_width + extra_width) * col, 0, 0]) {
					translate([0, 0, row_radius]) {
						rotate(col_curve * (row - centerrow), [1, 0, 0]) {
							translate([0, 0, -row_radius]) {
								children();
							}
						}
					}
				}
			}
		}
	}
}

function valid_key(col, row) =
	(col >= 0) &&
	(row >= 0) &&
	(col < ncols) &&
	(row < nrows) &&
	true;
	// !((row == lastrow) && ((col == 4) || (col == 5)));

// ;;;;;;;;;;;;;;;
// ;; Main Keys ;;
// ;;;;;;;;;;;;;;;

function default_cond(col, row) = true;

module main_keys() {
	union() {
		for (col = [0 : ncols], row = [0 : nrows]) {
			if (valid_key(col, row)) {
				key_place(col, row) {
					single_plate();
				}
			}
		}
	}
}

module main_caps() {
	union() {
		for (col = [0 : ncols], row = [0 : nrows]) {
			if (valid_key(col, row)) {
				key_place(col, row) {
					sa_cap();
				}
			}
		}
	}
}

// ;;;;;;;;;;;;;;;;
// ;; Thumb Keys ;;
// ;;;;;;;;;;;;;;;;

module thumb_place(row) {
	key_place(0, row) {
		translate([-mount_width / 2, 0, 0]) {
			rotate(-60, [0, 1, 0]) {
				translate([-mount_width / 2, 0, 0]) {
					children();
				}
			}
		}
	}
}

module thumb_keys() {
	union() {
		for (row = [0 : nrows - 1]) {
			thumb_place(row) {
				single_plate();
			}
		}
	}
}

module thumb_caps() {
	union() {
		for (row = [0 : nrows - 1]) {
			thumb_place(row) {
				sa_cap();
			}
		}
	}
}

// ;;;;;;;;;;;;;;;;;;;;;
// ;; Main Connectors ;;
// ;;;;;;;;;;;;;;;;;;;;;

module web_posts(idx) {
	half_mw = mount_width / 2;
	half_mh = mount_height / 2;
	post_size = 0.1;
	module web_post() {
		translate([0, 0, plate_thickness / 2]) {
			cube([post_size, post_size, plate_thickness], center=true);
		}
	}

	offsets = [
		[half_mw, half_mh],
		[-half_mw, half_mh],
		[half_mw, -half_mh],
		[-half_mw, -half_mh]
	];

	translate(offsets[idx]) web_post();
};

module web_post_tr() web_posts(0);
module web_post_tl() web_posts(1);
module web_post_br() web_posts(2);
module web_post_bl() web_posts(3);

module main_connectors() {
	union() {
		for (col = [0 : ncols], row = [0 : nrows]) {
			// Row connections
			if (valid_key(col + 1, row)) {
				hull() {
					key_place(col, row) web_post_tr();
					key_place(col + 1, row) web_post_tl();
					key_place(col, row) web_post_br();
					key_place(col + 1, row) web_post_bl();
				}
			}
			// Column connections
			if (valid_key(col, row + 1)) {
				hull() {
					key_place(col, row) web_post_tr();
					key_place(col, row) web_post_tl();
					key_place(col, row + 1) web_post_br();
					key_place(col, row + 1) web_post_bl();
				}
			}
			// Diagonal connections
			if (valid_key(col + 1, row + 1)) {
				hull() {
					key_place(col, row) web_post_tr();
					key_place(col + 1, row) web_post_tl();
					key_place(col, row + 1) web_post_br();
					key_place(col + 1, row + 1) web_post_bl();
				}
			}
		}
	}
}

// (def connectors
// 	(union
// 		; Special connection because of the missing row
// 		(if (>= ncols 4)
// 			(union
// 				(hull
// 					(key-place 3 (- lastrow 1) web-post-br)
// 					(key-place 4 (- lastrow 1) web-post-bl)
// 					(key-place 3 lastrow web-post-tr)
// 				)
// 				(hull
// 					(key-place 4 (- lastrow 1) web-post-bl)
// 					(key-place 3 lastrow web-post-tr)
// 					(key-place 3 lastrow web-post-br)
// 				)
// 				(hull
// 					(key-place 4 (- lastrow 1) web-post-bl)
// 					(key-place 4 (- lastrow 1) web-post-br)
// 					(key-place 3 lastrow web-post-br)
// 				)
// 			) ()
// 		)
// 	)
// )

// ;;;;;;;;;;;;;;;;;;;;;;
// ;; Thumb Connectors ;;
// ;;;;;;;;;;;;;;;;;;;;;;

module thumb_connectors() {
	union() {
		for (row = [0 : nrows - 2]) {
			// Between thumb buttons
			hull() {
				thumb_place(row) web_post_tr();
				thumb_place(row) web_post_tl();
				thumb_place(row + 1) web_post_br();
				thumb_place(row + 1) web_post_bl();
			}
			// Diagonal connections
			hull() {
				thumb_place(row) web_post_tr();
				key_place(0, row) web_post_tl();
				thumb_place(row + 1) web_post_br();
				key_place(0, row + 1) web_post_bl();
			}
		}
		for (row = [0 : nrows - 1]) {
			// Between thumb and main buttons
			hull() {
				thumb_place(row) web_post_tr();
				key_place(0, row) web_post_tl();
				thumb_place(row) web_post_br();
				key_place(0, row) web_post_bl();
			}
		}
	}
}

// ;;;;;;;;;;
// ;; Case ;;
// ;;;;;;;;;;

module bottom(height) {
	translate([0, 0, (height / 2) - 10]) {
		linear_extrude(height=height) {
			projection() {
				children();
			}
		}
	}
}

module bottom_hull() {
	hull() {
		children();
		bottom(0.001) children();
	}
}

function wall_locate1(dx, dy) = [dx * wall_thickness, dy * wall_thickness, -1];
function wall_locate2(dx, dy) = [dx * wall_xy_offset, dy * wall_xy_offset, wall_z_offset];
function wall_locate3(dx, dy) = [dx * (wall_xy_offset + wall_thickness), dy * (wall_xy_offset + wall_thickness), wall_z_offset];
// function wall_locate3(dx, dy) = wall_locate1(dx, dy) + wall_locate2(dx, dy) + [0, 0, 1];

module key_wall_brace(x1, y1, dx1, dy1, x2, y2, dx2, dy2) {
	union() {
		hull() {
			key_place(x1, y1) {
				children(0);
				translate(wall_locate1(dx1, dy1)) children(0);
				translate(wall_locate2(dx1, dy1)) children(0);
				translate(wall_locate3(dx1, dy1)) children(0);
			}
			key_place(x2, y2) {
				children(1);
				translate(wall_locate1(dx2, dy2)) children(1);
				translate(wall_locate2(dx2, dy2)) children(1);
				translate(wall_locate3(dx2, dy2)) children(1);
			}
		}
		bottom_hull() {
			key_place(x1, y1) {
				translate(wall_locate2(dx1, dy1)) children(0);
				translate(wall_locate3(dx1, dy1)) children(0);
			}
			key_place(x2, y2) {
				translate(wall_locate2(dx2, dy2)) children(1);
				translate(wall_locate3(dx2, dy2)) children(1);
			}
		}
	}
}

module half_key_wall_brace(row1, dx1, dy1, row2, dx2, dy2) {
	bottom_hull() {
		thumb_place(row1) {
			children(0);
			translate(wall_locate1(dx1, dy1)) children(0);
		}
		thumb_place(row2) {
			children(1);
			translate(wall_locate1(dx2, dy2)) children(1);
		}
	}
}

module half_corner(row, dy) {
	union() {
		hull() {
			thumb_place(row) children(1);
			key_place(0, row) {
				children(0);
				translate(wall_locate1(0, dy)) children(0);
			}
		}
		hull() {
			thumb_place(row) {
				children(0);
				children(1);
			}
			key_place(0, row) {
				translate(wall_locate1(0, dy)) children(0);
				translate(wall_locate2(0, dy)) children(0);
				translate(wall_locate3(0, dy)) children(0);
			}
		}
		hull() {
			thumb_place(row) {
				children(0);
				translate(wall_locate1(-1, 0)) children(0);
			}
			key_place(0, row) {
				translate(wall_locate2(0, dy)) children(0);
				translate(wall_locate3(0, dy)) children(0);
			}
		}
		bottom_hull() {
			thumb_place(row) {
				children(0);
				translate(wall_locate1(-1, 0)) children(0);
			}
			key_place(0, row) {
				translate(wall_locate2(0, dy)) children(0);
				translate(wall_locate3(0, dy)) children(0);
			}
		}
	}
}

// (defn half-corner [row dy post alt-post]
// 	(union
// 		(hull
// 			(place-thumb row alt-post)
// 			(key-place 0 row post)
// 			(key-place 0 row (translate (wall-locate1 0 dy) post))
// 		)
// 		(hull
// 			(place-thumb row alt-post)
// 			(place-thumb row post)
// 			(key-place 0 row (translate (wall-locate1 0 dy) post))
// 			(key-place 0 row (translate (wall-locate2 0 dy) post))
// 			(key-place 0 row (translate (wall-locate3 0 dy) post))
// 		)
// 		(hull
// 			(place-thumb row post)
// 			(place-thumb row (translate (wall-locate1 -1 0) post))
// 			(key-place 0 row (translate (wall-locate2 0 dy) post))
// 			(key-place 0 row (translate (wall-locate3 0 dy) post))
// 		)
// 		(bottom-hull
// 			(place-thumb row post)
// 			(place-thumb row (translate (wall-locate1 -1 0) post))
// 			(key-place 0 row (translate (wall-locate2 0 dy) post))
// 			(key-place 0 row (translate (wall-locate3 0 dy) post))
// 		)
// 	)
// )

module case_walls() {
	union() {
		// Back Wall
		for (x = [0 : ncols - 1]) {
			key_wall_brace(x, 0, 0, -1, x, 0, 0, -1) {
				web_post_bl();
				web_post_br();
			}
		}
		for (x = [1 : ncols - 1]) {
			key_wall_brace(x, 0, 0, -1, x - 1, 0, 0, -1) {
				web_post_bl();
				web_post_br();
			}
		}
		// Front Wall
		for (x = [0 : ncols - 1]) {
			key_wall_brace(x, lastrow, 0, 1, x, lastrow, 0, 1) {
				web_post_tl();
				web_post_tr();
			}
		}
		for (x = [1 : ncols - 1]) {
			key_wall_brace(x, lastrow, 0, 1, x - 1, lastrow, 0, 1) {
				web_post_tl();
				web_post_tr();
			}
		}
		// Right Wall
		for (y = [0 : nrows - 1]) {
			key_wall_brace(lastcol, y, 1, 0, lastcol, y, 1, 0) {
				web_post_br();
				web_post_tr();
			}
		}
		for (y = [1 : nrows - 1]) {
			key_wall_brace(lastcol, y, 1, 0, lastcol, y - 1, 1, 0) {
				web_post_br();
				web_post_tr();
			}
		}
		// Left Wall
		for (y = [0 : nrows - 1]) {
			half_key_wall_brace(y, -1, 0, y, -1, 0) {
				web_post_bl();
				web_post_tl();
			}
		}
		for (y = [1 : nrows - 1]) {
			half_key_wall_brace(y, -1, 0, y - 1, -1, 0) {
				web_post_bl();
				web_post_tl();
			}
		}
		// Corners
		half_corner(0, -1) {
			web_post_bl();
			web_post_br();
		}
		half_corner(lastrow, 1) {
			web_post_tl();
			web_post_tr();
		}
		key_wall_brace(lastcol, 0, 0, -1, lastcol, 0, 1, 0) {
			web_post_br();
			web_post_br();
		}
		key_wall_brace(lastcol, lastrow, 0, 1, lastcol, lastrow, 1, 0) {
			web_post_tr();
			web_post_tr();
		}
	}
}

// (def case-walls
// 	(union
// 		; Front wall
// 		(for [x (range 0 (min 4 ncols))] (key-wall-brace
// 			x lastrow 0 -1 web-post-bl
// 			x lastrow 0 -1 web-post-br
// 		))
// 		(for [x (range 1 (min 4 ncols))] (key-wall-brace
// 			x lastrow 0 -1 web-post-bl
// 			(dec x) lastrow 0 -1 web-post-br
// 		))
// 		(key-wall-brace
// 			5 cornerrow 0 -1 web-post-bl
// 			5 cornerrow 0 -1 web-post-br
// 		)
// 		(key-wall-brace
// 			5 cornerrow 0 -1 web-post-bl
// 			4 cornerrow 0 -1 web-post-br
// 		)
// 		(key-wall-brace
// 			3 lastrow 0 -1 web-post-br
// 			4 cornerrow 0 -1 web-post-br
// 		)
// 	)
// )

module case() {
	difference() {
		union() {
			// main_keys();
			// main_connectors();
			// thumb_keys();
			// thumb_connectors();
			// case_walls();

			color([0.2, 0.2, 0.8, 1]) main_keys();
			// color([0.8, 0.8, 0.2, 1]) main_caps();
			color([0.2, 0.8, 0.8, 1]) main_connectors();
			color([0.2, 0.8, 0.2, 1]) thumb_keys();
			// color([0.8, 0.8, 0.8, 1]) thumb_caps();
			color([0.8, 0.2, 0.2, 1]) thumb_connectors();
			color([0.8, 0.2, 0.8, 1]) case_walls();
		}
		translate([0, 0, -20]) {
			cube([350, 350, 40], center=true);
		}
	}
}

case();