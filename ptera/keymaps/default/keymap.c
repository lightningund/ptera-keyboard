// Copyright 2023 QMK
// SPDX-License-Identifier: GPL-2.0-or-later

#include QMK_KEYBOARD_H

// Normal
// "rows": ["B6", "B2", "B3", "B1", "F7"],
// "cols": ["D0", "D4", "C6", "D7", "E6", "B4", "B5"]

// Reversed
// "rows": ["F7", "B1", "B3", "B2", "B6"],
// "cols": ["B5", "B4", "E6", "D7", "C6", "D4", "D0"]

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
	// /*
	// | ESC  | 1 | 2 | 3 | 4 | 5 |   |		|   | 6 | 7 | 8 | 9 | 0 | BKSPC |
	// | TAB  | Q | W | E | R | T |   |		|   | Y | U | I | O | P | ENTER |
	// | SHFT | A | S | D | F | G |   |		|   | H | J | K | L | ; |   '   |
	// | CTRL | Z | X | C | V | B |   |		|   | N | M | , | . | / |   \   |
	// |      |   | - | = | [ | ] |   |		|   | ^ | v | < | > |   |       |
	//  */
	// [0] = LAYOUT(
	// 	KC_ESC, KC_1, KC_2, KC_3, KC_4, KC_5, KC_NO, KC_NO, KC_6, KC_7, KC_8, KC_9, KC_0, KC_BSPC,
	// 	KC_TAB, KC_Q, KC_W, KC_E, KC_R, KC_T, KC_NO, KC_NO, KC_Y, KC_U, KC_I, KC_O, KC_P, KC_ENT,
	// 	KC_LSFT, KC_A, KC_S, KC_D, KC_F, KC_G, KC_NO, KC_NO, KC_H, KC_J, KC_K, KC_L, KC_SCLN, KC_QUOT,
	// 	KC_LCTL, KC_Z, KC_X, KC_C, KC_V, KC_B, KC_NO, KC_NO, KC_N, KC_M, KC_COMM, KC_DOT, KC_SLSH, KC_BSLS,
	// 	KC_NO, KC_NO, KC_MINS, KC_EQL, KC_LBRC, KC_RBRC, KC_NO, KC_NO, KC_LEFT, KC_DOWN, KC_UP, KC_RGHT, KC_NO, KC_NO
	// )
	// Missing: Space, caps lock, grave, F keys, windows, alt, r ctrl, insert, delete, r shift, prnt scr

	// | ESC  | 1   | 2   | 3   | 4   | 5   |      |		| INS  | 6   | 7   | 8   | 9   | 0   | DEL |
	// | TAB  | Q   | W   | E   | R   | T   | CAPS |		| END  | Y   | U   | I   | O   | P   | '   |
	// | `    | A   | S   | D   | F   | G   | ALT  |		| HOME | H   | J   | K   | L   | ;   | /   |
	// | SHFT | Z   | X   | C   | V   | B   | GAME |		| \    | N   | M   | ,   | .   | ^   | ENT |
	// | CTRL | WIN | FN  | [   | ]   | SPC | SPC  |		| SPC  | BCK | -   | =   | <   | v   | >   |

	[0] = LAYOUT(
		KC_ESC,  KC_1,    KC_2,  KC_3,    KC_4,    KC_5,   KC_NO,   		KC_INS,  KC_6,   KC_7,    KC_8,    KC_9,    KC_0,    KC_DEL,
		KC_TAB,  KC_Q,    KC_W,  KC_E,    KC_R,    KC_T,   KC_CAPS, 		KC_END,  KC_Y,   KC_U,    KC_I,    KC_O,    KC_P,    KC_QUOT,
		KC_GRV,  KC_A,    KC_S,  KC_D,    KC_F,    KC_G,   KC_LALT, 		KC_HOME, KC_H,   KC_J,    KC_K,    KC_L,    KC_SCLN, KC_SLSH,
		KC_LSFT, KC_Z,    KC_X,  KC_C,    KC_V,    KC_B,   TG(2),   		KC_BSLS, KC_N,   KC_M,    KC_COMM, KC_DOT,  KC_UP,   KC_ENT,
		KC_LCTL, KC_LGUI, MO(1), KC_LBRC, KC_RBRC, KC_SPC, KC_SPC,  		KC_SPC, KC_BSPC, KC_MINS, KC_EQL,  KC_LEFT, KC_DOWN, KC_RGHT
	),
	// Function layout
	[1] = LAYOUT(
		KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   KC_F6,   KC_TRNS,		KC_TRNS, KC_F7,   KC_F8,   KC_F9,   KC_F10,  KC_F11,  KC_F12,
		KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,		KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
		KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,		KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
		KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,		KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
		KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,		KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS
	),
	// Gaming layout
	[2] = LAYOUT(
		KC_ESC, KC_1,    KC_2,    KC_3,  KC_4,    KC_5,    KC_NO,   	KC_INS,  KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_DEL,
		KC_T,   KC_TAB,  KC_Q,    KC_W,  KC_E,    KC_R,    KC_CAPS,		KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
		KC_G,   KC_GRV,  KC_A,    KC_S,  KC_D,    KC_F,    KC_LALT,		KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
		KC_B,   KC_LSFT, KC_Z,    KC_X,  KC_C,    KC_V,    TG(2),		KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
		KC_SPC, KC_LCTL, KC_LGUI, MO(1), KC_LBRC, KC_RBRC, KC_SPC,		KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS
	)
};