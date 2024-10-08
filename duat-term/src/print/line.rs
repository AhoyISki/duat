use crate::Brush::{self, *};

pub fn crossing(
    right: Option<Brush>,
    up: Option<Brush>,
    left: Option<Brush>,
    down: Option<Brush>,
    cut_ends: bool,
) -> char {
    match (right, up, left, down, cut_ends) {
        (Some(right), None, Some(left), None, _) => horizontal(right, left),
        (Some(line), None, None, None, false) | (None, None, Some(line), None, false) => {
            horizontal(line, line)
        }
        (None, Some(up), None, Some(down), _) => vertical(up, down),
        (None, Some(line), None, None, false) | (None, None, None, Some(line), false) => {
            vertical(line, line)
        }
        (Some(line), None, None, None, true) => right_end(line),
        (None, Some(line), None, None, true) => up_end(line),
        (None, None, Some(line), None, true) => left_end(line),
        (None, None, None, Some(line), true) => down_end(line),
        (None, None, Some(left), Some(down), _) => left_down(left, down),
        (Some(right), None, None, Some(down), _) => right_down(right, down),
        (Some(right), Some(up), None, None, _) => right_up(right, up),
        (None, Some(up), Some(left), None, _) => left_up(left, up),
        (Some(right), Some(up), None, Some(down), _) => right_vertical(right, up, down),
        (Some(right), Some(up), Some(left), None, _) => up_horizontal(up, left, right),
        (None, Some(up), Some(left), Some(down), _) => left_vertical(left, up, down),
        (Some(right), None, Some(left), Some(down), _) => down_horizontal(down, left, right),
        (Some(right), Some(up), Some(left), Some(down), _) => cross(right, up, left, down),
        (None, None, None, None, _) => unreachable!(),
    }
}

pub fn horizontal(right: Brush, left: Brush) -> char {
    match (left, right) {
        (_, Ascii) | (Ascii, _) => '-',

        (_, Custom(char)) | (Custom(char), _) => char,

        (Regular, Regular | Dashed | Double | Rounded)
        | (Dashed, Regular | Double)
        | (Double, Regular | Dashed)
        | (Rounded, Regular | Rounded) => '─',

        (Regular | Dashed, Thick | ThickDashed) => '╼',

        (Thick | ThickDashed, Regular | Dashed) => '╾',

        (Thick, _) | (_, Thick) => '━',

        (Dashed, _) | (_, Dashed) => '╌',

        (ThickDashed, _) | (_, ThickDashed) => '╍',

        (Double, _) | (_, Double) => '═',
    }
}

pub fn vertical(up: Brush, down: Brush) -> char {
    match (up, down) {
        (Ascii, _) | (_, Ascii) => '|',

        (Custom(char), _) | (_, Custom(char)) => char,

        (Regular, Regular | Dashed | Double | Rounded)
        | (Dashed, Regular | Double)
        | (Double, Regular | Dashed)
        | (Rounded, Regular | Rounded) => '│',

        (Regular | Dashed, Thick | ThickDashed) => '╽',

        (Thick | ThickDashed, Regular | Dashed) => '╿',

        (Thick, _) | (_, Thick) => '┃',

        (Dashed, _) | (_, Dashed) => '╎',

        (ThickDashed, _) | (_, ThickDashed) => '╏',

        (Double, _) | (_, Double) => '║',
    }
}

fn right_end(line: Brush) -> char {
    match line {
        Regular | Dashed | Rounded => '╶',
        Thick | ThickDashed | Double => '╺',
        Ascii => '-',
        Custom(char) => char,
    }
}

fn up_end(line: Brush) -> char {
    match line {
        Regular | Dashed | Rounded => '╵',
        Thick | ThickDashed | Double => '╹',
        Ascii => '|',
        Custom(char) => char,
    }
}

fn left_end(line: Brush) -> char {
    match line {
        Regular | Dashed | Rounded => '╴',
        Thick | ThickDashed | Double => '╸',
        Ascii => '-',
        Custom(char) => char,
    }
}

fn down_end(line: Brush) -> char {
    match line {
        Regular | Dashed | Rounded => '╷',
        Thick | ThickDashed | Double => '╻',
        Ascii => '|',
        Custom(char) => char,
    }
}

fn left_down(left: Brush, down: Brush) -> char {
    match (left, down) {
        (Ascii, _) | (_, Ascii) => '+',

        (Custom(char), _) | (_, Custom(char)) => char,

        (Rounded, _) | (_, Rounded) => '╮',

        (Regular | Dashed, Dashed | Regular) => '┐',

        (Regular | Dashed, Thick | ThickDashed) => '┒',

        (Thick | ThickDashed, Regular | Dashed) => '┑',

        (Thick | ThickDashed, _) | (_, Thick | ThickDashed) => '┓',

        (Regular | Dashed, Double) => '╖',

        (Double, Regular | Dashed) => '╕',

        (Double, Double) => '╗',
    }
}

fn right_down(right: Brush, down: Brush) -> char {
    match (right, down) {
        (Ascii, _) | (_, Ascii) => '+',

        (Custom(char), _) | (_, Custom(char)) => char,

        (Rounded, _) | (_, Rounded) => '╭',

        (Regular | Dashed, Dashed | Regular) => '┌',

        (Regular | Dashed, Thick | ThickDashed) => '┎',

        (Thick | ThickDashed, Regular | Dashed) => '┍',

        (Thick | ThickDashed, _) | (_, Thick | ThickDashed) => '┏',

        (Regular | Dashed, Double) => '╓',

        (Double, Regular | Dashed) => '╒',

        (Double, Double) => '╔',
    }
}

fn right_up(right: Brush, up: Brush) -> char {
    match (right, up) {
        (Ascii, _) | (_, Ascii) => '+',

        (Custom(char), _) | (_, Custom(char)) => char,

        (Rounded, _) | (_, Rounded) => '╰',

        (Regular | Dashed, Dashed | Regular) => '└',

        (Regular | Dashed, Thick | ThickDashed) => '┖',

        (Thick | ThickDashed, Regular | Dashed) => '┕',

        (Thick | ThickDashed, _) | (_, Thick | ThickDashed) => '┗',

        (Regular | Dashed, Double) => '╙',

        (Double, Regular | Dashed) => '╘',

        (Double, Double) => '╚',
    }
}

fn left_up(left: Brush, up: Brush) -> char {
    match (left, up) {
        (Ascii, _) | (_, Ascii) => '+',

        (Custom(char), _) | (_, Custom(char)) => char,

        (Rounded, _) | (_, Rounded) => '╯',

        (Regular | Dashed, Dashed | Regular) => '┘',

        (Regular | Dashed, Thick | ThickDashed) => '┚',

        (Thick | ThickDashed, Regular | Dashed) => '┙',

        (Thick | ThickDashed, _) | (_, Thick | ThickDashed) => '┛',

        (Regular | Dashed, Double) => '╜',

        (Double, Regular | Dashed) => '╛',

        (Double, Double) => '╝',
    }
}

macro_rules! regular {
    () => {
        Regular | Dashed | Rounded
    };
}

macro_rules! thick {
    () => {
        Thick | ThickDashed
    };
}

fn right_vertical(right: Brush, up: Brush, down: Brush) -> char {
    match (right, up, down) {
        (Ascii, ..) | (_, Ascii, _) | (_, _, Ascii) => '+',

        (Custom(char), ..) | (_, Custom(char), _) | (_, _, Custom(char)) => char,

        (regular!(), regular!(), regular!()) => '├',

        (thick!(), regular!() | Double, regular!() | Double) => '┝',

        (regular!() | Double, thick!(), regular!() | Double) => '┞',

        (regular!() | Double, regular!() | Double, thick!()) => '┟',

        (regular!() | Double, thick!(), thick!()) => '┠',

        (thick!(), regular!() | Double, thick!()) => '┢',

        (thick!(), thick!(), regular!() | Double) => '┡',

        (thick!(), thick!(), thick!()) => '┣',

        (Double, Double, Double) => '╠',

        (Double, ..) => '╞',

        (_, Double, _) | (_, _, Double) => '╟',
    }
}

fn up_horizontal(up: Brush, left: Brush, right: Brush) -> char {
    match (up, left, right) {
        (Ascii, ..) | (_, Ascii, _) | (_, _, Ascii) => '+',

        (Custom(char), ..) | (_, Custom(char), _) | (_, _, Custom(char)) => char,

        (regular!(), regular!(), regular!()) => '┴',

        (thick!(), regular!() | Double, regular!() | Double) => '┸',

        (regular!() | Double, thick!(), regular!() | Double) => '┵',

        (regular!() | Double, regular!() | Double, thick!()) => '┶',

        (regular!() | Double, thick!(), thick!()) => '┷',

        (thick!(), regular!() | Double, thick!()) => '┺',

        (thick!(), thick!(), regular!() | Double) => '┹',

        (thick!(), thick!(), thick!()) => '┻',

        (Double, Double, Double) => '╩',

        (Double, ..) => '╨',

        (_, Double, _) | (_, _, Double) => '╧',
    }
}

fn left_vertical(left: Brush, up: Brush, down: Brush) -> char {
    match (left, up, down) {
        (Ascii, ..) | (_, Ascii, _) | (_, _, Ascii) => '+',

        (Custom(char), ..) | (_, Custom(char), _) | (_, _, Custom(char)) => char,

        (regular!(), regular!(), regular!()) => '┤',

        (thick!(), regular!() | Double, regular!() | Double) => '┥',

        (regular!() | Double, thick!(), regular!() | Double) => '┦',

        (regular!() | Double, regular!() | Double, thick!()) => '┧',

        (regular!() | Double, thick!(), thick!()) => '┨',

        (thick!(), regular!() | Double, thick!()) => '┪',

        (thick!(), thick!(), regular!() | Double) => '┪',

        (thick!(), thick!(), thick!()) => '┫',

        (Double, Double, Double) => '╣',

        (Double, ..) => '╡',

        (_, Double, _) | (_, _, Double) => '╢',
    }
}

fn down_horizontal(down: Brush, left: Brush, right: Brush) -> char {
    match (down, left, right) {
        (Ascii, ..) | (_, Ascii, _) | (_, _, Ascii) => '+',

        (Custom(char), ..) | (_, Custom(char), _) | (_, _, Custom(char)) => char,

        (regular!(), regular!(), regular!()) => '┬',

        (thick!(), regular!() | Double, regular!() | Double) => '┰',

        (regular!() | Double, thick!(), regular!() | Double) => '┭',

        (regular!() | Double, regular!() | Double, thick!()) => '┮',

        (regular!() | Double, thick!(), thick!()) => '┯',

        (thick!(), regular!() | Double, thick!()) => '┲',

        (thick!(), thick!(), regular!() | Double) => '┱',

        (thick!(), thick!(), thick!()) => '┳',

        (Double, Double, Double) => '╦',

        (Double, ..) => '╥',

        (_, Double, _) | (_, _, Double) => '╤',
    }
}

fn cross(right: Brush, up: Brush, left: Brush, down: Brush) -> char {
    match (right, up, left, down) {
        (Ascii, ..) | (_, Ascii, ..) | (.., Ascii, _) | (.., Ascii) => '+',

        (Custom(char), ..) | (_, Custom(char), ..) | (.., Custom(char), _) | (.., Custom(char)) => {
            char
        }

        (regular!(), regular!(), regular!(), regular!()) => '┼',

        (thick!(), regular!() | Double, regular!() | Double, regular!() | Double) => '┾',

        (regular!() | Double, thick!(), regular!() | Double, regular!() | Double) => '╀',

        (regular!() | Double, regular!() | Double, thick!(), regular!() | Double) => '┽',

        (regular!() | Double, regular!() | Double, regular!() | Double, thick!()) => '╁',

        (thick!(), thick!(), regular!() | Double, regular!() | Double) => '╄',

        (thick!(), regular!() | Double, thick!(), regular!() | Double) => '┿',

        (thick!(), regular!() | Double, regular!() | Double, thick!()) => '╆',

        (regular!() | Double, thick!(), thick!(), regular!() | Double) => '╃',

        (regular!() | Double, thick!(), regular!() | Double, thick!()) => '╅',

        (regular!() | Double, regular!() | Double, thick!(), thick!()) => '╂',

        (regular!() | Double, thick!(), thick!(), thick!()) => '╉',

        (thick!(), regular!() | Double, thick!(), thick!()) => '╈',

        (thick!(), thick!(), regular!() | Double, thick!()) => '╊',

        (thick!(), thick!(), thick!(), regular!() | Double) => '╇',

        (thick!(), thick!(), thick!(), thick!()) => '╋',

        (Double, Double, ..)
        | (Double, _, _, Double)
        | (_, Double, Double, _)
        | (_, _, Double, Double) => '╬',

        (Double, ..) | (_, _, Double, _) => '╪',

        (_, Double, ..) | (_, _, _, Double) => '╫',
    }
}
