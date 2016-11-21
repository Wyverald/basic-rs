
#[derive(Clone, Copy, Debug)]
pub struct Position {
    pub line: u32,
    pub col: u32,
}

impl Position {
    pub fn new() -> Position {
        Position {
            line: 0,
            col: 0,
        }
    }

    pub fn next_col(&mut self) {
        self.col += 1;
    }

    pub fn next_line(&mut self) {
        self.line += 1;
        self.col = 0;
    }
}
