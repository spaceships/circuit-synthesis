
pub enum Gate {
    And(usize, usize),
    Not(usize),
}

pub struct Circuit {
    gates: Vec<Gate>,
    inputs: Vec<usize>,
    outputs: Vec<usize>,
}
