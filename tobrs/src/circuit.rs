
pub enum Gate {
    Input,
    And(usize, usize),
    Not(usize),
}

pub struct Circuit {
    gates: Vec<Gate>,
    inputs: Vec<usize>,
    outputs: Vec<usize>,
}

impl Circuit {
    pub fn new() -> Self {
        Self { gates: Vec::new(), inputs: Vec::new(), outputs: Vec::new() }
    }

    pub fn input(&mut self) -> usize {
        let r = self.gates.len();
        self.gates.push(Gate::Input);
        self.inputs.push(r);
        r
    }

    pub fn output(&mut self, x: usize) {
        self.outputs.push(x);
    }

    pub fn not(&mut self, x: usize) -> usize {
        let r = self.gates.len();
        self.gates.push(Gate::Not(x));
        r
    }

    pub fn and(&mut self, x: usize, y: usize) -> usize {
        let r = self.gates.len();
        self.gates.push(Gate::And(x, y));
        r
    }
}
