use crate::circuit::{Circuit, Gate};

use std::collections::HashMap;

pub fn print(c: &Circuit) {
    println!("{} {}", c.nwires() - c.ninputs(), c.nwires());
    println!("{} {} {}\n", c.ninputs(), 0, c.noutputs());

    let mut map = HashMap::new();

    // The inputs are implicitly the first refs in Bristol format
    for (i, inp) in c.inputs.iter().enumerate() {
        map.insert(*inp, i);
    }

    // The outputs are the last refs in Bristol format
    for (i, out) in c.outputs.iter().enumerate() {
        let j = c.nwires() - c.noutputs() + i;
        map.insert(*out, j);
    }

    let mut mapped_i = c.ninputs(); // Initial gate id starts after the inputs

    for (i, g) in c.gates.iter().enumerate() {
        match g {
            Gate::And(x, y) => {
                let mapped_x = *map.get(x).unwrap();
                let mapped_y = *map.get(y).unwrap();
                map.insert(i, mapped_i);
                println!("2 1 {} {} {} AND", mapped_x, mapped_y, mapped_i);
                mapped_i += 1;
            }
            Gate::Not(x) => {
                let mapped_x = *map.get(x).unwrap();
                map.insert(i, mapped_i);
                println!("1 1 {} {} INV", mapped_x, mapped_i);
                mapped_i += 1;
            }
            Gate::Input => (),
        }
    }
}
