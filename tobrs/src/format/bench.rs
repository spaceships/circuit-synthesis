use crate::circuit::Circuit;

use std::fs::File;
use std::io::{BufReader, BufRead};
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;

type RefMap = HashMap<String, usize>;

pub fn read(filename: &str) -> Circuit {
    let f = BufReader::new(File::open(filename).unwrap());

    let mut circuit = Circuit::new();
    let mut refmap = RefMap::new();
    let mut outputs = Vec::new();

    for line in f.lines().map(Result::unwrap) {
        if !(
            comment(&line) ||
            input(&line, &mut circuit, &mut refmap) ||
            output(&line, &mut outputs) ||
            not_gate(&line, &mut circuit, &mut refmap) ||
            and_gate(&line, &mut circuit, &mut refmap)
        ) {
            panic!("error parsing line: \"{}\"", line);
        }
    }

    for wire in &outputs {
        circuit.output(*refmap.get(wire).unwrap());
    }

    circuit
}

fn comment(line: &str) -> bool {
    lazy_static! {
        static ref COMMENT: Regex = Regex::new(r"^#.*").unwrap();
    }
    COMMENT.is_match(line)
}

fn input(line: &str, circuit: &mut Circuit, refmap: &mut RefMap) -> bool {
    lazy_static! {
        static ref INPUT: Regex = Regex::new(r"INPUT\((\w+)\)").unwrap();
    }
    match INPUT.captures(line) {
        Some(caps) => {
            let zwire = caps.get(1).unwrap();
            let z = circuit.input();
            refmap.insert(zwire.as_str().to_string(), z);
            true
        }
        None => false,
    }
}

fn output(line: &str, outputs: &mut Vec<String>) -> bool {
    lazy_static! {
        static ref OUTPUT: Regex = Regex::new(r"OUTPUT\((\w+)\)").unwrap();
    }
    match OUTPUT.captures(line) {
        Some(caps) => {
            let wire = caps.get(1).unwrap();
            outputs.push(wire.as_str().to_string());
            true
        }
        None => false,
    }
}

fn not_gate(line: &str, circuit: &mut Circuit, refmap: &mut RefMap) -> bool {
    lazy_static! {
        static ref NOT: Regex = Regex::new(r"(\w+)\s+=\s+NOT\((\w+)\)").unwrap();
    }
    match NOT.captures(line) {
        Some(caps) => {
            let zwire = caps.get(1).unwrap();
            let xwire = caps.get(2).unwrap();
            let x = refmap.get(xwire.as_str()).unwrap();
            let z = circuit.not(*x);
            refmap.insert(zwire.as_str().to_string(), z);
            true
        }
        None => false,
    }
}

fn and_gate(line: &str, circuit: &mut Circuit, refmap: &mut RefMap) -> bool {
    lazy_static! {
        static ref AND: Regex = Regex::new(r"(\w+)\s+=\s+AND\((\w+),\s+(\w+)\)").unwrap();
    }
    match AND.captures(line) {
        Some(caps) => {
            let zwire = caps.get(1).unwrap();
            let xwire = caps.get(2).unwrap();
            let ywire = caps.get(3).unwrap();
            let x = refmap.get(xwire.as_str()).unwrap();
            let y = refmap.get(ywire.as_str()).unwrap();
            let z = circuit.and(*x, *y);
            refmap.insert(zwire.as_str().to_string(), z);
            true
        }
        None => false,
    }
}
