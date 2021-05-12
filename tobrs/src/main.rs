mod circuit;
mod format;

fn main() {
    let input_filename = std::env::args().nth(1)
        .expect("Input filename required.");
    let c = format::bench::read(&input_filename);
    println!("done parsing");
}
