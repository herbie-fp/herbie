use std::io::{BufRead, BufReader, Write};

use egg_smol::EGraph;

fn main() {
    let mut cmd_buffer = String::new();
    let input = std::io::stdin();
    let mut egraph = EGraph::default();

    for line in BufReader::new(input).lines() {
        let line_str = line.unwrap();
        cmd_buffer.push_str(&line_str);
        cmd_buffer.push('\n');
    }

    match egraph.parse_and_run_program(&cmd_buffer) {
        Ok(msgs) => {
            for msg in msgs {
                println!("{msg}");
                // flush output
                std::io::stdout().flush().unwrap();
            }
        }
        Err(err) => eprintln!("{err}"),
    }
}
