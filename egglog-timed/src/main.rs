//! Thin REPL wrapper around `egglog_experimental::new_experimental_egraph()`
//! that emits a `(done <nanos>)` marker after each command, where `<nanos>`
//! is the wall-clock time `parse_and_run_program` spent on that command.
//! The Racket side (src/core/egglog-subprocess.rkt) parses the nanoseconds
//! to attribute engine time per send.
//!
//! Protocol: Herbie sends one s-expression per line (via Racket's writeln),
//! so we treat every non-empty line as a full program. Result messages come
//! out before the `(done <nanos>)` terminator; `(error <nanos>)` on failure.

use egglog_experimental::new_experimental_egraph;
use std::io::{self, BufRead, BufReader, Write};
use std::time::Instant;

fn main() {
    let mut egraph = new_experimental_egraph();
    let mut out = io::stdout().lock();

    for line in BufReader::new(io::stdin().lock()).lines() {
        let line = line.unwrap();
        if line.trim().is_empty() {
            continue;
        }

        let start = Instant::now();
        let result = egraph.parse_and_run_program(None, &line);
        let nanos = start.elapsed().as_nanos();

        match result {
            Ok(msgs) => {
                for msg in msgs {
                    write!(out, "{msg}").unwrap();
                }
                writeln!(out, "(done {nanos})").unwrap();
            }
            Err(e) => {
                eprintln!("egglog-timed error: {e}");
                writeln!(out, "(error {nanos})").unwrap();
            }
        }
        out.flush().unwrap();
    }
}
