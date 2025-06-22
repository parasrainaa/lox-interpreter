#!/bin/sh
#
# Use this script to run your program LOCALLY.

set -e

# Build the project in release mode
cargo build --release

# Execute the compiled binary
exec ./target/release/lox-interpreter "$@"
