# Build Stage
FROM rustlang/rust:nightly as builder


#add source code to the build state
ADD . /fancy-regex
WORKDIR /fancy-regex

RUN cargo install cargo-fuzz

## TODO: ADD YOUR BUILD INSTRUCTIONS HERE.
WORKDIR /fancy-regex/fuzz/fuzz_targets
RUN cargo +nightly fuzz build fuzz_parser

#Output is in /fancy-regex/fuzz/target/x86_64-unknown-linux-gnu/release

# Package Stage
FROM --platform=linux/amd64 ubuntu:20.04


COPY --from=builder /fancy-regex/fuzz/target/x86_64-unknown-linux-gnu/release /
