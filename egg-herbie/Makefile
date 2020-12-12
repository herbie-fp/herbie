.PHONY:
test: test-math
	cargo fmt -- --check


.PHONY: test-math
test-math:
	cargo clippy --tests
	cargo test
