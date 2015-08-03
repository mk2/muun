CARGO=cargo

.PHONY: test

test:
	$(CARGO) test -- --nocapture
