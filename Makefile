

make:
	stack build

run:
	stack build --exec=ruby-in-ruby -- test.rb

clean:
	stack clean

test:
	stack test

.PHONY: test run clean
