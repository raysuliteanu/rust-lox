#!/bin/bash

GREEN=$(echo -e '\033[0;32m')
RED=$(echo -e '\033[0;31m')
RESET=$(echo -e '\033[0m')

CARGO_RUN="cargo -q run -- "

TOKENIZE_TEST_FILES=$(find "$(pwd)" -type f -name "tokenize_test*.lox" | sort)
PARSE_TEST_FILES=$(find "$(pwd)" -type f -name "parse_test*.lox" | sort)

run_test() {
	local cmd=$1
	local files=$2

	echo "=== Running ${cmd} tests:"

	for file in $files; do
		dirname=$(dirname "$file")
		base=$(basename "$file" ".lox")
		echo "Running test: $base"
		expected="$dirname/${base}_expected.out"
		if [ -e "$expected" ]; then
			out="$dirname/$base.out"
			if ! $CARGO_RUN "$cmd" "$file" >"$out" 2>&1; then
				echo "${RED}Test failed:${RESET} $base"
				echo "See file $out"
			else
				if ! delta "$dirname/$base.out" "$expected"; then
					echo "${RED}Test ${base}:${RESET} output does not match expected output"
					echo "See file: $out"
				else
					echo "${GREEN}Test passed:${RESET} $base"
					rm "$out"
				fi
			fi
		else
			echo "${RED}Missing output comparison file:${RESET} $expected"
			echo "Skipping test: $base"
		fi

	done
}

run_test tokenize "$TOKENIZE_TEST_FILES"
run_test parse "$PARSE_TEST_FILES"
