#!/bin/bash

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
				echo "Test failed: $base"
				echo "See file $out"
			else
				if ! delta "$dirname/$base.out" "$expected"; then
					echo "Test ${base}: output does not match expected output"
					echo "See file: $out"
				else
					echo "Test passed: $base"
					rm "$out"
				fi
			fi
		else
			echo "Missing output comparison file: $expected"
			echo "Skipping test: $base"
		fi

	done
}

run_test tokenize "$TOKENIZE_TEST_FILES"
run_test parse "$PARSE_TEST_FILES"
