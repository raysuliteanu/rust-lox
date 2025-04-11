#!/bin/bash

CARGO_RUN_CMD="cargo -q run -- tokenize"

TOKENIZE_TEST_FILES=$(find "$(pwd)" -type f -name "tokenize_test*.lox" | sort)
PARSE_TEST_FILES=$(find "$(pwd)" -type f -name "parse_test*.lox" | sort)
ALL_TEST_FILES="$TOKENIZE_TEST_FILES $PARSE_TEST_FILES"

for file in $ALL_TEST_FILES; do
	dirname=$(dirname "$file")
	base=$(basename "$file" ".lox")
	echo "Running test: $base"
	expected="$dirname/${base}_expected.out"
	if [ -e "$expected" ]; then
		out="$dirname/$base.out"
		if ! $CARGO_RUN_CMD "$file" >"$out" 2>&1; then
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
