#!/bin/bash

# TODO: sort tests properly by test number i.e. numerically
# not lexicographically

GREEN=$(echo -e '\033[0;32m')
RED=$(echo -e '\033[0;31m')
RESET=$(echo -e '\033[0m')

CARGO_RUN="cargo -q run -- "

TOKENIZE_TEST_FILES=$(find "$(pwd)" -type f -name "tokenize_test*.lox" | sort)
PARSE_TEST_FILES=$(find "$(pwd)" -type f -name "parse_test*.lox" | sort)
EVALUATE_TEST_FILES=$(find "$(pwd)" -type f -name "evaluate_test*.lox" | sort)

RESULT=0

usage() {
	echo "run_tests.sh [tokenize|parse|evaluate] [test_file]"
	echo "  tokenize: Run all tokenize tests"
	echo "  parse: Run all parse tests"
	echo "  evaluate: Run all evaluate tests"
	echo "  test_file: Run a specific test file"
	echo "  If no arguments are provided, all tests will be run."
}

run_test() {
	local cmd=$1
	local file=$2

	dirname=$(dirname "$file")
	base=$(basename "$file" ".lox")
	echo "Running test: $base"
	expected="$dirname/${base}_expected.out"
	if [ -e "$expected" ]; then
		out="$dirname/$base.out"
		if ! $CARGO_RUN "$cmd" "$file" >"$out" 2>&1; then
			echo "${RED}Test failed:${RESET} $base"
			echo "See file $out"
			RESULT=1
		else
			if ! delta "$dirname/$base.out" "$expected"; then
				echo "${RED}Test ${base}:${RESET} output does not match expected output"
				echo "See file: $out"
				RESULT=1
			else
				echo "${GREEN}Test passed:${RESET} $base"
				rm "$out"
			fi
		fi
	else
		echo "${RED}Missing output comparison file:${RESET} $expected"
		echo "Skipping test: $base"
	fi
}

run_tests() {
	local cmd=$1
	local files=$2

	echo "=== Running ${cmd} tests:"

	for file in $files; do
		run_test "$cmd" "$file"
	done
}

# 0 args: run all tests
# 1 arg: run matching tests for operation: tokenize or parse
# 2 args: run specific test file for operation: tokenize or parse
case $# in
0)
	run_tests tokenize "$TOKENIZE_TEST_FILES"
	run_tests parse "$PARSE_TEST_FILES"
	run_tests evaluate "$EVALUATE_TEST_FILES"
	;;
1)
	case $1 in
	"tokenize")
		run_tests tokenize "$TOKENIZE_TEST_FILES"
		;;
	"parse")
		run_tests parse "$PARSE_TEST_FILES"
		;;
	"evaluate")
		run_tests evaluate "$EVALUATE_TEST_FILES"
		;;
	*)
		echo "${RED}Unknown operation type: $1${RESET}"
		usage
		exit 1
		;;
	esac
	;;
2)
	case $1 in
	"tokenize" | "parse" | "evaluate")
		run_test "$1" "$2"
		;;
	*)
		echo "${RED}Unknown operation type: $1${RESET}"
		usage
		exit 1
		;;
	esac
	;;
*)
	usage
	exit 1
	;;
esac

if [ "$RESULT" -eq 0 ]; then
	echo "${GREEN}All tests passed!${RESET}"
else
	echo "${RED}Some tests failed.${RESET}"
fi

exit $RESULT
