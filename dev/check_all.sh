#!/usr/bin/env bash
# Every check, in the order it should fail in. Run from the repo root
# after R CMD INSTALL.
#
# Exit codes are checked explicitly rather than relying on `set -e`: a
# failing command on the LEFT of a pipe does not fail the pipeline, so
# `Rscript x.R | tail -1` under `set -e` reports success no matter what
# happened. The first version of this script did exactly that and printed
# ALL CHECKS PASSED over eleven broken suites.
#
# Exit code alone is not enough either. A dev/tests/test_*.R suite prints
# "DONE  N failure(s)" from its own FAIL counter, but only SOME of them
# end with quit(status = 1L) when FAIL > 0 - Rscript otherwise exits 0
# regardless of how many "FAIL" lines were printed. This ran silently for
# a while: test_deck_layout.R had 17 real failures (stale step numbers
# after an unrelated UI change) and this script still printed ALL CHECKS
# PASSED, because rc was 0. The DONE line is parsed below as a second,
# independent signal - a suite fails this script if EITHER its exit code
# is non-zero OR its own counter says N > 0 - so a test file forgetting
# quit(status = 1L) is a missing safety net, not a silent hole.
fail=0
run () {
  printf '%-22s ' "$1"
  out=$(Rscript "$2" 2>&1); rc=$?
  last=$(printf '%s' "$out" | tail -1)
  if [ $rc -ne 0 ]; then
    fail=$((fail+1)); echo "FAILED (exit $rc)"; printf '%s\n' "$out" | tail -8 | sed 's/^/      /'
  else
    echo "$last"
  fi
}

for c in check_ascii check_syntax check_exports; do
  run "$c" "dev/$c.R"
done
for t in dev/tests/test_*.R; do
  printf '%-22s ' "$(basename "$t" .R)"
  out=$(Rscript -e "suppressPackageStartupMessages(library(rcef)); source('$t')" 2>&1); rc=$?
  # A suite's own FAIL counter is the ground truth, independent of rc.
  # Extract the trailing integer from a line matching "DONE ... N ...".
  # No match (a suite not using the ok()/FAIL convention) is treated as
  # 0 - exit code is still checked below regardless.
  donen=$(printf '%s\n' "$out" | grep -oE 'DONE[^0-9]*[0-9]+' | tail -1 | grep -oE '[0-9]+$')
  if [ $rc -ne 0 ] || { [ -n "$donen" ] && [ "$donen" -ne 0 ]; }; then
    fail=$((fail+1))
    if [ $rc -ne 0 ]; then echo "FAILED (exit $rc)"
    else echo "FAILED ($donen failure(s), exit 0 - missing quit(status=1L)?)"; fi
    printf '%s\n' "$out" | tail -8 | sed 's/^/      /'
  else
    printf '%s\n' "$out" | tail -1
  fi
done

echo
if [ $fail -eq 0 ]; then echo "ALL CHECKS PASSED"; else echo "$fail CHECK(S) FAILED"; fi
exit $fail
