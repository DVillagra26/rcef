#!/usr/bin/env bash
# Every check, in the order it should fail in. Run from the repo root
# after R CMD INSTALL.
#
# Exit codes are checked explicitly rather than relying on `set -e`: a
# failing command on the LEFT of a pipe does not fail the pipeline, so
# `Rscript x.R | tail -1` under `set -e` reports success no matter what
# happened. The first version of this script did exactly that and printed
# ALL CHECKS PASSED over eleven broken suites.
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

for c in check_ascii check_cssquotes check_syntax check_exports check_sections check_app check_server; do
  run "$c" "dev/$c.R"
done
for t in dev/tests/test_*.R; do
  printf '%-22s ' "$(basename "$t" .R)"
  out=$(Rscript -e "suppressPackageStartupMessages(library(rcef)); source('$t')" 2>&1); rc=$?
  if [ $rc -ne 0 ]; then
    fail=$((fail+1)); echo "FAILED (exit $rc)"; printf '%s\n' "$out" | tail -8 | sed 's/^/      /'
  else
    printf '%s\n' "$out" | tail -1
  fi
done

echo
echo "--- parity report (informational, does not fail the run) ---"
Rscript dev/check_parity.R || true

echo
if [ $fail -eq 0 ]; then echo "ALL CHECKS PASSED"; else echo "$fail CHECK(S) FAILED"; fi
exit $fail
