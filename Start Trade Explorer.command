#!/bin/bash
# ============================================================
#  Canadian Trade Explorer - macOS / Linux launcher
#  macOS: double-click this file. Linux: run  bash "Start Trade Explorer.command"
# ============================================================
cd "$(dirname "$0")" || exit 1

RSCRIPT="$(command -v Rscript)"
if [ -z "$RSCRIPT" ]; then
  for candidate in \
    /Library/Frameworks/R.framework/Resources/bin/Rscript \
    /opt/homebrew/bin/Rscript \
    /usr/local/bin/Rscript \
    /usr/bin/Rscript; do
    if [ -x "$candidate" ]; then RSCRIPT="$candidate"; break; fi
  done
fi

if [ -z "$RSCRIPT" ]; then
  echo
  echo "  R is not installed on this computer (or could not be found)."
  echo
  echo "  1. Your browser will now open the R download page."
  echo "  2. Download and install R for your system, accepting the defaults."
  echo "  3. Then open \"Start Trade Explorer\" again."
  echo
  URL="https://cran.r-project.org/"
  if command -v open >/dev/null 2>&1; then open "$URL"; elif command -v xdg-open >/dev/null 2>&1; then xdg-open "$URL"; fi
  read -r -p "Press Enter to close this window."
  exit 1
fi

echo "Using R at: $RSCRIPT"
if ! "$RSCRIPT" launch.R; then
  echo
  echo "  Something went wrong - see the message above."
  read -r -p "Press Enter to close this window."
fi
