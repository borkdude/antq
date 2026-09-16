#!/usr/bin/env bash
set -Euo pipefail

# antq must keep running on babashka. Scans a project with a deliberately old
# dependency and checks that antq reports a newer version for it.

antq_root=$PWD
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT

cat > "$dir/deps.edn" <<'EOF'
{:deps {org.clojure/tools.cli {:mvn/version "0.3.5"}}}
EOF

cd "$dir" || exit 1
out=$(bb -Sdeps "{:deps {com.github.liquidz/antq {:local/root \"$antq_root\"}}}" \
         -m antq.core --reporter=edn)
status=$?
echo "$out"

if [ "$status" -eq 0 ]; then
  echo "Expected antq to report an outdated dependency"
  exit 1
fi

if ! echo "$out" | grep -q "org.clojure/tools.cli"; then
  echo "Expected antq to report org.clojure/tools.cli"
  exit 1
fi

exit 0
