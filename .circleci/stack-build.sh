#!/usr/bin/env bash

set -eu -o pipefail

STACK_ARGS=(
    --ghc-options -Werror
)

if [[ "$*" != *--local-bin-path* ]]; then
    STACK_ARGS+=(--local-bin-path /usr/local/bin)
fi

# tasty-discover-3.0.2 does not discover all modules by default, but this
# flag is deprecated in newer versions
if [[ "${STACK_YAML}" == "stack-ghc-8.0.yaml" ]]; then
  STACK_ARGS+=(--ghc-options '-optF --no-module-suffix')
fi

exec stack build "${STACK_ARGS[@]}" "$@"
