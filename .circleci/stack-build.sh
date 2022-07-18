#!/usr/bin/env bash

set -eu -o pipefail

STACK_ARGS=(--no-terminal)

if [[ "$*" != *--local-bin-path* ]]; then
    STACK_ARGS+=(--local-bin-path /usr/local/bin)
fi

exec stack build "${STACK_ARGS[@]}" "$@"
