#!/usr/bin/env bash

set -e -o pipefail

function header() {
    echo -e "\n===== $* ====="
}

function msg() {
    echo ">>> $*"
}

function abort() {
    echo "$*" >&2
    exit 1
}

HPC_LCOV="${1?}"
DEST="${2?}"

echo "Using hpc-lcov: ${HPC_LCOV}"
echo "Storing coverage files at: ${DEST}"

mkdir -p "${DEST}"
COUNTER=0

function exec_hpc_lcov() {
    msg "Executing: hpc-lcov $*"
    "${HPC_LCOV}" "$@"

    local HPC_LCOV_TIX="$(basename "${HPC_LCOV}").tix"

    if [[ -n "${CI_LATEST}" ]]; then
        local DEST_TIX="${DEST}/hpc-lcov-exe-${COUNTER}.tix"
        mv "${HPC_LCOV_TIX}" "${DEST_TIX}"
        msg "Tix file stored as ${DEST_TIX}"
    fi

    COUNTER=$(( COUNTER + 1 ))
}

function check_output() {
    local FILE="${1?}"
    if [[ ! -f "${FILE}" ]]; then
        abort "${FILE} not found"
    fi
    msg "${FILE} exists."
    rm "${FILE}"
}

function test_hpc_lcov() {
    exec_hpc_lcov "$@"
    check_output lcov.info
}

header "Test without arguments"
test_hpc_lcov

header "Testing --help"
exec_hpc_lcov --help
exec_hpc_lcov -h

header "Testing --output"
exec_hpc_lcov --output /tmp/my-exe-lcov.info
check_output /tmp/my-exe-lcov.info
exec_hpc_lcov -o /tmp/my-exe-lcov.info
check_output /tmp/my-exe-lcov.info

header "Testing --file"
TIX_FILE="$(find "$(stack path --local-hpc-root)" -name "*.tix" | head)"
test_hpc_lcov --file "${TIX_FILE}"
test_hpc_lcov -f "${TIX_FILE}" -f "${TIX_FILE}"
