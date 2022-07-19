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

HPC_LCOV=${1}
COVERAGE_DIR=${2}

if [[ -x "${HPC_LCOV}" ]]; then
    echo "Using hpc-lcov: ${HPC_LCOV}"
else
    abort "Not an existing executable: ${HPC_LCOV}"
fi

mkdir -p "${COVERAGE_DIR}"
COUNTER=0

function exec_hpc_lcov() {
    msg "Executing: hpc-lcov $*"
    "${HPC_LCOV}" "$@"

    local HPC_LCOV_TIX="$(basename "${HPC_LCOV}").tix"
    if [[ -f "${HPC_LCOV_TIX}" && -n "${COVERAGE_DIR}" ]]; then
        local DEST_TIX="${COVERAGE_DIR}/hpc-lcov-exe-${COUNTER}.tix"
        mv "${HPC_LCOV_TIX}" "${DEST_TIX}"
        msg "Tix file stored as ${DEST_TIX}"
        COUNTER=$(( COUNTER + 1 ))
    fi
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
exec_hpc_lcov --output /tmp/my-exe-lcov-1.info
check_output /tmp/my-exe-lcov-1.info
exec_hpc_lcov -o /tmp/my-exe-lcov-2.info
check_output /tmp/my-exe-lcov-2.info

header "Testing --file"
TIX_FILE="$(find "$(stack path --local-hpc-root)" -name "*.tix" | head)"
test_hpc_lcov --file "${TIX_FILE}"
test_hpc_lcov -f "${TIX_FILE}" -f "${TIX_FILE}"
