#!/bin/bash
set -euo pipefail

SRCDIR="$(cd "$(dirname "$0")" && pwd)"
MFEM_SRC="${SRCDIR}/_reference/mfem"
MFEM_BUILD="${MFEM_SRC}/build"
BUILD="${SRCDIR}/build"

# Build MFEM (serial, Release)
echo "=== Building MFEM ==="
cmake -B "${MFEM_BUILD}" -S "${MFEM_SRC}" \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_POSITION_INDEPENDENT_CODE=ON
cmake --build "${MFEM_BUILD}" -j"$(nproc)"

# Build guile-mfem (clean to avoid stale cache)
echo "=== Building guile-mfem ==="
rm -rf "${BUILD}"
cmake -B "${BUILD}" -S "${SRCDIR}" \
  -DMFEM_DIR="${MFEM_BUILD}"
cmake --build "${BUILD}" -j"$(nproc)"

# Run tests
echo "=== Running tests ==="
cd "${BUILD}" && ctest --output-on-failure -L serial
