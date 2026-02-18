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
SWIG_DIR="${SRCDIR}/_reference/swig/Lib"
cmake -B "${BUILD}" -S "${SRCDIR}" \
  -DMFEM_DIR="${MFEM_BUILD}" \
  -DSWIG_EXECUTABLE="${SWIG_DIR}/../swig" \
  -DSWIG_DIR="${SWIG_DIR}"
cmake --build "${BUILD}" -j"$(nproc)"

# Run tests
echo "=== Running tests ==="
ctest --test-dir "${BUILD}" --output-on-failure -L serial
