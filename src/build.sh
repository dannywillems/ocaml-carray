#!/bin/sh -e
if [ -n "${BUILT_WITH_TRACK_CUSTOM_BLOCK}" ]; then
    echo "(-D__BUILT_WITH_TRACK_CUSTOM_BLOCK__ -DCAML_INTERNALS)" > c_flags_carray.sexp
else
    echo "()" > c_flags_carray.sexp
fi
