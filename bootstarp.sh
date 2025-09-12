#!/usr/bin/bash
set -o nounset
set -o errtrace
#set -o pipefail
function CATCH_ERROR { # {{{
    local __LEC=$? __i __j
    echo "Traceback (most recent call last):" >&2
    for ((__i = ${#FUNCNAME[@]} - 1; __i >= 0; --__i)); do
        printf '  File %q line %s in %q\n' >&2 \
            "${BASH_SOURCE[__i]}" \
            "${BASH_LINENO[__i]}" \
            "${FUNCNAME[__i]}"
        if ((BASH_LINENO[__i])) && [ -f "${BASH_SOURCE[__i]}" ]; then
            for ((__j = 0; __j < BASH_LINENO[__i]; ++__j)); do
                read -r REPLY
            done < "${BASH_SOURCE[__i]}"
            printf '    %s\n' "$REPLY" >&2
        fi
    done
    echo "Error: [ExitCode: ${__LEC}]" >&2
    exit "${__LEC}"
}
trap CATCH_ERROR ERR # }}}

hash rm

bootstarp=src/bootstarp

test -f grammar.abnf
test -f $bootstarp.rs

cargo run < grammar.abnf > ${bootstarp}_oldgen.rs
mv -v $bootstarp.rs ${bootstarp}_old.rs
mv -v ${bootstarp}_oldgen.rs $bootstarp.rs
cargo run < grammar.abnf > ${bootstarp}_newgen.rs

diff $bootstarp.rs ${bootstarp}_newgen.rs
