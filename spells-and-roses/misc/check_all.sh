#!/bin/bash

set -e -x

dir=$(dirname $(readlink -f $0))

for script in $dir/*.ml; do
    ./spells_and_roses.native script -check-only "$script"
done

echo -e "\e[32mAll checks ok\e[0m"
