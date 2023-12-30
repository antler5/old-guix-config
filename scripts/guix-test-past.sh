#!/usr/bin/env bash

# desperate times

# do not run this

set -xeuo pipefail

i=0
export i

rollback () {
  for dir in ../assorted_upstreams/{non,}guix; do
    pushd $dir; {
      git reset --hard "$(git rev-list origin/master -n 1 --before="$(date --date="$i day ago")")"
    }; popd
  done
}

until ( guix time-machine --channels=./channels.scm --disable-authentication -- system build --load-path=./modules ./modules/antlers/systems/tower.scm; ); do
  i=$((++i))
  rollback
done
