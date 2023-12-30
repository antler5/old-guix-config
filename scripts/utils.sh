#!/usr/bin/env bash

###
### Shell IO
###

BAD=$(printf '\033[1;31m')
BRACKET=$(printf '\033[1;34m')
GOOD=$(printf '\033[1;32m')
HILITE=$(printf '\033[1;36m')
NORMAL=$(printf '\033[0m')
WARN=$(printf '\033[1;33m')

INDENT=""
einfo () { printf '%s*%s %s%s\n' "$GOOD" "$NORMAL" "$INDENT" "$*"; }
ewarn () { printf '%s*%s %s%s\n' "$WARN" "$NORMAL" "$INDENT" "$*"; }
eerror () { printf '%s*%s %s%s\n' "$BAD" "$NORMAL" "$INDENT" "$*"; return 1; }

econfirm () {
  local RESULT
  printf "%s*%s %s%s " "$HILITE" "$NORMAL" "$INDENT" "Are you SURE? (y/N)"
  read -r RESULT
  [[ "$RESULT" =~ ^[Yy]$ ]] || exit 1
}

ebegin () {
  local MSG
  MSG="$* ..."
  printf '%s*%s %s%s' "$GOOD" "$NORMAL" "$INDENT" "$MSG"
  genfun_lastbegun_strlen="$(( 3 + ${#INDENT} + ${#MSG} ))"
}

eend () {
  local COLOR MSG OUTPUT RETVAL WARN_P
  WARN_P="$1"
  shift
  if OUTPUT="$( { ${@:-:}; } 2>&1; )"; then
    MSG="${BRACKET}[ ${GOOD}ok${BRACKET} ]${NORMAL}"
  else
    RETVAL=1
    (( WARN_P )) && COLOR="$WARN" || COLOR="$BAD"
    MSG="${BRACKET}[ ${COLOR}!!${BRACKET} ]${NORMAL}"
  fi
  printf "%$(( 80 - genfun_lastbegun_strlen - 7 ))s %s\n" '' "${MSG}"
  if (( ${RETVAL:-0} )); then
    [[ -n "$OUTPUT" ]] && printf '%s\n' "$OUTPUT" 1>&2
    return "$RETVAL"
  fi
}

erun () { ebegin $1; shift; eend 0 $@; }
ewrun () { ebegin $1; shift; eend 1 $@; }

###
### Disk IO
###

find-partition () {
  local DEVICE PARTITION_NUMBER SUFFIX CONCAT
  DEVICE="$1" && [[ -b "$DEVICE" ]] && shift
  PARTITION_NUMBER="$1" && [[ -n "$PARTITION_NUMBER" ]] && shift
  for SUFFIX in "" "p" "-part"; do
    CONCAT="${DEVICE}${SUFFIX}${PARTITION_NUMBER}"
    if [[ -b "$CONCAT" ]]; then
      printf '%s' "$CONCAT"
      return 0
    fi
  done
}

###
### Read Encryption Passphrase
###

declare PASSPHRASE
read-first-passphrase () {
  read -srp "${HILITE}*$NORMAL Enter encryption passphrase: " PASSPHRASE; printf '\n'
  until [[ "${#PASSPHRASE}" -gt 0 ]] || { [[ $- == *x* ]] || printf '\e[K' && eerror "Passphrase can not be empty"; }; do
    { [[ $- == *x* ]] || printf '\e[2A'; }
    read -srp "${HILITE}*$NORMAL Enter encryption passphrase: " PASSPHRASE; printf '\n'
  done
}

verify-passphrases () {
  # I do it like this because I don't personally trust [[ not to leak to the
  # process-tree like [ would. It's probably un-unnecessary, but hey why not.
  local TMP
  TMP="${PASSPHRASE#"$PASSPHRASE_2"}"
  [[ "${#TMP}" -eq 0 ]]
}

read-encryption-passphrase () {
  read-first-passphrase
  { [[ $- == *x* ]] || printf '\e[K'; }
  read -srp "${HILITE}*$NORMAL Verify passpharse: " PASSPHRASE_2; printf '\n'
  until verify-passphrases $PASSPHRASE $PASSPHRASE_2 || { [[ $- == *x* ]] || printf '\e[1A\e[K' && eerror "Passphrases do not match"; }; do
    { [[ $- == *x* ]] || printf '\e[2A'; }
    read-first-passphrase
    { [[ $- == *x* ]] || printf '\e[K'; }
    read -srp "${HILITE}*$NORMAL Verify passpharse: " PASSPHRASE_2; printf '\n'
  done
}

expect-passphrase-prompt () {
  PROMPT="$1" && [[ -n "$1" ]] && shift
  expect << EOF
spawn $@
expect {
  "$PROMPT" {
    send -- "$PASSPHRASE\r"
    exp_continue
  }
  eof
}
lassign [wait] pid spawnid os_error_flag value
exit \$value
EOF
}

pretty-print-store-item () {
  local item index max_width suffix take_right
  index="$1"
  max_width="$(($2 - 18 - index))"
  item="$3"
  suffix="${item:$((index + 43))}"
  take_right="$((${#suffix} - ( max_width > ${#suffix} ? ${#suffix} : max_width )))"
  printf '%s%s%s' "${item::((index + 15))}" "..." "${suffix:$take_right}"
}

pretty-print-abbr () {
  local START END MAX_LENGTH STRING
  [[ $1 -ge 0      ]] && START="$1"         && shift
  [[ $1 -gt $START ]] && END="$1"           && shift
  [[ $1 -ge 3      ]] && MAX_LENGTH="$1"    && shift
  [[ -n $1         ]] && STRING="$1"        && shift
  if [[ $(( END - START )) -le MAX_LENGTH ]]; then
    printf '%s' "$STRING"
  else
    printf '%s...%s' "${STRING::((START + MAX_LENGTH - 3))}" "${STRING:$END}"
  fi
}
