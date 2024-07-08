#!/usr/bin/env bash

users=(alice bob chuck duck eve)

echo "\$users: ${users}"
echo "\$users[*]: ${users[*]}"
echo "\$users[@]: ${users[@]}"
echo "\$users[0]: ${users[0]}"
echo "\$users[1]: ${users[1]}"
echo "\$users[4]: ${users[4]}"

users+=( frey ghost )
for user in "${users[@]}"; do
  echo ">> $user"
done

echo "\$#users[@]: ${#users[@]}"
echo "\$!users[@]: ${!users[@]}"
for userid in "${!users[@]}"; do
  echo ">> [#${userid}] ${users[$userid]}"
done

echo "${users[@]:2:2}"

# ITS SAME AS USING -a keeps Indexed Array Variable
declare -a indexed_state
indexed_state=(
  "OK"
  "WARN"
  "ERROR"
)
indexed_state[-2]=BLAHBLAH
for sid in "${!indexed_state[@]}"; do
  echo ">> [${sid}] ${indexed_state[$sid]}"
done


## Using "declare" Associative Array for Array
declare -A app_state
app_state=(
  [0]="OK"
  [1]="WARN"
  [2]="ERROR"
)
app_state[-1]="UNDEF"
for sid in "${!app_state[@]}"; do
  echo ">> [${sid}] ${app_state[$sid]}"
done

check-state(){
  local app_code="$1"
  echo "App is in ${app_state[$app_code]} state."
}
check-state -1
check-state 1
check-state 0
check-state 2


## Using "declare" Associative Array for KeyVal Map
declare -A uris=(
  [api]="api.example.com"
  [www]="example.com"
  [beta]="beta.example.com"
)

echo "API calls at https://${uris[api]}"
for u in "${!uris[@]}"; do
  echo "URI for ${u}: ${uris[$u]}"
done

unset uris[beta]
echo "[new key set] ${!uris[*]}"

uris[api]="api2.example.com"
echo "[new value set] ${uris[*]}"

## READONLY
declare -r -A DNS=(
  [google]="8.8.8.8"
  [cloudflare]="1.1.1.1"
)
echo "[DNS keys] ${!DNS[*]}"
# unset DNS[google]
## echo "[DNS keys] ${DNS[*]}"
# DNS[cloudflare]="192.168.1.10"
## echo "[DNS keys] ${DNS[*]}"

# Auto CharCase Conversion
declare -l LOWER_MSG
LOWER_MSG="THIS will be ALL lowercase"
echo "[lower] ${LOWER_MSG}"

declare -u UPPER_MSG
UPPER_MSG="THIS will be ALL uppercase"
echo "[upper] ${UPPER_MSG}"


# ONLY INT VALUES VAR; any other becomes ZERO
declare -i EXITCODE
EXITCODE=1
echo "[code] ${EXITCODE}"
EXITCODE="ABC"
echo "[code] ${EXITCODE}"


# Making -A to Have A Set like List
declare -A USER_GROUPS
USER_GROUPS[admin]=true
USER_GROUPS[guest]=true
USER_GROUPS[johndoe]=true
echo "[UserGroups] ${!USER_GROUPS[*]}"
USER_GROUPS[johndoe]=true
echo "[UserGroups] ${!USER_GROUPS[*]}"
[[ ${USER_GROUPS[johndoe]} ]] && echo "johndoe"
[[ ${USER_GROUPS[nobody]} ]] && echo "nobody"
