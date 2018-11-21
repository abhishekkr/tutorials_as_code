
jokeBannerX(){
  local RESET_X=${-//[^x]/} ## to avoid 'set -x' messing overwriting
  [[ "${RESET_X}" == "x" ]] && set +x
  local SLEEP_FOR="$1"
  for i in `seq $SLEEP_FOR -1 1`; do
    sleep 5
    echo -en "\e[1A"; echo -e "\e[0K\r"$( curl -skL http://api.icndb.com/jokes/random\?firstName=John\&lastName=Doe | jq ".value.joke" )
  done
  [[ "${RESET_X}" == "x" ]] && set -x
}

jokeBanner(){
  local RESET_X=${-//[^x]/} ## to avoid 'set -x' messing overwriting
  [[ "${RESET_X}" == "x" ]] && set +x
  while true; do
    echo -en "\e[1A\e[1A"; echo -e "\e[0K\r\e[0K\r"$( curl -skL http://api.icndb.com/jokes/random\?firstName=John\&lastName=Doe | jq ".value.joke" )
    sleep 5
  done
  [[ "${RESET_X}" == "x" ]] && set -x
}

jokeBanner
