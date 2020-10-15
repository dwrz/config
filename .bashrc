# Ignore non-interactive shells.
[[ $- != *i* ]] && return

# VARIABLES
black="\[\e[0;30m\\]"
red="\[\e[0;31m\\]"
green="\[\e[0;32m\\]"
yellow="\[\e[0;33m\\]"
blue="\[\e[0;34m\\]"
magenta="\[\e[0;35m\\]"
cyan="\[\e[0;36m\\]"
white="\[\e[0;37m\\]"

bold_black="\[\e[01;30m\\]"
bold_red="\[\e[01;31m\\]"
bold_green="\[\e[01;32m\\]"
bold_yellow="\[\e[01;33m\\]"
bold_blue="\[\e[01;34m\\]"
bold_magenta="\[\e[01;35m\\]"
bold_cyan="\[\e[01;36m\\]"
bold_white="\[\e[01;37m\\]"

icon_checkmark="\342\234\223"
icon_xmark="\342\234\227"
no_color="\[\e[00m\\]"

# ALIASES
alias aoeu="setxkbmap us"
alias asdf="setxkbmap dvorak; xmodmap ~/.xmodmap"
alias c="cat"
alias cal="cal --monday --iso --week -3"
alias camera="mpv av://v4l2:/dev/video0"
alias cheat="~/.bin/cht.sh"
alias checkmail="~/.msmtpqueue/checkmail.sh"
alias cm="~/.msmtpqueue/checkmail.sh"
alias cp="cp -i"
alias cpwd="pwd | tr -d '\n' | xclip"
alias d2u="find . -type f -print0 | xargs -0 -n 1 -P 4 dos2unix"
alias df="df -h"
alias diff="diff --color=auto"
alias dirdu="du -cksh * | sort -hr"
alias docker="sudo docker"
alias dwrznet_publish="rsync -av --progress --delete ~/ruck/pd/dwrz/ -e 'ssh -p 8822 -i ~/.ssh/srv-nyc' dwrz@dwrz.net:/home/dwrz/dwrz/"
alias e="emacsclient -t"
alias gcc="gcc -O3 -Wall -Wextra -Wstrict-prototypes -std=c17 -pedantic"
alias grep="grep --color=auto"
alias home_wifi="nmcli r wifi on; nmcli c up home-nyc-wifi"
alias ha="history -a"
alias hn="history -n"
alias hr='printf $(printf "\e[$(shuf -i 91-97 -n 1);1m%%%ds\e[0m\n" $(tput cols)) | tr " " ='
alias hw="home_wifi"
alias j="journalctl"
alias kernel_version="uname -a"
alias km="xmodmap ~/.xmodmap"
alias l="light -S"
alias la="ls -A"
alias le="xrandr --output HDMI-2 --brightness"
alias ll="ls -lhF --group-directories-first"
alias lll="ls -alhF --group-directories-first"
alias locks="i3lock -c ffffff -f; systemctl suspend"
alias ls="ls --color=auto"
alias lss="ls -lShr"
alias lt="localtime"
alias me="xrandr --output eDP-1 --off --output HDMI-2 --auto --brightness 0.8"
alias mkdir="mkdir -p"
alias ml="xrandr --output eDP-1 --auto --output HDMI-2 --off"
alias mobile="ssh dwrz@mobile"
alias mv="mv -i"
alias my_ip="curl ifconfig.io"
alias open="xdg-open"
alias pacman="sudo pacman"
alias py_http="python3 -m http.server 8000"
alias qd="diff --side-by-side --suppress-common-lines"
alias rb=". ~/.bashrc"
alias rc="remind-calendar"
alias rd="rm -rfI"
alias rm="rm -I"
alias rt="remind-timer"
alias s="systemctl"
alias sp="tmux new-session -A -s scratch"
alias tablet="ssh dwrz@tablet"
alias t1="tree -CL 1"
alias t2="tree -CL 2"
alias t3="tree -CL 3"
alias t="tmux new-session -A -s main"
alias thesaurus="aiksaurus"
alias timers="systemctl --user -all list-timers"
alias tree="tree -C"
alias unread="notmuch search tag:unread"
alias vdir="vdir --color=auto"
alias which="type -a"
alias wisdom='$HOME/ruck/oo/scripts/wisdom/wisdom.sh'

# EXPORTS
export EDITOR="emacsclient -t --alternate-editor mg"
export GCC_COLORS="error=01;31:warning=01;35:note=01;36:caret=0$"
export HISTCONTROL=ignoreboth:erasedups
export NVM_DIR="$HOME/.nvm"
export VISUAL="emacsclient -c --alternate-editor mg"

# FUNCTIONS
bb() {
  if ! [[ -x "$(command -v borg)" ]]; then
    echo "borg not installed" >&2
    return 1
  fi
  case "$1" in
    "open")
      sudo cryptsetup luksOpen /dev/sdb1 dwrz-archival-backup
      sudo mount /dev/mapper/dwrz-archival-backup /mnt/dwrz-archival-backup/
      ;;
    "list") borg list /mnt/dwrz-archival-backup/dwrz-backup/ ;;
    "create")
      name="dwrz@earth-$(TZ=UTC date '+%FT%T%z')"
      borg create -v --progress --exclude /home/dwrz/.cache/ \
	   /mnt/dwrz-archival-backup/dwrz-backup/::"$name" /home/dwrz/
      ;;
    "prune") borg prune --keep-last 1 -w 1 -m 12 --save-space \
		  /mnt/dwrz-archival-backup/dwrz-backup/
      ;;
    "mount") borg mount /mnt/dwrz-archival-backup/dwrz-backup/ \
		  /mnt/dwrz-archival-backup/mnt
      ;;
    "unmount") borg umount /mnt/dwrz-archival-backup/mnt ;;
    "close")
      sudo umount /mnt/dwrz-archival-backup
      sudo cryptsetup luksClose dwrz-archival-backup
      ;;
    *) printf "unrecognized command: %s\n" "$1" >&2 ;;
  esac
}

change-bg-map() {
  if ! [[ -x "$(command -v feh)" ]]; then
    echo "feh not installed" >&2
    return 1
  fi
  case "$1" in

    "local")
      if ! [[ -x "$(command -v create-static-map)" ]]; then
	echo "create-static-map not installed" >&2
	return 1
      fi
      if ! [[ -x "$(command -v geo)" ]]; then
	echo "geo not installed" >&2
	return 1
      fi
      local coordinates;
      coordinates="$(geo)"
      create-static-map \
	--width 1920 --height 1080 \
	-o /tmp/local-map.png \
	-c "$coordinates" -z 15 && \
	feh --bg-fill "/tmp/local-map.png"
      ;;

    *) local mapsdir;
       mapsdir="$HOME/ruck/oo/images/maps"
       if ! [[ -d "$mapsdir" ]]; then
	 echo "no ruck maps directory on this host" >&2
	 return 1
       fi
       feh --bg-fill --randomize "$mapsdir"
       ;;

  esac
}

compress-jpg() {
  mkdir 1920x;
  mkdir 800x;

  for p in *.jpg; do
    basename=${p%.*}
    convert -strip -resize 800 -quality 50 "$p" "./800x/$basename-800.jpg";
    convert -strip -resize 1920 -quality 50 "$p" "./1920x/$basename-1920.jpg";
  done
}

compress-video() {
  local filetype crf
  filetype=$(printf "*.%s" "$1")
  # "The range of the CRF scale is 0–51, where 0 is lossless, 23 is the default,
  # and 51 is worst quality possible. A lower value generally leads to higher
  #quality, and a subjectively sane range is 17–28. Consider 17 or 18 to be
  # visually lossless or nearly so; it should look the same or nearly the same
  # as the input but it isn't technically lossless."
  crf="$2"
  if [[ -z "$crf" ]]; then
    crf=32 # Use a default CRF of 32.
  fi
  mkdir compressed
  for v in $filetype; do
    ffmpeg -i "$v" -vcodec libx264 -crf "$crf" ./compressed/"$v";
  done
}

d() {
  case "$1" in
    "file") date '+%Y%m%d-%H%M%S' ;;
    "iso") date '+%Y-%m-%dT%H:%M:%S%z' ;;
    "mime") date '+%a, %d %b %Y %H:%M:%S %z' ;;
    "unix") date +%s ;;
    # u2h: convert Unix to human readable.
    "u2h" ) date -d "@$2";;
    *) date "$@"
  esac
}

dict() {
  sdcv --non-interactive --utf8-output --color "$@" 2>&1 | \
    fold --width=80 --spaces | \
    less -FRX
}

emacstest() {
  emacs -batch -l ert -l "$@" -f ert-run-tests-batch-and-exit
}

## fd: find directories
fd() {
  find . -type d -iname '*'"$*"'*' -ls ;
}

## ff: find files
ff() {
  find . -type f -iname '*'"$*"'*' -ls ;
}

fix-ddns() {
  if ! [[ -x "$(command -v noip)" ]]; then
    echo "noip not installed" >&2
    return 1
  fi
  local domains user password
  domains=(
    "dwrz.net"
    "www.dwrz.net"
  )
  ip="$(dig +short myip.opendns.com @resolver1.opendns.com)"
  hz="$(pass aws/route53/hosted-zones/dwrznet)"
  update='{ "Comment": "DDNS", "Changes": [ { "Action": "UPSERT", "ResourceRecordSet": { "Name": "dwrz.net", "Type": "A", "TTL": 300, "ResourceRecords": [ { "Value": "'"$ip"'" } ] } } ] }'

  printf "ip is %s" "$ip\n"

  for d in "${domains[@]}"; do
    printf "requesting update for %s\n" "$d"
    aws route53 change-resource-record-sets \
	--hosted-zone-id "$hz" \
	--change-batch "$update"
  done
}

gps() {
  if ! [[ -x "$(command -v mmcli)" ]]; then
    echo "mmcli not installed" >&2
    return 1
  fi
  local cmd modem;
  cmd="$1"
  modem="$(mmcli --list-modems | cut -c 42)"
  case "$cmd" in
    "disable") mmcli \
		 --location-disable-3gpp \
		 --location-disable-gps-raw \
		 --location-disable-gps-nmea \
		 -m "$modem"
	       ;;
    "enable") mmcli \
		--location-enable-3gpp \
		--location-enable-gps-raw \
		--location-enable-gps-nmea \
		-m "$modem"
	      ;;
    "get") mmcli --location-get -m "$modem" ;;
    *) printf "unrecognized command: %s\n" "$cmd" >&2 ;;
  esac
}

mfa() {
  case "$1" in
    "aws") oathtool -b --totp "$(pass amazon/mfa)" | xclip ;;
    "g") oathtool -b --totp "$(pass google/mfa/dwrz@dwrz.net)" | xclip ;;
    "gmail") oathtool -b --totp "$(pass google/mfa/dwriccardizhu@gmail.com)" \
	       | xclip ;;
    "ms") oathtool -b --totp "$(pass microsoft/mfa)" | xclip ;;
    *) printf "unrecognized service: %s\n" "$1" >&2
  esac
}

mobile-backup() {
  if [[ "$OSTYPE" != "linux-android" ]]; then
    echo "not on mobile" >&2
    return 1
  fi

  local dcim_dir screenshot_dir downloads_dir

  dcim_dir="$HOME/storage/dcim/"
  if [[ -d "$dcim_dir" ]]; then
    printf "backing up %s\n" "$dcim_dir"
    rsync -av "$dcim_dir" dwrz@earth:/home/dwrz/mobile/dcim/
    result=$?
    if [[ result -eq 0 ]]; then
      rm -rf "$dcim_dir"
    fi
  fi

  screenshot_dir="$HOME/storage/pictures/Screenshots/"
  if [[ -d "$screenshot_dir" ]]; then
    printf "backing up %s\n" "$screenshot_dir"
    rsync -av "$screenshot_dir" dwrz@earth:/home/dwrz/mobile/screenshots/
    result=$?
    if [[ result -eq 0 ]]; then
      rm -rf "$screenshot_dir"
    fi
  fi

  downloads_dir="$HOME/storage/downloads/"
  if [[ -d "$downloads_dir" ]]; then
    printf "backing up %s\n" "$downloads_dir"
    rsync -av "$downloads_dir" dwrz@earth:/home/dwrz/mobile/downloads/
    result=$?
    if [[ result -eq 0 ]]; then
      rm -rf "$downloads_dir"
    fi
  fi
}

paclean() {
  sudo pacman -Rns $(pacman -Qtdq)
}

panopticat() {
  local host password port user;

  password="$(pass panopticat/password)"
  user="$(pass panopticat/user)"

  case "$1" in
    "local")
      host="$(pass panopticat/host-local)"
      port="$(pass panopticat/port-local)"
      mpv "http://$user@$password@$host:$port"
      ;;
    *)
      host="$(pass panopticat/host)"
      port="$(pass panopticat/port)"
      mpv "https://$user@$password@$host:$port"
      ;;
  esac
}

remind-calendar() {
  local date reminder
  date="$1"; shift;
  reminder="$*"

  systemd-run --user --on-calendar="$date" \
	      "$HOME/ruck/oo/scripts/remind/remind.sh" "$reminder"
}

remind-timer() {
  local delay reminder
  delay="$1"; shift;
  reminder="$*"

  systemd-run --user --on-active="$delay" --timer-property=AccuracySec=1000ms \
	      "$HOME/ruck/oo/scripts/remind/remind.sh" "$reminder"
}

repeat() {
    local c max
    max="$1"; shift;
    for ((c=1; c <= max ; c++)); do
        eval "$@";
    done
}

ruck-backup() {
  if ! [[ -d "$HOME/ruck" ]]; then
    echo "no ruck on this host" >&2
    return 1
  fi
  case "$1" in
    "mobile") rsync -av --progress --delete ~/ruck/ \
	      dwrz@mobile:/data/data/com.termux/files/home/ruck/
	      ;;
    "srv") rsync -av --progress --delete ~/ruck/ \
		 dwrz@srv-nyc:/home/dwrz/ruck/
	   ;;
    "tablet") rsync -av --progress --delete ~/ruck/ \
		    dwrz@tablet:/data/data/com.termux/files/home/ruck/
	      ;;
    *) printf "unrecognized host: %s\n" "$1" >&2
  esac
}

search() {
  if ! [[ -x "$(command -v firefox)" ]]; then
    echo "firefox not installed" >&2
    return 1
  fi
  if ! [[ -n "$(pgrep --exact firefox\|MainThread)" ]]; then
    echo "firefox not running" >&2
    return 2
  fi

  local engine;
  engine="$1"; shift;
  case "$engine" in
    "ecosia"|"e") firefox "https://www.ecosia.org/search?q=$*" ;;
    "google"|"g") firefox "https://www.google.com/search?q=$*" ;;
    "maps"|"m") firefox "http://maps.google.com/maps?q=$*" ;;
    "wikipedia"|"w") firefox "https://en.wikipedia.org/wiki/$*" ;;
    *) printf "unrecognized engine: %s\n" "$engine" ;;
  esac
}

screenshot() {
  if ! [[ -x "$(command -v maim)" ]]; then
    echo "maim not installed" >&2
    return 1
  fi
  sleep 2;
  maim -s > screenshot-"$(date '+%Y-%m-%dT%H:%M:%S%z')".jpg
}

ssha() {
  ssh-add ~/.ssh/github
  ssh-add ~/.ssh/mobile
  ssh-add ~/.ssh/srv-nyc
  ssh-add ~/.ssh/tablet
}

upsys() {
  if [[ "$HOSTNAME" == "earth" && ! -d /boot/loader ]]; then
    echo "boot partition not mounted" >&2
    return 1
  fi
  sudo reflector --verbose --latest 25 --protocol https --sort rate \
       --save /etc/pacman.d/mirrorlist && pacman -Syu
}

weather() {
  local location format units url;
  url="wttr.in/"

  location="$1"
  case "$location" in
    "nyc"|"ny") url+="NewYork" ;;
    "上海"|"shanghai"|"sh") url+="Shanghai" ;;
    "napoli"|"naples"|"na") url+="Napoli" ;;
    "moon"|"m") url+="Moon?";;
    *) url+="$location" ;;
  esac

  units="$2"
  case "$units" in
    "celsius"|"c") url+="?m" ;;
    "fahrenheit"|"f") url+="?u" ;;
    *) url+="?m";
  esac

  # Units may be set or left blank.
  # If they were set, shift the format argument into place.
  if ! [[ -z "$units" ]]; then
    shift
  fi

  format="$2"
  case "$format" in
    "simple"|"s") url+="&format=1" ;;
    "detail"|"d") url+="&format=v2" ;;
  esac

  curl "$url"
}

# OPTIONS
set -o emacs
set -o notify
set -o noclobber

shopt -s cdspell
shopt -s checkjobs
shopt -s checkwinsize
shopt -s cmdhist
shopt -s complete_fullquote
shopt -s dirspell
shopt -s dotglob
shopt -s expand_aliases
shopt -s extquote
shopt -s force_fignore
shopt -s histappend
shopt -s histreedit
shopt -s histverify
shopt -s hostcomplete
shopt -s interactive_comments
shopt -s nullglob
shopt -s progcomp
shopt -s promptvars
shopt -s sourcepath

# PROMPT
timer_now() {
  date +%s%N
}

timer_start() {
  timer_start=${timer_start:-$(timer_now)}
}

timer_stop() {
  local delta_us=$((($(timer_now) - timer_start) / 1000))
  local us=$((delta_us % 1000))
  local ms=$(((delta_us / 1000) % 1000))
  local s=$(((delta_us / 1000000) % 60))
  local m=$(((delta_us / 60000000) % 60))
  local h=$((delta_us / 3600000000))
  # Goal: always show around 3 digits of accuracy
  if ((h > 0)); then timer_show=${h}h${m}m
  elif ((m > 0)); then timer_show=${m}m${s}s
  elif ((s >= 10)); then timer_show=${s}.$((ms / 100))s
  elif ((s > 0)); then timer_show=${s}.$(printf %03d $ms)s
  elif ((ms >= 100)); then timer_show=${ms}ms
  elif ((ms > 0)); then timer_show=${ms}.$((us / 100))ms
  else timer_show=${us}us
  fi
  unset timer_start
}

set_prompt () {
  # First, get the exit code of the last command.
  last_command=$?

  # Stop the timer.
  timer_stop

  # Set the current UTC time.
  PS1="$bold_black$(date -u +"%H:%M:%S") "

  # Set the user; red if root, green otherwise.
  if [[ "$EUID" == 0 ]]; then
    PS1+="$bold_red\\u"
  else
    PS1+="$bold_green\\u"
  fi

  # Set the "@".
  PS1+="$bold_green@"

  # Set the host; green for local, yellow for SSH, red for unsecured connection.
  if [[ -n "${SSH_CONNECTION}" ]]; then
    PS1+="$bold_yellow\\h "
  elif [[ "${DISPLAY%%:0*}" != "" ]]; then
    PS1+="$bold_red\\h "
  else
    PS1+="$bold_green\\h "
  fi

  # Set the exit code.
  PS1+="$bold_black\$last_command "

  # Set a check mark for an exit code of 0 (success).
  # Otherwise, set an X mark.
  if [[ $last_command == 0 ]]; then
    PS1+="$bold_green$icon_checkmark "
  else
    PS1+="$bold_red$icon_xmark "
  fi

  # Set the elapsed time.
  # Inherit the color from the preceding mark.
  PS1+="($timer_show) "

  # Set the working directory and prompt marker.
  # Finally, reset to no color.
  PS1+="$bold_blue\\w \\\$$no_color "
}

trap "timer_start" DEBUG
PROMPT_COMMAND="set_prompt"

# SETUP
[[ -r "$HOME/.dir_colors" ]] && eval "$(dircolors ~/.dir_colors)"

[[ -x "$(command -v keychain)" ]] &&
  eval "$(keychain --eval --noask --nogui --quiet)"

[[ -r /usr/share/bash-completion/bash_completion ]] && . /usr/share/bash-completion/bash_completion

#[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
#[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
