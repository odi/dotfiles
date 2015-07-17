# -*- sh -*-
# written by Oliver Dunkl

# set PATH
typeset -U path
path=(~/bin ~/.cabal/bin $path)

# load all files in my private function-directory
# there are some private used functions
fpath=(${HOME}/.zfunc "${fpath[@]}")

# set default editor
export EDITOR="/usr/bin/emacsclient -c -a 'emacs'"

# set nix_path
# see http://lists.science.uu.nl/pipermail/nix-dev/2015-February/016132.html
# to use unstable-channel for `nix-env -f ~/conf/packs.nix ...`
export NIX_PATH="nixpkgs=/nix/var/nix/profiles/per-user/odi/channels/nixpkgs"

# use my own colors
eval `dircolors ${HOME}/etc/colors`

# set zsh options
# see zshoptions
setopt promptsubst
setopt completealiases
setopt histignorealldups	# do not save duplicates in history
setopt sharehistory		# share history file over all instances
setopt appendhistory		# all instances append entry to file
setopt extendedhistory		# all instances will save chronologically entries
setopt autocd			# switching directories for lazy people
setopt autopushd		# push changed directory into the dir-stack
setopt pushdignoredups		# dont add duplicates into the dir-stack

# history
HISTFILE=${HOME}/.zsh_history	# where to save all history entries
HISTSIZE=12000			# ~ 20% bigger than SAVEHIST
SAVEHIST=10000			# how much entries will be saved in history

# modules
autoload -U colors && colors     # load color
autoload -Uz vcs_info            # vcs informations
autoload -U compinit && compinit # activates completion
#autoload -Uz misc && misc        # my misc functions from ~/.zfunc

# stylings
# version control systems
zstyle ':vcs_info:*'    enable git
zstyle ':vcs_info:*'    formats "%{$fg_bold[yellow]%}(%b)%{$reset_color%}"
zstyle ':vcs_info:git*' check-for-changes true
zstyle ':vcs_info:git*' stagedstr 'A'        # activate with %c
zstyle ':vcs_info:git*' unstagedstr 'M'      # activate with %u
#zstyle ':vcs_info:git*+set-message:*' hooks git-untracked
# completions
zstyle ':completion:*' menu select
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,%mem,cputime,cmd'
zstyle ':completion:*:complete:*' rehash true
zstyle ':completion:*:descriptions' format $'\e[0;46;30m%d\e[0m'

# hooks
# +vi-git-untracked() {
#   if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == 'true' ]] && \
#   [[ $(git ls-files --other --directory --exclude-standard | sed q | wc -l | tr -d ' ') == 1 ]] ; then
#   hook_com[unstaged]+='%{$fg[red]%}??'
# fi
# }

# aliases
alias ls='ls --color'
alias ll='ls -l --color'
alias la='ls -a --color'
alias lla='ls -la --color'
alias e='emacsclient -a "emacs" -c'
alias ..='cd ..'
alias ...='cd ../..'
alias g='git'
alias gs='git st'
alias gss='git st -s'
alias gll='git log'
alias gls='git ls'
alias -s txt=${EDITOR}
alias -s cabal=${EDITOR}
alias G='grep --color -i'
alias h='history'
alias feh='feh -F'
alias 1='cd -0'
alias gcal='gcalcli'

# prompt
# calculate width of the terminal with $(echo $COLUMNS/2 | bc)
# PROMPT="%(!.%{$fg_bold[red]%}.%{$fg_bold[green]%})[%n@%m %$(echo $COLUMNS/2 | bc)<..<%~%<<] %{$fg_bold[yellow]%}"$'\n'"%# %{$reset_color%}"
PROMPT="%{$fg_bold[green]%}[%n@%m %$(echo $COLUMNS/2 | bc)<..<%~%<<] "$'\n'"%# %{$reset_color%}"
# RPROMPT=$'%{$fg_bold[magenta]%}$(cabal_sandbox) %{$fg_bold[yellow]%}${vcs_info_msg_0_}%(?..%{$fg[red]%}%?)%{$reset_color%}%'
RPROMPT=$'${vcs_info_msg_0_}'

# call function before drawing the prompts
precmd () {
    print -Pn "\e]0;zsh - %l: %~ $TITLE\a"
    vcs_info
}

# check if the directory is in a cabal sandbox
cabal_sandbox () {
    # look into the current directory if there is a cabal.sandbox.config
    # if you only could start cabal from root-directory
    # it is only necessary to check the root-directory for the
    # config file
    if [[ -f cabal.sandbox.config ]]; then echo "sb"
    else echo ""
    fi
}

# starts top with a given process-name (e.g. ptop firefox)
ptop () { top -p $(pgrep -d, $1) 2>/dev/null }

# open file in emacs as root
#E () { emacsclient -c -a 'emacs' "/sudo:root@localhost:$1" }

# pretty print json
pp-json () { cat - | python -mjson.tool }

# set and unset title of term
title () { export TITLE="=$1=" }
untitle () { unset TITLE }

# start and stop NixOS-VBox-VM
#nixos-start () { VBoxManage startvm NixOS --type headless }
#nixos-stop () { VBoxManage controlvm NixOS savestate }
#nixos-connect () { ssh -p 2222 localhost }

# create maildir-folders
mkmaildir () { if [[ $# -ne 1; ]]; then echo "usage: $0 DIR" && return 1; else mkdir -p $1/cur $1/new $1/tmp; fi }

# nix functions
#nix-search () { nix-env -I ~ -qaP \* | grep $1 }
nix-search () { nix-env -f "<nixpkgs>" -qaP \* | grep $1 }
nix-update () { nix-channel --update && nix-env -f ~/conf/packs.nix -uA }
nixos-update () { sudo nixos-rebuild switch --upgrade }

sec-mount () { sudo cryptsetup luksOpen $1 secure && sudo mount /dev/mapper/secure /mnt/secure }
sec-umount () { sudo umount /mnt/secure && sudo cryptsetup luksClose /dev/mapper/secure }
