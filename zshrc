
# set PATH
typeset -U path
path=(~/bin ~/.cabal/bin $path)

# load all files in my private function-directory
# there are some private used functions
fpath=(${HOME}/.zfunc "${fpath[@]}")

# set terminal
export TERM=xterm-256color
export EDITOR="emacsclient -a 'emacs' -c"

# set zsh options
setopt prompt_subst

# history
HISTFILE=${HOME}/.zsh_history
HISTSIZE=5000
SAVEHIST=10000

# modules
autoload -U colors && colors     # load color
autoload -Uz vcs_info            # vcs informations
autoload -U compinit && compinit # activates completion
autoload -Uz misc && misc        # my misc functions from ~/.zfunc

# stylings
zstyle ':vcs_info:*'    enable git darcs     # I use only git and darcs
zstyle ':vcs_info:*'    formats "(%s)-[%b]"
zstyle ':vcs_info:git*' check-for-changes true
zstyle ':vcs_info:git*' stagedstr '•'        # activate with %c
zstyle ':vcs_info:git*' unstagedstr '✖'      # activate with %u

# aliases
alias ls='ls --color'
alias ll='ls -l --color'
alias la='ls -a --color'
alias lla='ls -la --color'
alias e='emacsclient -a "emacs" -c'

# prompt
# calculate width of the terminal with $(echo $COLUMNS/2 | bc)
PROMPT="%(!.%{$fg_bold[red]%}.%{$fg_bold[green]%})[%n@%m %$(echo $COLUMNS/2 | bc)<..<%~%<<] "$'\n'"%# %{$reset_color%}%"
RPROMPT=$'%{$fg[magenta]%}$(cabal_sandbox) %{$fg[yellow]%}$(vcs_info_wrapper)%{$reset_color%}%'

# call function before drawing the prompts
precmd () {
    vcs_info
}

# wrapper for vcs_info
# configure style with zstyle ':vcs_info:*'
vcs_info_wrapper () {
    echo "$vcs_info_msg_0_"
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
