# ~/.zshrc — minimal zsh config
# https://github.com/bearpelican/dotfiles

################################################################### path
typeset -U path                                 # auto-dedupe $PATH
[[ -d /opt/homebrew/bin ]] && path=(/opt/homebrew/bin $path)
[[ -d $HOME/.local/bin  ]] && path=($HOME/.local/bin $path)
export PATH

export EDITOR=vim
export VISUAL=$EDITOR

################################################################### history
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt EXTENDED_HISTORY HIST_IGNORE_ALL_DUPS HIST_REDUCE_BLANKS SHARE_HISTORY
unsetopt BG_NICE                                # do NOT nice bg commands

################################################################### prompt
# host:path on the left, git branch + time on the right
autoload -Uz vcs_info add-zsh-hook
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' formats '(%b) '
add-zsh-hook precmd vcs_info

setopt PROMPT_SUBST
PROMPT='%F{green}%B%m%b%f:%F{magenta}%B%~%b%f%(!.#.>) '
RPROMPT='%F{cyan}${vcs_info_msg_0_}%f%F{yellow}%B(%D{%m-%d %H:%M})%b%f'

################################################################### completion
autoload -Uz compinit && compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'   # case-insensitive
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

################################################################### keybindings
bindkey -e
bindkey '^r' history-incremental-search-backward
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^[v' edit-command-line                 # alt-v: edit line in $EDITOR

################################################################### aliases
if [[ $(uname) == Darwin ]]; then
    alias ls='ls -G'
    export LSCOLORS=dxfxcxdxbxegedabagacad
    alias cdd='print "cd $(pwd)" | pbcopy'   # macOS only
else
    alias ls='ls --color'
fi
alias ll='ls -l'
alias la='ls -A'
alias l='ls -a'
alias df='df -h'
alias du='du -h'
alias dus='du -h -s'
alias grep='grep --color'
alias g='git'

################################################################### local
# per-machine settings that shouldn't be committed
[[ -f ~/.aliases     ]] && source ~/.aliases
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
