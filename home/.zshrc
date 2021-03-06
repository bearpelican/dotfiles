# Path to your oh-my-zsh configuration
ZSH=$HOME/.oh-my-zsh

#zsh plugins
plugins=(git osx)

################################################################### zsh
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
unsetopt BG_NICE            # do NOT nice bg commands
setopt CORRECT              # command CORRECTION
setopt EXTENDED_HISTORY     # puts timestamps in the history
setopt ALL_EXPORT
setopt hist_ignore_all_dups # prevent's recording of duplicates
bindkey -e
zstyle :compinstall filename '~/.zshrc'
autoload -Uz compinit
compinit

HISTIGNORE="&:ls:ll:la:l.:pwd:exit:clear:clr:[bf]g"
SHOPT=`which shopt`
if [ -z SHOPT ]; then
    shopt -s histappend        # Append history instead of overwriting                                                                 
    shopt -s cdspell           # Correct minor spelling errors in cd command                                                           
    shopt -s dotglob           # includes dotfiles in pathname expansion                                                               
    shopt -s checkwinsize      # If window size changes, redraw contents                                                               
    shopt -s cmdhist           # Multiline commands are a single command in history.                                                   
    shopt -s extglob           # Allows basic regexps in bash.                                                                         
fi

################################################################### prompt
# This zsh function is called whenever changing directories and
# shows the current git branch in the prompt
findup() {
    arg="$1"
    if test -z "$arg"; then return 1; fi   
    while ! test -e "$arg"; do
        cd ..
        if test "$PWD" = "/"; then
            exit 1
        fi
    done
    echo $PWD/$arg
}

function precmd() {
    PROMPT=$'%{\e[0;32m%}%B'$HOSTNAME$'%b%{\e[0m%}:%{\e[0;35m%}%B%~%b%{\e[0m%}'
    RPS1=$'%{\e[0;33m%}%B(%D{%m-%d %H:%M})%b%{\e[0m%}'

    local _git _branch

    _git=`findup .git 2>/dev/null`
    if test -n "$_git"; then
        _branch=`sed -e 's,.*/,,' $_git/HEAD 2>/dev/null`
        RPS1=$'%{\e[0;36m%}'($_branch)" "$'%{\e[0m%}'$RPS1
    fi

    if test -n "$CONDA_DEFAULT_ENV"; then
        RPS1=$'%{\e[0;32m%}'($CONDA_DEFAULT_ENV)" "$RPS1
    else
        RPS1=$'%{\e[0;30m%}'("none")" "$RPS1
    fi

    export PROMPT=$PROMPT"%(!.#.>) "
    export RPS1=$RPS1
}

################################################################### env
EDITOR=jed
PATH=~/bin:$PATH:/usr/local/git/bin:/usr/X11/bin
if [ -z "$HOSTNAME" ]; then
    HOSTNAME=`hostname`
fi
LD_LIBRARY_PATH=lib:/usr/local/lib

# return if not interactive
[ -z "$PS1" ] && return

################################################################### osx
if [ `uname` = "Darwin" ]; then
    alias ls='ls -G'
    export LSCOLORS=dxfxcxdxbxegedabagacad
    bindkey "\e[3~" delete-char
elif [ "$TERM" != "dumb" ]; then
    if [ -e "~/.dir_colors" ]; then
        eval `dircolors ~/.dir_colors`
    fi
    alias ls='ls --color'
fi

################################################################### commands
alias ll='ls -l'
alias la='ls -A'
alias l='ls -a'
alias df='df -h'
alias du='du -h'
alias dus='du -h -s'
alias grep='grep --color'
alias egrep='egrep -n --color'
alias cd="pushd >/dev/null"
alias bd="popd >/dev/null"
alias g="git"
alias cdd='print "cd $(pwd)" | pbcopy'
if [ -f ~/.aliases ]; then
    source ~/.aliases
fi
bindkey '^[v' edit_command_line
bindkey '^[!' edit_command_output

################################################################### utilities
function freq() {
    sort $* | uniq -c | sort -rn;
}

# from zsh-users
edit_command_line () {
	# edit current line in $EDITOR
	local tmpfile=${TMPPREFIX:-/tmp/zsh}ecl$$
 
	print -R - "$PREBUFFER$BUFFER" >$tmpfile
	exec </dev/tty
	jed $tmpfile
	zle kill-buffer
	BUFFER=${"$(<$tmpfile)"/$PREBUFFER/}
	CURSOR=$#BUFFER
 
	command rm -f $tmpfile
	zle redisplay
}
zle -N edit_command_line
 
# ever used this :?
edit_command_output () {
	local output
	output=$(eval $BUFFER) || return
	BUFFER=$output
	CURSOR=0
}
zle -N edit_command_output

################################################################### completion

autoload -U compinit
compinit
bindkey "^?" backward-delete-char
bindkey '^[OH' beginning-of-line
bindkey '^[OF' end-of-line
bindkey '^[[5~' up-line-or-history
bindkey '^[[6~' down-line-or-history
bindkey "^r" history-incremental-search-backward
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^I' complete-word # complete on tab, leave expansion to _expand
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/6 )) numeric )'
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'

# Completion Styles

# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/6 )) numeric )'

# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions

# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

## add colors to processes for kill completion
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

zstyle ':completion:*:*:kill:*:processes' command 'ps --forest -A -o pid,user,cmd'
zstyle ':completion:*:processes-names' command 'ps axho command'

# complete hosts from known hosts of ssh, users from list
knownhosts=( ${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
zstyle ':completion:*' hosts $knownhosts
zstyle ':completion:*' users ashaw bearpelican

zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~' \
    '*?.old' '*?.pro'

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm apache bin daemon games gdm halt ident junkbust lp mail mailnull \
        named news nfsnobody nobody nscd ntp operator pcap postgres radvd \
        rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs avahi-autoipd\
        avahi backup messagebus beagleindex debian-tor dhcp dnsmasq fetchmail\
        firebird gnats haldaemon hplip irc klog list man cupsys postfix\
        proxy syslog www-data mldonkey sys snort

# SSH Completion
zstyle ':completion:*:yafc:*' tag-order \
   users hosts
zstyle ':completion:*:scp:*' tag-order \
   files users hosts
zstyle ':completion:*:scp:*' group-order \
   files all-files users hosts
zstyle ':completion:*:ssh:*' tag-order \
   users hosts
zstyle ':completion:*:ssh:*' group-order \
   hosts
zstyle '*' single-ignored show

# Aliases
export ALTERNATE_EDITOR=""
export EDITOR=emacsclient
alias e='emacsclient -c'
alias pycharm='/Applications/PyCharm\ CE.app/Contents/MacOS/pycharm'

# Paths
PATH=/usr/local/bin:$PATH
export PATH=$PATH:~/.local/bin
export PATH=$HOME/.cabal/bin:$PATH
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
export PATH="/Users/andrewshaw/miniconda3/bin:$PATH"
JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/andrewshaw/google-cloud-sdk/path.zsh.inc' ]; then source '/Users/andrewshaw/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/andrewshaw/google-cloud-sdk/completion.zsh.inc' ]; then source '/Users/andrewshaw/google-cloud-sdk/completion.zsh.inc'; fi
