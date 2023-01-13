setopt PROMPT_SUBST

# GIT FUNCTIONS
git_branch() {
    git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}

export PROMPT='%~@$(git_branch) > '

# export CLICOLOR=1
# export LSCOLORS=ExFxBxDxCxegedabagacad

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
#export PATH="/usr/local/opt/emacs-plus/bin/:$PATH"

#After install coreutils
PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"

eval "$(pyenv init -)"

eval `gdircolors ~/.dir_colors`
alias ls='gls --color=always'
alias dir='gdir --color=auto'
alias grep='grep --color=auto'

alias ta="tmux attach"
alias tl="tmux list-sessions"
