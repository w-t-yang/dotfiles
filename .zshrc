setopt PROMPT_SUBST
export PROMPT='[T7@%~]$ '

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
#export PATH="/usr/local/opt/emacs-plus/bin/:$PATH"

eval `gdircolors ~/.dir_colors`
alias ls='gls --color=always'
alias dir='gdir --color=auto'
alias grep='grep --color=auto'
