# Path to your oh-my-zsh installation.
export ZSH=/Users/linuxfish/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git autojump)

# User configuration

export PATH="/Users/linuxfish/.bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/linuxfish/Work/code/golang/bin"
# path for sml
#export PATH=/usr/local/Cellar/smlnj/110.78/bin:$PATH
# export MANPATH="/usr/local/man:$MANPATH"
# path for cs15112
#export PATH=/Users/linuxfish/playground/CMU15122/cc0/bin:$PATH
#gnu tools go first
#PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

source $ZSH/oh-my-zsh.sh

# Initialize rbenv, MUST be put under the PATH declaration, or the ruby version will still be system version
source ~/.profile 

# Shell Integration
source ~/.iterm2_shell_integration.`basename $SHELL`

# set android sdk home for eclipse buiding system (which uses ant)
#export ANDROID_SDK_HOME=/Users/linuxfish/Library/Android/sdk

# You may need to manually set your language environment
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

export EDITOR='vim'

# erase duplicated commands
export HISTCONTROL=erasedups

# for autojump
[ -f /usr/local/etc/profile.d/autojump.sh ] && . /usr/local/etc/profile.d/autojump.sh

# set GOPATH
export GOPATH=$HOME/Work/code/golang

# 配置 GOPROXY 环境变量
export GOPROXY="https://goproxy.cn,direct"
# https://medium.com/mabar/today-i-learned-fix-go-get-private-repository-return-error-reading-sum-golang-org-lookup-93058a058dd8
export GOPRIVATE=gitlab.luojilab.com

# ignore some specified commands
# not work in zsh
#export HISTIGNORE="pwd:ls:ll:" 
# this works
setopt HIST_IGNORE_SPACE

# # don't share command history among tabs
# unsetopt inc_append_history
# unsetopt share_history

# use vi mode
#set -o vi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#alias -s py=vi
alias zshconfig="vi ~/.zshrc"
alias .='source'
# use neo vim
alias vi='nvim'
alias vim='nvim'
alias rm='rm -i'
alias rake="noglob rake"
# tricks to make zsh not record those commands in history
alias ll=' ls -lh'
alias pwd=' pwd'
# ProxyChains-ng alias
alias pc="proxychains4"
alias gcc='/usr/local/bin/gcc-8'
alias config='/usr/bin/git --git-dir=/Users/linuxfish/.cfg/ --work-tree=/Users/linuxfish'
