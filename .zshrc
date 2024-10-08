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
plugins=(git zsh-autosuggestions)

# User configuration

export PATH="/Users/linuxfish/.bin:/opt/homebrew/bin:/Users/linuxfish/.cargo/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/linuxfish/Work/code/golang/bin"
# jdk
export PATH=/Library/Java/JavaVirtualMachines/adoptopenjdk-11.jdk/Contents/Home/bin:$PATH
# scala
export PATH=/opt/homebrew/opt/scala@2.12/bin:$PATH

# lib
export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"

# # for EAF 
# # https://github.com/emacs-eaf/emacs-application-framework
# export PATH=/opt/homebrew/opt/python@3.9/libexec/bin:$PATH
# export PATH=/Users/linuxfish/Library/Python/3.9/bin:$PATH

# path for sml
#export PATH=/usr/local/Cellar/smlnj/110.78/bin:$PATH
# export MANPATH="/usr/local/man:$MANPATH"
# path for cs15112
#export PATH=/Users/linuxfish/playground/CMU15122/cc0/bin:$PATH
#gnu tools go first
#PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

# for cross-compile in rust
export CC_x86_64_unknown_linux_gnu=x86_64-unknown-linux-gnu-gcc
export CXX_x86_64_unknown_linux_gnu=x86_64-unknown-linux-gnu-g++
export AR_x86_64_unknown_linux_gnu=x86_64-unknown-linux-gnu-ar
export CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_LINKER=x86_64-unknown-linux-gnu-gcc
export TARGET_CC=x86_64-linux-gnu-gcc

# homebrew 国内镜像
# https://mirrors.ustc.edu.cn/help/homebrew-bottles.html
export HOMEBREW_CORE_GIT_REMOTE="https://mirrors.ustc.edu.cn/homebrew-core.git"
export HOMEBREW_BOTTLE_DOMAIN="https://mirrors.ustc.edu.cn/homebrew-bottles"

source $ZSH/oh-my-zsh.sh

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

export EDITOR='nvim'

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

# fzf
# rg respects git ignore files
export FZF_DEFAULT_COMMAND='rg --files --hidden'

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
alias config='/usr/bin/git --git-dir=/Users/linuxfish/.cfg/ --work-tree=/Users/linuxfish'

# pyenv
# https://github.com/pyenv/pyenv#set-up-your-shell-environment-for-pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
