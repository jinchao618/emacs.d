#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# eval "$(pyenv init --path)"
# eval "$(pyenv init -)"

alias ls='ls --color=auto'
alias la='ls -a'
alias ll='ls -l'
# alias julia='/opt/julia-1.4.0/bin/julia'
PS1='[\u@\h \W]\$ '
