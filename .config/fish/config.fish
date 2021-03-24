set -x EDITOR kak
set -x VISUAL kak

alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'
alias ......='cd ../../../../../'

# prompt
if type -q starship
	starship init fish | source
end

bind --erase \cf
bind \ct __fzf_search_current_dir
