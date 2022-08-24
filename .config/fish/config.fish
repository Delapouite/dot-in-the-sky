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

direnv hook fish | source
