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

if type -q direnv
	direnv hook fish | source
end

if type -q zoxide
	zoxide init fish | source
end
