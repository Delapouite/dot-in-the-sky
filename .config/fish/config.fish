set -x EDITOR kak
set -x VISUAL kak

alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'
alias ......='cd ../../../../../'

# plurals
alias builtins='builtin --names'

# ← arrow
bind \e\[1\;3D backward-kill-word
bind \e\[1\;4D backward-kill-bigword
# → arrow
bind \e\[1\;3C kill-word
bind \e\[1\;4C kill-bigword

# prompt
if type -q starship
	function starship_transient_prompt_func
		starship module character
	end

	starship init fish | source
	enable_transience
end

if type -q direnv
	direnv hook fish | source
end

if type -q zoxide
	zoxide init fish | source
end
