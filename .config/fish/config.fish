set -x EDITOR kak
set -x VISUAL kak
# XDG user directories
set -x GOBIN "$HOME/.local/bin"
set -x FZF_DEFAULT_OPTS_FILE "$HOME/.config/fzf/config"
set -x NPM_CONFIG_USERCONFIG "$HOME/.config/npm/config"

alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'
alias ......='cd ../../../../../'

# plurals
alias builtins='builtin --names'

# curl
abbr --add --position anywhere -- --jwt '--header "Authorization: Bearer $JWT"'

# docker / podman
abbr --add --position anywhere -- --transient '--rm --interactive --tty'

# use fish_key_reader

# ctrl+backspace
bind \b backward-kill-bigword

# ← arrow
bind \e\[1\;3D backward-kill-word
bind \e\[1\;4D backward-kill-bigword
# → arrow
bind \e\[1\;3C kill-word
bind \e\[1\;4C kill-bigword

fish_add_path ~/.local/bin

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

if test -e /opt/asdf-vm/asdf.fish
	# source /opt/asdf-vm/asdf.fish
end

if status is-interactive && type -q atuin
	atuin init fish | source
end

if type -q mise
	mise activate fish | source
end
