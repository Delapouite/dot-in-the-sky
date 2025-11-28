function l --description 'eza shortcuts'
	set --local eza_cmd "eza \
		--classify \
		--color-scale all \
		--color-scale-mode fixed \
		--git \
		--links \
		--long \
		--octal-permissions"

	alias _eza="$eza_cmd"

	switch $argv[1]

	case 'la'
		_eza --all
	case 'ld'
		_eza --only-dirs
	case 'le'
		le
	case 'lf'
		_eza --only-files
	case 'lg'
		git ls-files . | _eza
	case 'lh'
		lh
	case 'll'
		ll
	case 'lt'
		lt
	case 'lu'
		git ls-files --others --exclude-standard . | _eza
	case '*'
		set --local commands \
			'la - all' \
			'ld - directories' \
			'le - envars' \
			'lf - files' \
			'lg - git' \
			'lh - hidden' \
			'll - visible' \
			'lt - tree' \
			'lu - untracked'

		set --local selected_command (printf '%s\n' $commands | fzf --prompt 'l ❯ ' --accept-nth 1 --bind 'backward-eof:abort')
		if test -n "$selected_command"
			l $selected_command
		end
	end
end
