function fz --description 'entry point for all the fuzziness glory'
	set --local commands \
		bins \
		docker-containers \
		docker-images \
		docker-images-dangling \
		docker-networks \
		docker-volumes \
		files \
		git-log \
		git-status \
		i3-windows \
		linux-kernel-modules \
		npm-scripts \
		pacman \
		podman-pods \
		processes \
		shell-functions \
		shell-history \
		ssh-keys \
		systemd \
		vscode-workspaces

	set --local prompt "$argv[1] ❯ "

	# commands starting with _fzf are from https://github.com/PatrickF1/fzf.fish
	switch $argv[1]

	case bins
		set --local bin (complete -C '' | awk '{print $1}' | fzf --prompt $prompt)
		i3-msg --quiet "exec --no-startup-id $bin"

	case docker-containers
		docker container ls -a | tail --lines +2 | fzf --prompt $prompt

	case docker-images
		docker image ls | tail --lines +2 | fzf --prompt $prompt

	case docker-images-dangling
		docker image ls --filter 'dangling=true' | tail --lines +2 | fzf --prompt $prompt

	case docker-networks
		docker network ls | tail --lines +2 | fzf --prompt $prompt

	case docker-volumes
		docker volume ls | tail --lines +2 | fzf --prompt $prompt

	case files
		_fzf_search_directory

	case git-log
		_fzf_search_git_log

	case git-status
		_fzf_search_git_status

	case i3-windows
		set --local con_id (~/bin/i3-windows.js | fzf --prompt $prompt --with-nth=2.. | awk '{print $1}')
		i3-msg --quiet "[con_id=$con_id] focus"

	case linux-kernel-modules
		lsmod | tail --lines +2 | fzf \
			--prompt $prompt \
			--preview 'modinfo {1}' \
			--bind 'enter:execute(modinfo {1})'

	case npm-scripts
		set --local script (jq -r '.scripts | to_entries | .[] | "\(.key) \(.value)"' package.json | fzf --prompt $prompt | awk '{print $1}')
		npm run "$script"

	case pacman
		pacman --query --quiet | fzf \
			--prompt $prompt \
			--preview 'pacman --query --info --list {}' \
			--bind 'enter:execute(pacman --query --info --list {} | bat)'

	case podman-pods
		podman pod ls | fzf

	case processes
		_fzf_search_processes

	case shell-functions
		for name in (functions)
			set --local details (functions --details --verbose $name)
			set_color --bold cyan
			printf $name
			set_color normal
			printf " $details[-1]\n"
		end | fzf --ansi --prompt $prompt

	case shell-history
		_fzf_search_history

	case ssh-keys
		ssh-add -l | fzf --prompt $prompt

	case systemd
		sysz

	case vscode-workspaces
		set --local dir "$HOME/code/workspaces/"
		set --local workspace (fd .code-workspace "$dir" | fzf --prompt $prompt)
		code "$workspace"

	# by default let the user discover and choose the input source
	case '*'
		set --local selected_command (for command in $commands
			echo $command
		end | fzf --prompt 'fz ❯ ' --tac)
		fz $selected_command

	end
end
