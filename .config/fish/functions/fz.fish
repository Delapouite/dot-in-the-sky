function fz --description 'entry point for all the fuzziness glory'
	set --local commands \
		bins \
		browser-bookmarks \
		docker-containers \
		docker-images \
		docker-images-dangling \
		docker-networks \
		docker-volumes \
		files \
		git-log \
		git-status \
		i3-windows \
		i3-workspaces \
		ip-addresses \
		linux-kernel-modules \
		npm-scripts \
		pacman \
		podman-pods \
		processes \
		shell-functions \
		shell-history \
		ssh-keys \
		systemd \
		vscode-extensions \
		vscode-workspaces \
		xinput-devices

	alias _fzf="fzf --ansi --reverse --info inline --no-separator --preview-window=bottom --prompt '$argv[1] ❯ '"

	# commands starting with _fzf are from https://github.com/PatrickF1/fzf.fish
	switch $argv[1]

	case bins
		set --local bin (complete -C '' | awk '{print $1}' | _fzf)
		i3-msg --quiet "exec --no-startup-id $bin"

	case browser-bookmarks
		cat ~/.cache/browser-bookmarks \
			| awk -F \u001f '{printf "%-12s \x1b[36m%s\x1b[m %s\n", $1, $2, $3}' \
			| _fzf \
			| sed 's#.*\(https*://\)#\1#' \
			| xargs xdg-open

	case docker-containers
		if systemctl is-active docker > /dev/null
			docker container ls -a | tail --lines +2 | _fzf \
				--preview 'docker container inspect {1} | jq .[0] | bat --plain --language json --color always'
		else
			echo 'docker daemon is not active'
		end

	case docker-images
		if systemctl is-active docker > /dev/null
			docker image ls | tail --lines +2 | _fzf \
				--preview 'docker image inspect {3} | jq .[0] | bat --plain --language json --color always'
		else
			echo 'docker daemon is not active'
		end

	case docker-images-dangling
		if systemctl is-active docker > /dev/null
			docker image ls --filter 'dangling=true' | tail --lines +2 | _fzf \
				--preview 'docker image inspect {3} | jq .[0] | bat --plain --language json --color always'
		else
			echo 'docker daemon is not active'
		end

	case docker-networks
		if systemctl is-active docker > /dev/null
			docker network ls | tail --lines +2 | _fzf \
				--preview 'docker network inspect {1} | jq .[0] | bat --plain --language json --color always'
		else
			echo 'docker daemon is not active'
		end

	case docker-volumes
		if systemctl is-active docker > /dev/null
			docker volume ls | tail --lines +2 | _fzf \
				--preview 'docker volume inspect {2} | jq .[0] | bat --plain --language json --color always'
		else
			echo 'docker daemon is not active'
		end

	case files
		_fzf_search_directory

	case git-log
		_fzf_search_git_log

	case git-status
		_fzf_search_git_status

	case i3-windows
		set --local jq_filter '.. | objects | select(.window_type == "normal") | "\(.id) \(.window_properties.class): \(.name)"'
		set --local con_id (i3-msg -t get_tree | jq --raw-output "$jq_filter" | awk '{printf "%s \x1b[36m%s\x1b[m %s\n", $1, $2, $3}' | _fzf --with-nth=2.. | awk '{print $1}')
		if test -n "$con_id"
			i3-msg --quiet "[con_id=$con_id] focus"
		end

	case i3-workspaces
		set --local workspace_id (i3-msg -t get_workspaces | jq --raw-output '.[] .name' | _fzf)
		if test -n "$workspace_id"
			i3-msg --quiet "workspace $workspace_id"
		end

	case ip-addresses
		ip -oneline address | _fzf \
			--preview 'ip address show {2}' \

	case linux-kernel-modules
		lsmod | tail --lines +2 | _fzf \
			--preview 'modinfo {1}' \
			--bind 'enter:execute(modinfo {1})'

	case npm-scripts
		if test ! -e './package.json'
			echo 'no package.json in current directory'
		else
			set --local script (jq -r '.scripts | to_entries | .[] | "\(.key) \(.value)"' package.json | _fzf | awk '{print $1}')
			if test -n "$script"
				npm run "$script"
			end
		end

	case pacman
		pacman --query --quiet | _fzf \
			--preview 'pacman --query --info --list {}' \
			--bind 'enter:execute(pacman --query --info --list {} | bat)'

	case podman-pods
		podman pod ls | _fzf

	case processes
		_fzf_search_processes

	case shell-functions
		for name in (functions)
			set --local details (functions --details --verbose $name)
			set_color --bold cyan
			printf $name
			set_color normal
			printf " $details[-1]\n"
		end | _fzf

	case shell-history
		_fzf_search_history

	case ssh-keys
		ssh-add -l | _fzf

	case systemd
		sysz

	case vscode-extensions
		code --list-extensions --show-versions 2> /dev/null | _fzf

	case vscode-workspaces
		set --local dir "$HOME/code/workspaces/"
		set --local workspace (fd .code-workspace "$dir" | _fzf)
		if test -n "$workspace"
			code "$workspace"
		end

	case xinput-devices
		xinput --list --name-only | _fzf \
			--preview 'xinput --list {}'

	# by default let the user discover and choose the input source
	case '*'
		set --local selected_command (for command in $commands
			echo $command
		end | _fzf --prompt 'fz ❯ ')
		if test -n "$selected_command"
			fz $selected_command
		end
	end
end
