function fz --description 'entry point for all the fuzziness glory'
	alias _fzf="fzf --ansi --reverse --info inline --no-separator --preview-window=bottom --prompt '$argv[1] ❯ '"
	set --local bat_json 'bat --plain --language json --color always'

	# commands starting with _fzf are from https://github.com/PatrickF1/fzf.fish
	switch $argv[1]

	case acpi-devices
		acpi --everything | _fzf

	case azure-accounts
		az account list | jq --raw-output '.[] | "\(.name) \(.user.name) \(.tenantId)"' | _fzf

	case azure-resource-groups
		az group list | jq --raw-output '.[] | "\(.name) \(.location)"' \
			| _fzf --preview 'az resource list --resource-group {1} | jq ".[] | .name"'

	case azure-resources
		az resource list \
			| jq --raw-output '.[] | "\(.name)\u001f\(.type)\u001f\(.resourceGroup)"' \
			| awk -F \u001f '{printf "\x1b[36m%s\x1b[m %s %s\n", $1, $2, $3}' \
			| _fzf --preview 'az resource show --name {1} --resource-type {-2} --resource-group {-1}'

	case bins
		set --local bin (complete -C '' | awk '{print $1}' | _fzf)
		if test -n "$bin"
			i3-msg --quiet "exec --no-startup-id $bin"
		end

	case browser-bookmarks
		cat ~/.cache/browser-bookmarks \
			| awk -F \u001f '{printf "%-12s \x1b[36m%s\x1b[m %s\n", $1, $2, $3}' \
			| _fzf \
			| sed 's#.*\(https*://\)#\1#' \
			| xargs xdg-open

	case browser-tabs
		set --local tab_id (firefoxctl tab list \
			| jq --raw-output '.[] | "\(.id) \(.title) \(.url)"' \
			| _fzf \
			| awk '{print $1}')
		if test -n "$tab_id"
			firefoxctl tab activate "$tab_id"
		end

	case docker-containers
		if systemctl is-active docker > /dev/null
			docker container ls -a | _fzf \
				--header-lines=1 \
				--preview "docker container inspect {1} | jq .[0] | $bat_json"
		else
			echo 'docker daemon is not active'
		end

	case docker-images
		if systemctl is-active docker > /dev/null
			docker image ls | _fzf \
				--header-lines=1 \
				--preview "docker image inspect {3} | jq .[0] | $bat_json"
		else
			echo 'docker daemon is not active'
		end

	case docker-images-dangling
		if systemctl is-active docker > /dev/null
			docker image ls --filter 'dangling=true' | _fzf \
				--header-lines=1 \
				--preview "docker image inspect {3} | jq .[0] | $bat_json"
		else
			echo 'docker daemon is not active'
		end

	case docker-networks
		if systemctl is-active docker > /dev/null
			docker network ls | _fzf \
				--header-lines=1 \
				--preview "docker network inspect {1} | jq .[0] | $bat_json"
		else
			echo 'docker daemon is not active'
		end

	case docker-volumes
		if systemctl is-active docker > /dev/null
			docker volume ls | _fzf \
				--header-lines=1 \
				--preview "docker volume inspect {2} | jq .[0] | $bat_json"
		else
			echo 'docker daemon is not active'
		end

	case environment-variables
		_fzf_search_variables (set --show | psub) (set --names | psub)

	case files
		_fzf_search_directory

	case fonts
		fontpreview

	case git-branches
		git branches --color=always | _fzf --preview 'git show {2} --color=always'

	case git-log
		_fzf_search_git_log

	case git-status
		_fzf_search_git_status

	case github-repositories
		set --local repo (gh repo list | _fzf --preview 'gh repo view {1}' | awk '{print $1}')
		if test -n "$repo"
			gh repo view --web "$repo"
		end

	case gpg-keys
		gpg --list-keys --with-colons | rg uid | awk -F ':' '{ print $8 " " $10 }' | _fzf

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
		lsmod | _fzf \
			--header-lines=1 \
			--preview 'modinfo {1}' \
			--bind 'enter:execute(modinfo {1})'

	case man-pages
		man -k . | _fzf --preview 'man {1}'

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

	case pastel-colors
		pastel list | _fzf --preview 'pastel --force-color paint {} █▓▒░ pastel'

	case podman-pods
		podman pod ls | _fzf

	case processes
		_fzf_search_processes

	case shell-abbreviations
		abbr | _fzf

	case shell-aliases
		alias | _fzf

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

	case shell-prompts
		fish_config prompt list | _fzf --preview 'fish_config prompt show {}'

	case shell-themes
		fish_config theme list | _fzf --preview 'fish_config theme show {}'

	case ssh-keys
		ssh-add -l | _fzf

	case starship-modules
		starship module --list | tail --lines +3 | _fzf

	case starship-presets
		starship preset --list | _fzf

	case systemd
		sysz

	case usb-devices
		lsusb | _fzf --preview 'lsusb --verbose -d {6} 2> /dev/null'

	case vscode-extensions
		code --list-extensions --show-versions 2> /dev/null | _fzf

	case vscode-workspaces
		set --local dir "$HOME/projects/vscode-workspaces/"
		set --local workspace (fd .code-workspace "$dir" | _fzf --preview 'bat {} --plain --language json --color always')
		if test -n "$workspace"
			code "$workspace"
		end

	case xinput-devices
		xinput --list --name-only | _fzf \
			--preview 'xinput --list {}'

	case '--help'
		if test -n "$argv[2]"
			echo "no special help yet for $argv[2]"
		else
			echo "fz is a wrapper around fzf with predefined input sources"
		end

	# by default let the user discover and choose the input source
	case '*'
		set --local commands \
			acpi-devices \
			azure-accounts \
			azure-resource-groups \
			azure-resources \
			bins \
			browser-bookmarks \
			browser-tabs \
			docker-containers \
			docker-images \
			docker-images-dangling \
			docker-networks \
			docker-volumes \
			environment-variables \
			files \
			fonts \
			git-branches \
			git-log \
			git-status \
			github-repositories \
			gpg-keys \
			i3-windows \
			i3-workspaces \
			ip-addresses \
			linux-kernel-modules \
			man-pages \
			npm-scripts \
			pacman \
			pastel-colors \
			podman-pods \
			processes \
			shell-abbreviations \
			shell-aliases \
			shell-functions \
			shell-history \
			shell-prompts \
			shell-themes \
			ssh-keys \
			starship-modules \
			starship-presets \
			systemd \
			usb-devices \
			vscode-extensions \
			vscode-workspaces \
			xinput-devices

		set --local selected_command (for command in $commands
			echo $command
		end | _fzf --prompt 'fz ❯ ' --preview 'fz --help {}')
		if test -n "$selected_command"
			fz $selected_command
		end
	end
end
