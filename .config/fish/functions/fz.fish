function fz --description 'entry point for all the fuzziness glory'
	alias _fzf="fzf --ansi --reverse --info inline --no-separator --preview-window=bottom --prompt '$argv[1] ❯ '"
	set --local bat_json 'bat --plain --language json --color always'

	# commands starting with _fzf are from https://github.com/PatrickF1/fzf.fish
	switch $argv[1]

	case acpi-devices
		if test "$argv[2]" = "--help"
			if command -q acpi
				echo 'list all acpi devices'
			else
				set_color red; echo 'acpi command not found'
			end
			return
		end

		if command -q acpi
			acpi --everything | _fzf
		else
			echo 'acpi command not found'
			return 1
		end

	case azure-accounts
		if test "$argv[2]" = "--help"
			if command -q az
				echo 'list all azure accounts'
			else
				set_color red; echo 'az command not found'
			end
			return
		end

		if command -q az
			az account list | jq --raw-output '.[] | "\(.name) \(.user.name) \(.tenantId)"' | _fzf
		else
			echo 'az command not found'
			return 1
		end

	case azure-resource-groups
		if test "$argv[2]" = "--help"
			if command -q az
				echo 'list all azure resource groups'
			else
				set_color red; echo 'az command not found'
			end
			return
		end

		if command -q az
			az group list | jq --raw-output '.[] | "\(.name) \(.location)"' \
				| _fzf --preview 'az resource list --resource-group {1} | jq ".[] | .name"'
		else
			echo 'az command not found'
			return 1
		end

	case azure-resources
		if test "$argv[2]" = "--help"
			if command -q az
				echo 'list all azure resources'
			else
				set_color red; echo 'az command not found'
			end
			return
		end

		if command -q az
			az resource list \
				| jq --raw-output '.[] | "\(.name)\u001f\(.type)\u001f\(.resourceGroup)"' \
				| awk -F \u001f '{printf "%s \x1b[38;2;98;114;164m%s %s\x1b[m\n", $1, $2, $3}' \
				| _fzf --preview 'az resource show --name {1} --resource-type {-2} --resource-group {-1}'
		else
			echo 'az command not found'
			return 1
		end

	case bins
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		set --local bin (complete -C '' | awk '{print $1}' | _fzf)
		if test -n "$bin"
			i3-msg --quiet "exec --no-startup-id $bin"
		end

	case browser-bookmarks
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		cat ~/.cache/browser-bookmarks \
			| awk -F \u001f '{printf "%-12s \x1b[36m%s\x1b[m %s\n", $1, $2, $3}' \
			| _fzf \
			| sed 's#.*\(https*://\)#\1#' \
			| xargs xdg-open

	case browser-tabs
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		set --local tab_id (firefoxctl tab list \
			| jq --raw-output '.[] | "\(.id) \(.title) \(.url)"' \
			| _fzf \
			| awk '{print $1}')
		if test -n "$tab_id"
			firefoxctl tab activate "$tab_id"
		end

	case docker-containers
		if test "$argv[2]" = "--help"
			if systemctl is-active docker > /dev/null
				echo 'list all docker containers'
			else
				set_color red; echo 'docker daemon is not active'
			end
			return
		end

		if systemctl is-active docker > /dev/null
			docker container ls -a | _fzf \
				--header-lines=1 \
				--preview "docker container inspect {1} | jq .[0] | $bat_json"
		else
			echo 'docker daemon is not active'
			return 1
		end

	case docker-images
		if test "$argv[2]" = "--help"
			if systemctl is-active docker > /dev/null
				echo 'list all docker images'
			else
				set_color red; echo 'docker daemon is not active'
			end
			return
		end

		if systemctl is-active docker > /dev/null
			docker image ls | _fzf \
				--header-lines=1 \
				--preview "docker image inspect {3} | jq .[0] | $bat_json"
		else
			echo 'docker daemon is not active'
			return 1
		end

	case docker-images-dangling
		if test "$argv[2]" = "--help"
			if systemctl is-active docker > /dev/null
				echo 'list all docker dangling images'
			else
				set_color red; echo 'docker daemon is not active'
			end
			return
		end

		if systemctl is-active docker > /dev/null
			docker image ls --filter 'dangling=true' | _fzf \
				--header-lines=1 \
				--preview "docker image inspect {3} | jq .[0] | $bat_json"
		else
			echo 'docker daemon is not active'
			return 1
		end

	case docker-networks
		if test "$argv[2]" = "--help"
			if systemctl is-active docker > /dev/null
				echo 'list all docker networks'
			else
				set_color red; echo 'docker daemon is not active'
			end
			return
		end

		if systemctl is-active docker > /dev/null
			docker network ls | _fzf \
				--header-lines=1 \
				--preview "docker network inspect {1} | jq .[0] | $bat_json"
		else
			echo 'docker daemon is not active'
			return 1
		end

	case docker-volumes
		if test "$argv[2]" = "--help"
			if systemctl is-active docker > /dev/null
				echo 'list all docker volumes'
			else
				set_color red; echo 'docker daemon is not active'
			end
			return
		end

		if systemctl is-active docker > /dev/null
			docker volume ls | _fzf \
				--header-lines=1 \
				--preview "docker volume inspect {2} | jq .[0] | $bat_json"
		else
			echo 'docker daemon is not active'
			return 1
		end

	case environment-variables
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		_fzf_search_variables (set --show | psub) (set --names | psub)

	case files
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		_fzf_search_directory

	case file-descriptors
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		lsfd | _fzf --header-lines=1

	case fonts
		if test "$argv[2]" = "--help"
			if command -q fontpreview
				echo 'list all fonts'
			else
				set_color red; echo 'fontpreview command not found'
			end
			return
		end

		if command -q fontpreview
			fontpreview
		else
			echo 'fontpreview command not found'
			return 1
		end

	case git-branches
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		git branches --color=always | _fzf --preview 'git show {2} --color=always'

	case git-log
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		_fzf_search_git_log

	case git-status
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		_fzf_search_git_status

	case github-repositories
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		set --local repo (gh repo list | _fzf --preview 'gh repo view {1}' | awk '{print $1}')
		if test -n "$repo"
			gh repo view --web "$repo"
		end

	case gpg-keys
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		gpg --list-keys --with-colons | rg uid | awk -F ':' '{ print $8 " " $10 }' | _fzf

	case i3-windows
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		set --local jq_filter '.. | objects | select(.window_type == "normal") | "\(.id) \(.window_properties.class): \(.name)"'
		set --local con_id (i3-msg -t get_tree | jq --raw-output "$jq_filter" | awk '{printf "%s \x1b[36m%s\x1b[m %s\n", $1, $2, $3}' | _fzf --with-nth=2.. | awk '{print $1}')
		if test -n "$con_id"
			i3-msg --quiet "[con_id=$con_id] focus"
		end

	case i3-workspaces
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		set --local workspace_id (i3-msg -t get_workspaces | jq --raw-output '.[] .name' | _fzf)
		if test -n "$workspace_id"
			i3-msg --quiet "workspace $workspace_id"
		end

	case ip-addresses
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		ip -oneline address | _fzf \
			--preview 'ip address show {2}' \

	case kakoune-sessions
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		kak -l | _fzf

	case linux-kernel-modules
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		lsmod | _fzf \
			--header-lines=1 \
			--preview 'modinfo {1}' \
			--bind 'enter:execute(modinfo {1})'

	case man-pages
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		man -k . | _fzf --preview 'man {1}'

	case monitors
		if test "$argv[2]" = "--help"
			echo "list monitors using xrandr"
			return
		end

		xrandr --listmonitors | _fzf --header-lines=1

	case music-albums
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		mpc list album | _fzf --preview 'mpc search album {}'

	case music-artists
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		mpc list artist | _fzf --preview 'mpc search artist {}'

	case music-dates
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		mpc list date | _fzf --preview 'mpc search date {}'

	case music-playlists
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		mpc lsplaylist | _fzf --preview 'mpc playlist {}'

	case network-ports
		if test "$argv[2]" = "--help"
			echo "list: tcp and upd ports registered by IANA"
			return
		end

		cat /etc/services | tail --lines +3 | _fzf

	case npm-scripts
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		if test ! -e './package.json'
			echo 'no package.json in current directory'
		else
			set --local script (jq -r '.scripts | to_entries | .[] | "\(.key) \(.value)"' package.json | _fzf | awk '{print $1}')
			if test -n "$script"
				npm run "$script"
			end
		end

	case pacman
		if test "$argv[2]" = "--help"
			printf "list: packages and their version\npreview: package details"
			return
		end

		pacman --query \
			| awk '{printf "%s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $2}' \
			| _fzf \
				--preview 'pacman --query --info --list {1}' \
				--bind 'enter:execute(pacman --query --info --list {1} | bat)'

	case pastel-colors
		if test "$argv[2]" = "--help"
			if command -q pastel
				echo 'list all pastel colors'
			else
				set_color red; echo 'pastel command not found'
			end
			return
		end

		if command -q pastel
			pastel list | _fzf --preview 'pastel --force-color paint {} █▓▒░ pastel'
		else
			echo 'pastel command not found'
			return 1
		end

	case podman-pods
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		podman pod ls | _fzf

	case processes
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		_fzf_search_processes

	case shell-abbreviations
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		abbr | _fzf

	case shell-aliases
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		alias | _fzf

	case shell-functions
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		for name in (functions)
			set --local details (functions --details --verbose $name)
			set_color --bold cyan
			printf $name
			set_color normal
			printf " $details[-1]\n"
		end | _fzf

	case shell-history
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		_fzf_search_history

	case shell-prompts
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		fish_config prompt list | _fzf --preview 'fish_config prompt show {}'

	case shell-themes
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		fish_config theme list | _fzf --preview 'fish_config theme show {}'

	case ssh-keys
		if test "$argv[2]" = "--help"
			printf "list: ssh keys SHA256 fingerprints\npreview: full public key"
			return
		end

		ssh-add -l | _fzf --preview 'ssh-add -L | rg {3}' --preview-window wrap

	case starship-modules
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		starship module --list | tail --lines +3 | _fzf

	case starship-presets
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		starship preset --list | _fzf

	case systemd
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		sysz

	case usb-devices
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		lsusb | _fzf --preview 'lsusb --verbose -d {6} 2> /dev/null'

	case vscode-extensions
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		code --list-extensions --show-versions 2> /dev/null | _fzf

	case vscode-workspaces
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		set --local dir "$HOME/projects/vscode-workspaces/"
		set --local workspace (fd .code-workspace "$dir" | _fzf --preview 'bat {} --plain --language json --color always')
		if test -n "$workspace"
			code "$workspace"
		end

	case xinput-devices
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		xinput --list --name-only | _fzf \
			--preview 'xinput --list {}'

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
			file-descriptors \
			fonts \
			git-branches \
			git-log \
			git-status \
			github-repositories \
			gpg-keys \
			i3-windows \
			i3-workspaces \
			ip-addresses \
			kakoune-sessions \
			linux-kernel-modules \
			man-pages \
			monitors \
			music-albums \
			music-artists \
			music-dates \
			music-playlists \
			network-ports \
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

		set --local selected_command (printf '%s\n' $commands | _fzf --prompt 'fz ❯ ' --preview 'fz {} --help')
		if test -n "$selected_command"
			fz $selected_command
		end
	end
end
