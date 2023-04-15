function fz --description 'entry point for all the fuzziness glory'
	set --local cmd "fzf \
		--ansi \
		--reverse \
		--info inline \
		--no-separator \
		--preview-window=bottom \
		--prompt '$argv[1] ❯ '"

	if test -n "$argv[2]"
		set cmd "$cmd --query $argv[2]"
	end

	alias _fzf="$cmd"

	set --local bat_json 'bat \
		--plain
		--language json
		--color always'

	# commands starting with _fzf are from https://github.com/PatrickF1/fzf.fish
	switch $argv[1]

	case acpi-devices
		if test "$argv[2]" = "--help"
			if command -q acpi
				printf 'list: acpi devices, like battery, adapter, thermal, cooling…\n'
				set_color brblack; printf 'preview: none\n'
				set_color brblack; printf 'action: none\n'
			else
				set_color red; printf 'acpi command not found'
			end
			return
		end

		if command -q acpi
			acpi --everything | _fzf
		else
			printf 'acpi command not found'
			return 1
		end

	case azure-accounts
		if test "$argv[2]" = "--help"
			if command -q az
				printf 'list: azure accounts using az\n'
				printf 'preview: azure account details\n'
				printf 'action: set default account and display resource groups\n'
			else
				set_color red; printf 'az command not found'
			end
			return
		end

		if command -q az
			set --local account (az account list \
				| jq --raw-output '.[] | "\(.name)\u001f\(.user.name)\u001f(default:\(.isDefault))\u001f\(.id)"' \
				| awk -F \u001f '{printf "%s %s %s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $2, $3, $4}' \
				| _fzf --preview "az account show --subscription {-1} | $bat_json" \
				| awk '{print $NF}')
			if test -n "$account"
				az account set --subscription "$account"
				fz azure-resource-groups
			end
		else
			printf 'az command not found'
			return 1
		end

	case azure-container-registries
		if test "$argv[2]" = "--help"
			if command -q az
				printf 'list: azure container registries using az\n'
				printf 'preview: azure container registry details\n'
				printf 'action: set default container registry and display container registry repositories\n'
			else
				set_color red; printf 'az command not found'
			end
			return
		end

		if command -q az
			set --local acr (az acr list \
				| jq --raw-output '.[] | "\(.name) \(.loginServer)\u001f\(.location)\u001f\(.id)"' \
				| awk -F \u001f '{printf "%s %s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $2, $3, $4}' \
				| _fzf --preview "az acr show --name {1} | $bat_json" \
				| awk '{print $2}')
			if test -n "$acr"
				az config set defaults.acr="$acr"
				fz azure-container-registry-repositories
			end
		else
			printf 'az command not found'
			return 1
		end

	case azure-container-registry-manifests
		if test "$argv[2]" = "--help"
			if command -q az
				printf 'list: azure container registry manifests using az\n'
				set_color brblack; printf 'preview: container registry manifest metadata\n'
				set_color brblack; printf 'action: none\n'
			else
				set_color red; printf 'az command not found'
			end
			return
		end

		if command -q az
			set --local registry (az config get defaults.acr | jq --raw-output .value)
			set --local repository (az config get defaults.acrepo | jq --raw-output .value)
			set --local manifest (az acr manifest list-metadata "$registry/$repository" 2> /dev/null \
				| jq --raw-output '.[] | .digest' \
				| _fzf --query '' \
					--prompt "$argv[1] ($registry/$repository) ❯ " \
					--preview "az acr manifest show-metadata $registry/$repository@{1} 2> /dev/null | $bat_json" \
				| awk '{print $1}')
			if test -n "$manifest"
			end
		else
			printf 'az command not found'
			return 1
		end

	case azure-container-registry-repositories
		if test "$argv[2]" = "--help"
			if command -q az
				printf 'list: azure container registry repositories using az\n'
				printf 'preview: azure container registry repository\n'
				printf 'action: set default container registry repository and display container registry manifests\n'
			else
				set_color red; printf 'az command not found'
			end
			return
		end

		if command -q az
			set --local registry (az config get defaults.acr | jq --raw-output .value)
			set --local repository (az acr repository list 2> /dev/null \
				| jq --raw-output '.[]' \
				| _fzf --query '' \
					--prompt "$argv[1] ($registry) ❯ " \
					--preview "az acr repository show --repository {1} 2> /dev/null | $bat_json" \
				| awk '{print $1}')
			if test -n "$repository"
				az config set defaults.acrepo="$repository"
				fz azure-container-registry-manifests
			end
		else
			printf 'az command not found'
			return 1
		end

	case azure-resource-groups
		if test "$argv[2]" = "--help"
			if command -q az
				printf 'list: azure resource groups using az\n'
				printf 'preview: azure resources in this rg\n'
				printf 'action: set default resource group and display resources\n'
			else
				set_color red; printf 'az command not found'
			end
			return
		end

		if command -q az
			set --local rg (az group list \
				| jq --raw-output '.[] | "\(.name)\u001f\(.location)\u001f\(.id)"' \
				| awk -F \u001f '{printf "%s %s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $2, $3}' \
				| _fzf --preview 'az resource list --resource-group {1} | jq --raw-output ".[] | .name"' \
				| awk '{print $1}')
			if test -n "$rg"
				az config set defaults.rg="$rg"
				fz azure-resources "'$rg'"
			end
		else
			printf 'az command not found'
			return 1
		end

	case azure-resources
		if test "$argv[2]" = "--help"
			if command -q az
				printf 'list: azure resources using az\n'
				printf 'preview: azure resource\n'
				set_color brblack; printf 'action: none\n'
			else
				set_color red; printf 'az command not found'
			end
			return
		end

		if command -q az
			set --local resource (az resource list \
				| jq --raw-output '.[] | "\(.name)\u001f\(.type)\u001f\(.resourceGroup)"' \
				| awk -F \u001f '{printf "%s \x1b[38;2;98;114;164m%s %s\x1b[m\n", $1, $2, $3}' \
				| _fzf --preview "az resource show --name {1} --resource-type {-2} --resource-group {-1} | $bat_json")
		else
			printf 'az command not found'
			return 1
		end

	case bins
		if test "$argv[2]" = "--help"
			printf 'list: system binaries\n'
			set_color brblack; printf 'preview: none\n'
			set_color normal; printf 'action: launch binary\n'
			return
		end

		set --local bin (complete -C '' \
			| awk '{print $1}' \
			| _fzf)
		if test -n "$bin"
			i3-msg --quiet "exec --no-startup-id $bin"
		end

	case browser-bookmarks
		if test "$argv[2]" = "--help"
			printf 'list: browser bookmarks\n'
			set_color brblack; printf 'preview: none\n'
			set_color normal; printf 'action: open bookmark in default browser\n'
			return
		end

		cat ~/.local/share/browser-bookmarks/*.bookmarks \
			| rg -v '^#' | rg -v '^$' \
			| awk -F \u001f '{printf "%-12s \x1b[36m%s\x1b[m %s\n", $1, $2, $3}' \
			| _fzf \
			| sed 's#.*\(https*://\)#\1#' \
			| xargs xdg-open

	case browser-tabs
		if test "$argv[2]" = "--help"
			printf 'list: browser tabs\n'
			set_color brblack; printf 'preview: none\n'
			set_color normal; printf 'action: activate browser tab\n'
			return
		end

		set --local tab_id (firefoxctl tab list \
			| jq --raw-output '.[] | "\(.id) \(.lastAccessed)\t\(.title) \u001b[38;2;98;114;164m\(.url)\u001b[m"' \
			| _fzf \
			| awk '{print $1}')
		if test -n "$tab_id"
			firefoxctl tab activate "$tab_id"
		end

	case docker-containers
		if test "$argv[2]" = "--help"
			if systemctl is-active docker > /dev/null
				printf 'list: docker containers\n'
				printf 'preview: docker container\n'
				set_color brblack; printf 'action: none\n'
			else
				set_color red; printf 'docker daemon is not active'
			end
			return
		end

		if systemctl is-active docker > /dev/null
			set --local container (docker container ls -a \
				| _fzf \
					--header-lines=1 \
					--preview "docker container inspect {1} | jq .[0] | $bat_json")
		else
			printf 'docker daemon is not active'
			return 1
		end

	case docker-images
		if test "$argv[2]" = "--help"
			if systemctl is-active docker > /dev/null
				printf 'list: docker images\n'
				printf 'preview: docker image\n'
				set_color brblack; printf 'action: none\n'
			else
				set_color red; printf 'docker daemon is not active'
			end
			return
		end

		if systemctl is-active docker > /dev/null
			set --local image (docker image ls \
				| _fzf \
					--header-lines=1 \
					--preview "docker image inspect {3} | jq .[0] | $bat_json")
		else
			printf 'docker daemon is not active'
			return 1
		end

	case docker-images-dangling
		if test "$argv[2]" = "--help"
			if systemctl is-active docker > /dev/null
				printf 'list: docker dangling images\n'
				printf 'preview: docker image\n'
				set_color brblack; printf 'action: none\n'
			else
				set_color red; printf 'docker daemon is not active'
			end
			return
		end

		if systemctl is-active docker > /dev/null
			set --local image (docker image ls --filter 'dangling=true' \
				| _fzf \
					--header-lines=1 \
					--preview "docker image inspect {3} | jq .[0] | $bat_json")
		else
			printf 'docker daemon is not active'
			return 1
		end

	case docker-networks
		if test "$argv[2]" = "--help"
			if systemctl is-active docker > /dev/null
				printf 'list: docker networks\n'
				printf 'preview: docker network\n'
				set_color brblack; printf 'action: none\n'
			else
				set_color red; printf 'docker daemon is not active'
			end
			return
		end

		if systemctl is-active docker > /dev/null
			set --local network (docker network ls \
				| _fzf \
					--header-lines=1 \
					--preview "docker network inspect {1} | jq .[0] | $bat_json")
		else
			printf 'docker daemon is not active'
			return 1
		end

	case docker-volumes
		if test "$argv[2]" = "--help"
			if systemctl is-active docker > /dev/null
				printf 'list: docker volumes\n'
				printf 'preview: docker volume\n'
				set_color brblack; printf 'action: none\n'
			else
				set_color red; printf 'docker daemon is not active'
			end
			return
		end

		if systemctl is-active docker > /dev/null
			set --local volume (docker volume ls \
				| _fzf \
					--header-lines=1 \
					--preview "docker volume inspect {2} | jq .[0] | $bat_json")
		else
			printf 'docker daemon is not active'
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
				printf 'list: fonts using fontpreview\n'
				set_color brblack; printf 'preview: none\n'
				set_color brblack; printf 'action: none\n'
			else
				set_color red; printf 'fontpreview command not found'
			end
			return
		end

		if command -q fontpreview
			fontpreview
		else
			printf 'fontpreview command not found'
			return 1
		end

	case git-branches
		if test "$argv[2]" = "--help"
			printf 'list: git branches\n'
			printf 'preview: branch details\n'
			set_color brblack; printf 'action: none\n'
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

	case git-tags
		if test "$argv[2]" = "--help"
			printf 'list: git tags\n'
			printf 'preview: tagged commit details\n'
			set_color brblack; printf 'action: none\n'
			return
		end

		git tags | _fzf --preview 'git show {1} --color=always'

	case github-repositories
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		set --local repo (gh repo list \
			| _fzf \
				--preview 'gh repo view {1}' | awk '{print $1}')
		if test -n "$repo"
			gh repo view --web "$repo"
		end

	case gpg-keys
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		gpg --list-keys --with-colons \
			| rg uid \
			| awk -F ':' '{ print $8 " " $10 }' \
			| _fzf

	case i3-windows
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		set --local jq_filter '.. | objects | select(.window_type == "normal") | "\(.id) \(.window_properties.class): \(.name)"'
		set --local con_id (i3-msg -t get_tree \
			| jq --raw-output "$jq_filter" \
			| awk '{printf "%s \x1b[36m%s\x1b[m %s\n", $1, $2, $3}' \
			| _fzf --with-nth=2.. \
			| awk '{print $1}')
		if test -n "$con_id"
			i3-msg --quiet "[con_id=$con_id] focus"
		end

	case i3-workspaces
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		set --local workspace_id (i3-msg -t get_workspaces \
			| jq --raw-output '.[] .name' \
			| _fzf)
		if test -n "$workspace_id"
			i3-msg --quiet "workspace $workspace_id"
		end

	case ip-addresses
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		ip -oneline address \
			| _fzf \
				--preview 'ip address show {2}'

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

		lsmod \
			| _fzf \
				--header-lines=1 \
				--preview 'modinfo {1}' \
				--bind 'enter:execute(modinfo {1})'

	case linux-namespaces
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		lsns | _fzf --header-lines=1

	case man-pages
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		man -k . | _fzf --preview 'man {1}'

	case media-types
		if test "$argv[2]" = "--help"
			printf "list: media-types registered by IANA\n"
			set_color brblack; printf 'preview: none\n'
			set_color brblack; printf 'action: none\n'
			return
		end

		cat /etc/mime.types | tail --lines +14 | _fzf

	case monitors
		if test "$argv[2]" = "--help"
			printf 'list: monitors using xrandr\n'
			set_color brblack; printf 'preview: none\n'
			set_color brblack; printf 'action: none\n'
			return
		end

		xrandr --listmonitors | _fzf --header-lines=1

	case music-albums
		if test "$argv[2]" = "--help"
			printf 'list: music albums using mpc\n'
			printf 'preview: music album tracks\n'
			set_color brblack; printf 'action: none\n'
			return
		end

		mpc list album | _fzf --preview 'mpc search album {}'

	case music-artists
		if test "$argv[2]" = "--help"
			printf 'list: music artists using mpc\n'
			printf 'preview: music artist tracks\n'
			set_color brblack; printf 'action: none\n'
			return
		end

		mpc list artist | _fzf --preview 'mpc search artist {}'

	case music-dates
		if test "$argv[2]" = "--help"
			printf 'list: music dates using mpc\n'
			printf 'preview: music dates tracks\n'
			set_color brblack; printf 'action: none\n'
			return
		end

		mpc list date | _fzf --preview 'mpc search date {}'

	case music-playlists
		if test "$argv[2]" = "--help"
			printf 'list: music playlists using mpc\n'
			printf 'preview: music playlist tracks\n'
			set_color brblack; printf 'action: none\n'
			return
		end

		mpc lsplaylist | _fzf --preview 'mpc playlist {}'

	case network-ports
		if test "$argv[2]" = "--help"
			printf 'list: tcp and upd ports registered by IANA\n'
			set_color brblack; printf 'preview: none\n'
			set_color brblack; printf 'action: none\n'
			return
		end

		cat /etc/services | tail --lines +3 | _fzf

	case npm-scripts
		if test "$argv[2]" = "--help"
			printf 'list: npm scripts\n'
			set_color brblack; printf 'preview: none\n'
			set_color normal; printf 'action: run npm script\n'
			return
		end

		if test ! -e './package.json'
			printf 'no package.json in current directory\n'
		else
			set --local script (jq --raw-output \
				'.scripts | to_entries | .[] | "\(.key)\u001f\(.value)"' package.json \
				| awk -F \u001f '{printf "%s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $NF}' \
				| _fzf \
				| awk '{print $1}')
			if test -n "$script"
				npm run "$script"
			end
		end

	case pacman
		if test "$argv[2]" = "--help"
			printf 'list: packages and their version\n'
			printf 'preview: package details\n'
			set_color brblack; printf 'action: none\n'
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
				printf 'list: pastel colors\n'
				printf 'preview: colored example\n'
				set_color brblack; printf 'action: none\n'
			else
				set_color red; printf 'pastel command not found'
			end
			return
		end

		if command -q pastel
			pastel list | _fzf --preview 'pastel --force-color paint {} █▓▒░ pastel'
		else
			printf 'pastel command not found'
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

	case pulseaudio-sinks
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		if command -q pactl
			set --local sink (pactl -f json list sinks \
				| jq --raw-output '.[] | "\(.description)\u001f\(.active_port)"' \
				| awk -F \u001f '{printf "%s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $2}' \
				| _fzf \
				| awk '{print $1}')
			if test -n "$sink"
			end
		else
			printf 'pactl command not found'
			return 1
		end

	case pulseaudio-sources
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		if command -q pactl
			set --local src (pactl -f json list sources \
				| jq --raw-output '.[] | "\(.description)\u001f\(.active_port)"' \
				| awk -F \u001f '{printf "%s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $2}' \
				| _fzf \
				| awk '{print $1}')
			if test -n "$src"
			end
		else
			printf 'pactl command not found'
			return 1
		end

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

	case shell-key-bindings
		if test "$argv[2]" = "--help"
			printf 'list: fish key-bindings and associated functions\n'
			set_color brblack; printf 'preview: none\n'
			set_color brblack; printf 'action: none\n'
			return
		end

		bind | _fzf

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

	case ssh-hosts
		if test "$argv[2]" = "--help"
			printf 'list: ssh hosts in ~/.ssh configs\n'
			set_color brblack; printf 'preview: none\n'
			set_color normal; printf 'action: connect to ssh server\n'
			return
		end

		set --local configs (fd config ~/.ssh/)
		set --local host (cat $configs | rg 'Host ' | _fzf | awk '{print $2}')
		if test -n "$host"
			ssh "$host"
		end

	case ssh-keys
		if test "$argv[2]" = "--help"
			printf 'list: ssh keys SHA256 fingerprints\n'
			printf 'preview: full public key\n'
			set_color brblack; printf 'action: none\n'
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
			if command -q sysz
				printf 'list: systemd units and unit-files using sysz\n'
			else
				set_color red; printf 'sysz command not found'
			end
			return
		end

		if command -q sysz
			sysz
		else
			printf 'sysz command not found'
			return 1
		end

	case top-level-domains
		if test "$argv[2]" = "--help"
			printf 'list: TLDs fetched from IANA.org\n'
			set_color brblack; printf 'preview: none\n'
			set_color brblack; printf 'action: none\n'
			return
		end

		curl --silent 'https://data.iana.org/TLD/tlds-alpha-by-domain.txt' \
			| _fzf --header-lines=1

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
		set --local workspace (fd .code-workspace "$dir" \
			| _fzf --preview 'bat {} --plain --language json --color always')
		if test -n "$workspace"
			code "$workspace"
		end

	case xinput-devices
		if test "$argv[2]" = "--help"
			echo "no special help yet for $argv[1]"
			return
		end

		xinput --list --name-only \
			| _fzf \
				--preview 'xinput --list {}'

	# by default let the user discover and choose the input source
	case '*'
		set --local commands \
			acpi-devices \
			azure-accounts \
			azure-container-registries \
			azure-container-registry-repositories \
			azure-container-registry-manifests \
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
			git-tags \
			github-repositories \
			gpg-keys \
			i3-windows \
			i3-workspaces \
			ip-addresses \
			kakoune-sessions \
			linux-kernel-modules \
			linux-namespaces \
			man-pages \
			media-types \
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
			pulseaudio-sinks \
			pulseaudio-sources \
			shell-abbreviations \
			shell-aliases \
			shell-functions \
			shell-history \
			shell-key-bindings \
			shell-prompts \
			shell-themes \
			ssh-hosts \
			ssh-keys \
			starship-modules \
			starship-presets \
			systemd \
			top-level-domains \
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
