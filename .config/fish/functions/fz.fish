function fz --description 'entry point for all the fuzziness glory'
	set --local cmd "fzf \
		--ansi \
		--cycle \
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
		--plain \
		--language json \
		--color always'

	function print_error
		set_color red; printf "$argv[1]\n"; set_color normal;
	end

	function print_dim
		set_color brblack; printf "$argv[1]\n"; set_color normal;
	end

	# commands starting with _fzf are from https://github.com/PatrickF1/fzf.fish
	switch $argv[1]

	case acpi-devices
		if not command -q acpi
			print_error 'acpi command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: acpi devices, like battery, adapter, thermal, cooling…\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		set --local device (acpi --everything | _fzf)
		if test -n "$device"
		end

	case azure-accounts
		if not command -q az
			print_error 'az command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: azure accounts using az\n'
			printf 'preview: azure account details\n'
			printf 'action: set default account and display resource groups\n'
			return
		end

		set --local account (az account list \
			| jq --raw-output '.[] | "\(.name)\u001f\(.user.name)\u001f(default:\(.isDefault))\u001f\(.id)"' \
			| awk -F \u001f '{printf "%s %s %s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $2, $3, $4}' \
			| _fzf --preview "az account show --subscription {-1} | $bat_json" \
			| awk '{print $NF}')
		if test -n "$account"
			az account set --subscription "$account"
			fz azure-resource-groups
		end

	case azure-container-registries
		if not command -q az
			print_error 'az command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: azure container registries using az\n'
			printf 'preview: azure container registry details\n'
			printf 'action: set default container registry and display container registry repositories\n'
			return
		end

		# prompt
		set --local account (az account show | jq --raw-output '"\(.user.name) at \(.name)"')
		set --local rg (az config get defaults.rg 2> /dev/null | jq --raw-output .value)

		set --local acr (az acr list \
			| jq --raw-output '.[] | "\(.name) \(.loginServer)\u001f\(.location)\u001f\(.id)"' \
			| awk -F \u001f '{printf "%s %s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $2, $3, $4}' \
			| _fzf \
				--prompt "$argv[1] ($account/$rg) ❯ " \
				--preview "az acr show --name {1} | $bat_json" \
			| awk '{print $2}')
		if test -n "$acr"
			az config set defaults.acr="$acr" 2> /dev/null
			fz azure-container-registry-repositories
		end

	case azure-container-registry-manifests
		if not command -q az
			print_error 'az command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: azure container registry manifests using az\n'
			printf 'preview: container registry manifest metadata\n'
			print_dim 'action: none'
			return
		end

		# prompt
		set --local registry (az config get defaults.acr 2> /dev/null | jq --raw-output .value)
		set --local repository (az config get defaults.acrepo 2> /dev/null | jq --raw-output .value)

		set --local manifest (az acr manifest list-metadata "$registry/$repository" 2> /dev/null \
			| jq --raw-output '.[] | .digest' \
			| _fzf --query '' \
				--prompt "$argv[1] ($registry/$repository) ❯ " \
				--preview "az acr manifest show-metadata $registry/$repository@{1} 2> /dev/null | $bat_json" \
			| awk '{print $1}')
		if test -n "$manifest"
		end

	case azure-container-registry-repositories
		if not command -q az
			print_error 'az command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: azure container registry repositories using az\n'
			printf 'preview: azure container registry repository\n'
			printf 'action: set default container registry repository and display container registry manifests\n'
			return
		end

		# prompt
		set --local registry (az config get defaults.acr 2> /dev/null | jq --raw-output .value)

		set --local repository (az acr repository list 2> /dev/null \
			| jq --raw-output '.[]' \
			| _fzf --query '' \
				--prompt "$argv[1] ($registry) ❯ " \
				--preview "az acr repository show --repository {1} 2> /dev/null | $bat_json" \
			| awk '{print $1}')
		if test -n "$repository"
			az config set defaults.acrepo="$repository" 2> /dev/null
			fz azure-container-registry-manifests
		end

	case azure-resource-groups
		if not command -q az
			print_error 'az command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: azure resource groups using az\n'
			printf 'preview: azure resources in this rg\n'
			printf 'action: set default resource group and display resources\n'
			return
		end

		# prompt
		set --local account (az account show | jq --raw-output '"\(.user.name) at \(.name)"')

		set --local rg (az group list \
			| jq --raw-output '.[] | "\(.name)\u001f\(.location)\u001f\(.id)"' \
			| awk -F \u001f '{printf "%s %s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $2, $3}' \
			| _fzf \
				--prompt "$argv[1] ($account) ❯ " \
				--preview 'az resource list --resource-group {1} | jq --raw-output ".[] | .name"' \
			| awk '{print $1}')
		if test -n "$rg"
			az config set defaults.rg="$rg" 2> /dev/null
			fz azure-resources "'$rg'"
		end

	case azure-resources
		if not command -q az
			print_error 'az command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: azure resources using az\n'
			printf 'preview: azure resource\n'
			print_dim 'action: none'
			return
		end

		# prompt
		set --local account (az account show | jq --raw-output '"\(.user.name) at \(.name)"')

		set --local resource (az resource list \
			| jq --raw-output '.[] | "\(.name)\u001f\(.type)\u001f\(.resourceGroup)"' \
			| awk -F \u001f '{printf "%s \x1b[38;2;98;114;164m%s %s\x1b[m\n", $1, $2, $3}' \
			| _fzf \
				--prompt "$argv[1] ($account) ❯ " \
				--preview "az resource show --name {1} --resource-type {-2} --resource-group {-1} | $bat_json")
		if test -n "$resource"
			set --local resource (string split ' ' "$resource")

			switch "$resource[2]"
			case "Microsoft.ContainerRegistry/registries"
				az config set defaults.acr="$resource[1]" 2> /dev/null
				fz azure-container-registry-repositories

			case "Microsoft.Storage/storageAccounts"
				az config set defaults.storageaccount="$resource[1]" 2> /dev/null
				fz azure-storage-containers

			end
		end

	case azure-storage-accounts
		if not command -q az
			print_error 'az command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: azure storage accounts using az\n'
			printf 'preview: azure storage account details\n'
			printf 'action: set default azure storage account and display containers\n'
			return
		end

		# prompt
		set --local account (az account show | jq --raw-output '"\(.user.name) at \(.name)"')

		set --local storageaccount (az storage account list \
			| jq --raw-output '.[] | "\(.name)\u001f\(.location)\u001f\(.id)"' \
			| awk -F \u001f '{printf "%s %s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $2, $3}' \
			| _fzf \
				--prompt "$argv[1] ($account) ❯ " \
				--preview "az storage account show --name {1} | $bat_json" \
			| awk '{print $1}')
		if test -n "$storageaccount"
			az config set defaults.storageaccount="$storageaccount" 2> /dev/null
			fz azure-storage-containers "'$storageaccount'"
		end

	case azure-storage-blobs
		if not command -q az
			print_error 'az command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: azure storage blobs using az\n'
			printf 'preview: blob details\n'
			print_dim 'action: none'
			return
		end

		# prompt
		set --local storageaccount (az config get defaults.storageaccount 2> /dev/null | jq --raw-output .value)
		set --local container (az config get defaults.storagecontainer 2> /dev/null | jq --raw-output .value)

		set --local blob (az storage blob list --account-name "$storageaccount" --container-name "$container" 2> /dev/null \
			| jq --raw-output '.[] | "\(.name)"' \
			| _fzf --query '' \
				--prompt "$argv[1] ($storageaccount/$container) ❯ " \
				--preview "az storage blob show --account-name="$storageaccount" --container-name="$container" --name {1} 2> /dev/null | $bat_json" \
			| awk '{print $1}')
		if test -n "$blob"
		end

	case azure-storage-containers
		if not command -q az
			print_error 'az command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: azure storage containers using az\n'
			printf 'preview: azure storage container details\n'
			printf 'action: set default azure storage container and display its blobs\n'
			return
		end

		# prompt
		set --local storageaccount (az config get defaults.storageaccount 2> /dev/null | jq --raw-output .value)

		set --local container (az storage container list --auth-mode login --account-name "$storageaccount" 2> /dev/null \
			| jq --raw-output '.[] | "\(.name)"' \
			| awk -F \u001f '{printf "%s\n", $1}' \
			| _fzf --query '' \
				--prompt "$argv[1] ($storageaccount) ❯ " \
				--preview "az storage container show --account-name="$storageaccount" --name {1} 2> /dev/null | $bat_json" \
			| awk '{print $1}')
		if test -n "$container"
			az config set defaults.storagecontainer="$container" 2> /dev/null
			fz azure-storage-blobs "'$container'"
		end

	case bins
		if test "$argv[2]" = "--help"
			printf 'list: system binaries\n'
			printf 'preview: which binary\n'
			printf 'action: launch binary\n'
			return
		end

		set --local bin (complete -C '' \
			| awk '{print $1}' \
			| _fzf --preview 'which {1}')
		if test -n "$bin"
			i3-msg --quiet "exec --no-startup-id $bin"
		end

	case browser-bookmarks
		if test "$argv[2]" = "--help"
			printf 'list: browser bookmarks\n'
			print_dim 'preview: none'
			printf 'action: open bookmark in default browser\n'
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
			print_dim 'preview: none'
			printf 'action: activate browser tab\n'
			printf 'action: alt-d delete browser tab\n'
			return
		end

		set --local action (firefoxctl tab list \
			| jq --raw-output '.[] | "\(.id) \(.lastAccessed)\t\(.title) \u001b[38;2;98;114;164m\(.url)\u001b[m"' \
			| _fzf --multi --expect alt-d \
			| awk '{print $1}')

		if test -n "$action"
			set --local verb_ids (string split ' ' "$action")
			set --local verb "$verb_ids[1]"
			set --local ids "$verb_ids[2..]"

			switch "$verb"

			case 'alt-d'
				firefoxctl tab delete $ids

			case '*'
				firefoxctl tab activate "$ids[1]"
			end
		end

	case deno-tasks
		if not test -e './deno.jsonc'
			print_error 'no deno.jsonc in current directory'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: deno tasks\n'
			print_dim 'preview: none'
			printf 'action: run deno task\n'
			return
		end

		set --local task (jq --raw-output \
			'.tasks | to_entries | .[] | "\(.key)\u001f\(.value)"' deno.jsonc \
			| awk -F \u001f '{printf "%s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $NF}' \
			| _fzf \
			| awk '{print $1}')
		if test -n "$task"
			deno task "$task"
		end

	case docker-accounts
		if test "$argv[2]" = "--help"
			printf 'list: docker accounts\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		jq .auths ~/.docker/config.json \
			| gron \
			| rg '.auth =' \
			| sed --expression 's/json\["\(.*\)"\].auth = "\(.*\)";/\1 \2/' \
			| teip -sf 2 -- base64 -d \
			| cut --delimiter ':' --fields 1 \
			| _fzf

	case docker-containers
		if not systemctl is-active docker > /dev/null
			print_error 'docker daemon is not active'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: docker containers\n'
			printf 'preview: docker container details\n'
			print_dim 'action: none'
			return
		end

		set --local container (docker container ls -a \
			| _fzf \
				--header-lines=1 \
				--preview "docker container inspect {1} | jq .[0] | $bat_json")

	case docker-images
		if not systemctl is-active docker > /dev/null
			print_error 'docker daemon is not active'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: docker images\n'
			printf 'preview: docker image details\n'
			print_dim 'action: none'
			return
		end

		set --local image (docker image ls \
			| _fzf \
				--header-lines=1 \
				--preview "docker image inspect {3} | jq .[0] | $bat_json")

	case docker-images-dangling
		if not systemctl is-active docker > /dev/null
			print_error 'docker daemon is not active'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: docker dangling images\n'
			printf 'preview: docker image details\n'
			print_dim 'action: none'
			return
		end

		set --local image (docker image ls --filter 'dangling=true' \
			| _fzf \
				--header-lines=1 \
				--preview "docker image inspect {3} | jq .[0] | $bat_json")

	case docker-networks
		if not systemctl is-active docker > /dev/null
			print_error 'docker daemon is not active'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: docker networks\n'
			printf 'preview: docker network details\n'
			print_dim 'action: none'
			return
		end

		set --local network (docker network ls \
			| _fzf \
				--header-lines=1 \
				--preview "docker network inspect {1} | jq .[0] | $bat_json")

	case docker-volumes
		if not systemctl is-active docker > /dev/null
			print_error 'docker daemon is not active'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: docker volumes\n'
			printf 'preview: docker volume details\n'
			print_dim 'action: none'
			return
		end

		set --local volume (docker volume ls \
			| _fzf \
				--header-lines=1 \
				--preview "docker volume inspect {2} | jq .[0] | $bat_json")

	case environment-variables
		if test "$argv[2]" = "--help"
			printf 'list: environment variables\n'
			printf 'preview: environment variable content and scope\n'
			print_dim 'action: none'
			return
		end

		_fzf_search_variables (set --show | psub) (set --names | psub)

	case files
		if test "$argv[2]" = "--help"
			printf 'list: files\n'
			printf 'preview: file or dir content\n'
			print_dim 'action: none'
			return
		end

		_fzf_search_directory

	case file-descriptors
		if test "$argv[2]" = "--help"
			printf 'list: files descriptors using lsfd\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		lsfd | _fzf --header-lines=1

	case fonts
		if not command -q fontpreview
			print_error 'fontpreview command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: fonts using fontpreview\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		fontpreview

	case git-branches
		if test "$argv[2]" = "--help"
			printf 'list: git branches\n'
			printf 'preview: git branch details\n'
			print_dim 'action: none'
			return
		end

		git branches --color=always | _fzf --preview 'git show {2} --color=always'

	case git-log
		if test "$argv[2]" = "--help"
			printf 'list: git log\n'
			printf 'preview: git commit details\n'
			print_dim 'action: none'
			return
		end

		_fzf_search_git_log

	case git-status
		if test "$argv[2]" = "--help"
			printf 'list: git status\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		_fzf_search_git_status

	case git-tags
		if test "$argv[2]" = "--help"
			printf 'list: git tags\n'
			printf 'preview: tagged commit details\n'
			print_dim 'action: none'
			return
		end

		git tags | _fzf --preview 'git show {1} --color=always'

	case github-repositories
		if not command -q gh
			print_error 'gh command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: github repositories using gh\n'
			printf 'preview: repository details\n'
			printf 'action: open repository in browser\n'
			return
		end

		set --local repo (gh repo list \
			| _fzf \
				--preview 'gh repo view {1}' | awk '{print $1}')
		if test -n "$repo"
			gh repo view --web "$repo"
		end

	case gpg-keys
		if not command -q gpg
			print_error 'gpg command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: git gpg keys\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		gpg --list-keys --with-colons \
			| rg uid \
			| awk -F ':' '{ print $8 " " $10 }' \
			| _fzf

	case i3-windows
		if not command -q i3-msg
			print_error 'i3-msg command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: i3 windows\n'
			print_dim 'preview: none'
			printf 'action: focus window\n'
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
		if not command -q i3-msg
			print_error 'i3-msg command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: i3 workspaces\n'
			print_dim 'preview: none'
			printf 'action: focus workspace\n'
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
			printf 'list: ip addresses\n'
			printf 'preview: ip address details\n'
			print_dim 'action: none'
			return
		end

		ip -oneline address \
			| _fzf \
				--preview 'ip address show {2}'

	case kakoune-sessions
		if not command -q kak
			print_error 'kak command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: kakoune sessions\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		kak -l | _fzf

	case linux-kernel-modules
		if test "$argv[2]" = "--help"
			printf 'list: linux kernel modules using lsmod\n'
			printf 'preview: modinfo\n'
			print_dim 'action: none'
			return
		end

		lsmod \
			| _fzf \
				--header-lines=1 \
				--preview 'modinfo {1}'

	case linux-namespaces
		if test "$argv[2]" = "--help"
			printf 'list: linux namespaces using lsns\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		lsns | _fzf --header-lines=1

	case man-pages
		if test "$argv[2]" = "--help"
			printf 'list: man pages\n'
			printf 'preview: man page details\n'
			print_dim 'action: none'
			return
		end

		man -k . | _fzf --preview 'man {1}'

	case media-types
		if test "$argv[2]" = "--help"
			printf 'list: media-types registered by IANA\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		cat /etc/mime.types | tail --lines +14 | _fzf

	case monitors
		if not command -q xrandr
			print_error 'xrandr command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: monitors using xrandr\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		xrandr --listmonitors | _fzf --header-lines=1

	case music-albums
		if not command -q mpc
			print_error 'mpc command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: music albums using mpc\n'
			printf 'preview: music album tracks\n'
			print_dim 'action: none'
			return
		end

		mpc list album | _fzf --preview 'mpc search album {}'

	case music-artists
		if not command -q mpc
			print_error 'mpc command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: music artists using mpc\n'
			printf 'preview: music artist tracks\n'
			print_dim 'action: none'
			return
		end

		mpc list artist | _fzf --preview 'mpc search artist {}'

	case music-dates
		if not command -q mpc
			print_error 'mpc command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: music dates using mpc\n'
			printf 'preview: music dates tracks\n'
			print_dim 'action: none'
			return
		end

		mpc list date | _fzf --preview 'mpc search date {}'

	case music-playlists
		if not command -q mpc
			print_error 'mpc command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: music playlists using mpc\n'
			printf 'preview: music playlist tracks\n'
			print_dim 'action: none'
			return
		end

		mpc lsplaylist | _fzf --preview 'mpc playlist {}'

	case network-ports
		if test "$argv[2]" = "--help"
			printf 'list: tcp and upd ports registered by IANA\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		cat /etc/services | tail --lines +3 | _fzf

	case npm-scripts
		if not test -e './package.json'
			print_error 'no package.json in current directory'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: npm scripts\n'
			print_dim 'preview: none'
			printf 'action: run npm script\n'
			return
		end

		set --local script (jq --raw-output \
			'.scripts | to_entries | .[] | "\(.key)\u001f\(.value)"' package.json \
			| awk -F \u001f '{printf "%s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $NF}' \
			| _fzf \
			| awk '{print $1}')
		if test -n "$script"
			npm run "$script"
		end

	case pacman-mirrors
		if test "$argv[2]" = "--help"
			printf 'list: pacman mirrors\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		cat /etc/pacman.d/mirrorlist | rg '^Server' | _fzf

	case pacman-packages
		if test "$argv[2]" = "--help"
			printf 'list: packages and their version\n'
			printf 'preview: package details\n'
			print_dim 'action: none'
			return
		end

		pacman --query \
			| awk '{printf "%s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $2}' \
			| _fzf \
				--preview 'pacman --query --info --list {1}'

	case pastel-colors
		if not command -q pastel
			print_error 'pastel command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: pastel colors\n'
			printf 'preview: colored example\n'
			print_dim 'action: none'
			return
		end

		pastel list | _fzf --preview 'pastel --force-color paint {} █▓▒░ pastel'

	case podman-pods
		if not command -q podman
			print_error 'podman command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: podman pods\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		podman pod ls | _fzf

	case processes
		if test "$argv[2]" = "--help"
			printf 'list: processes\n'
			print_dim 'preview: process details'
			print_dim 'action: none'
			return
		end

		_fzf_search_processes

	case pulseaudio-sinks
		if not command -q pactl
			print_error 'pactl command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: pulseaudio sinks\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		set --local sink (pactl -f json list sinks \
			| jq --raw-output '.[] | "\(.description)\u001f\(.active_port)"' \
			| awk -F \u001f '{printf "%s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $2}' \
			| _fzf \
			| awk '{print $1}')
		if test -n "$sink"
		end

	case pulseaudio-sources
		if not command -q pactl
			print_error 'pactl command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: pulseaudio sources\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		set --local src (pactl -f json list sources \
			| jq --raw-output '.[] | "\(.description)\u001f\(.active_port)"' \
			| awk -F \u001f '{printf "%s \x1b[38;2;98;114;164m%s\x1b[m\n", $1, $2}' \
			| _fzf \
			| awk '{print $1}')
		if test -n "$src"
		end

	case shell-abbreviations
		if test "$argv[2]" = "--help"
			printf 'list: shell abbreviations\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		abbr | _fzf

	case shell-aliases
		if test "$argv[2]" = "--help"
			printf 'list: shell aliases\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		alias | _fzf

	case shell-functions
		if test "$argv[2]" = "--help"
			printf 'list: shell functions\n'
			print_dim 'preview: none'
			print_dim 'action: none'
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
			printf 'list: history\n'
			printf 'preview: command details\n'
			print_dim 'action: none'
			return
		end

		_fzf_search_history

	case shell-key-bindings
		if test "$argv[2]" = "--help"
			printf 'list: shell key-bindings and associated functions\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		bind | _fzf

	case shell-prompts
		if test "$argv[2]" = "--help"
			printf 'list: shell prompts\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		fish_config prompt list | _fzf --preview 'fish_config prompt show {}'

	case shell-themes
		if test "$argv[2]" = "--help"
			printf 'list: shell themes\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		fish_config theme list | _fzf --preview 'fish_config theme show {}'

	case ssh-hosts
		if test "$argv[2]" = "--help"
			printf 'list: ssh hosts in ~/.ssh configs\n'
			print_dim 'preview: none'
			printf 'action: connect to ssh server\n'
			return
		end

		set --local configs (fd config ~/.ssh/)
		set --local host (cat $configs | rg 'Host ' | _fzf | awk '{print $2}')
		if test -n "$host"
			ssh "$host"
		end

	case ssh-keys
		if not command -q ssh-add
			print_error 'ssh-add command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: ssh keys SHA256 fingerprints\n'
			printf 'preview: full public key\n'
			print_dim 'action: none'
			return
		end

		ssh-add -l | _fzf --preview 'ssh-add -L | rg {3}' --preview-window wrap

	case starship-modules
		if not command -q starship
			print_error 'starship command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: starship modules\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		starship module --list | tail --lines +3 | _fzf

	case starship-presets
		if not command -q starship
			print_error 'starship command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: starship presets\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		starship preset --list | _fzf

	case systemd
		if not command -q sysz
			print_error 'sysz command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: systemd units and unit-files using sysz\n'
			printf 'preview: systemd unit details\n'
			print_dim 'action: none'
			return
		end

		sysz

	case top-level-domains
		if test "$argv[2]" = "--help"
			printf 'list: TLDs fetched from IANA.org\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		curl --silent 'https://data.iana.org/TLD/tlds-alpha-by-domain.txt' \
			| _fzf --header-lines=1

	case usb-devices
		if not command -q lsusb
			print_error 'lsusb command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: usb devices with lsusb\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		set --local device (lsusb | _fzf --preview 'lsusb --verbose -d {6} 2> /dev/null')

	case vscode-extensions
		if not command -q code
			print_error 'code command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: vscode extensions\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		set --local extension (code --list-extensions --show-versions 2> /dev/null | _fzf)
		if test -n "$extension"
		end

	case vscode-workspaces
		if not command -q code
			print_error 'code command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: vscode workspaces\n'
			printf 'preview: vscode workspace details\n'
			print_dim 'action: none'
			return
		end

		set --local dir "$HOME/projects/vscode-workspaces/"
		set --local workspace (fd .code-workspace "$dir" \
			| _fzf --preview 'bat {} --plain --language json --color always')
		if test -n "$workspace"
			code "$workspace"
		end

	case xinput-devices
		if not command -q xinput
			print_error 'xinput command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: xinput devices\n'
			printf 'preview: xinput device detail\n'
			print_dim 'action: none'
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
			azure-storage-accounts \
			azure-storage-blobs \
			azure-storage-containers \
			bins \
			browser-bookmarks \
			browser-tabs \
			deno-tasks \
			docker-accounts \
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
			pacman-mirrors \
			pacman-packages \
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
