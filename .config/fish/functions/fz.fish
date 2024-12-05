function fz --description 'entry point for all the fuzziness glory'
	set --local fzf_cmd "fzf \
		--ansi \
		--cycle \
		--reverse \
		--info inline \
		--no-separator \
		--preview-window=bottom \
		--prompt '$argv[1] ❯ '"

	if test -n "$argv[2]"
		set fzf_cmd "$fzf_cmd --query $argv[2]"
	end

	alias _fzf="$fzf_cmd"
	alias _awk="awk --field-separator \u001f"
	alias _jq="jq --raw-output"

	set --local bat_json 'bat \
		--plain \
		--language json \
		--color always'

	# colors

	set --local grey '\x1b[38;2;173;178;203m'
	set --local comment '\x1b[38;2;98;114;164m'
	set --local reset '\x1b[m'

	set --local awk_dim2 \
		"{printf \"%s $comment%s$reset\n\", \$1, \$2}"
	set --local awk_dim3 \
		"{printf \"%s $grey%s$reset $comment%s$reset\n\", \$1, \$2, \$3}"
	set --local awk_dim4 \
		"{printf \"%s %s $grey%s$reset $comment%s$reset\n\", \$1, \$2, \$3, \$4}"
	set --local awk_dim5 \
		"{printf \"%s %s $grey%s$reset $comment%s %s$reset\n\", \$1, \$2, \$3, \$4, \$5}"

	function print_info
		printf "$argv[1]: "
		set_color cyan; printf "$argv[2]\n"; set_color normal;
	end

	function print_error
		set_color red; printf "$argv[1]\n"; set_color normal;
	end

	function print_dim
		set_color brblack; printf "$argv[1]\n"; set_color normal;
	end

	function print_disabled
		set_color brblack; printf "$argv[1]"; set_color normal;
	end

	set --local tmpdir "/tmp/fz/$argv[1]"

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

		set --local choice (acpi --everything | _fzf)

	case asdf-plugins
		if not command -q asdf
			print_error 'asdf command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: asdf plugins\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		set --local choice (asdf plugin list | _fzf)

	case 'azure-*'
		if not command -q az
			print_error 'az command not found'
			return 1
		end

		# header or prompt
		set --local account (az account show | _jq '"\(.user.name) at \(.name)"')
		set --local rg (az config get defaults.rg 2> /dev/null | _jq .value)

		function print_azure_help
			set --local account (az account show | _jq '"\(.user.name) at \(.name)"')
			set --local rg (az config get defaults.rg 2> /dev/null | _jq .value)
			print_info identity "$account"
			print_info resource-group "$rg"
		end

		switch $argv[1]

		case azure-accounts
			if test "$argv[2]" = "--help"
				print_azure_help
				printf 'list: azure accounts using az\n'
				printf 'preview: azure account details\n'
				printf 'action: set default account and display its resource groups\n'
				return
			end

			set --local choice (az account list \
				| _jq '.[] | "\(.name)\u001f\(.user.name)\u001f(default:\(.isDefault))\u001f\(.id)"' \
				| _awk "$awk_dim4" \
				| _fzf --preview "az account show --subscription {-1} | $bat_json" \
				| awk '{print $NF}')

			if test -n "$choice"
				az account set --subscription "$choice"
				fz azure-resource-groups
			end

		case azure-appservice-functionapps
			if test "$argv[2]" = "--help"
				print_azure_help
				printf 'list: azure app service functionapp using az\n'
				printf 'preview: azure app service functionapp details\n'
				print_dim 'action: none'
				return
			end

			set --local choice (az functionapp list \
				| _jq '.[] | "\(.name)\u001f\(.resourceGroup)\u001f\(.location)\u001f\(.kind)\u001f\(.id)"' \
				| _awk "$awk_dim5" \
				| _fzfa \
					--header "$account" \
					--preview "az functionapp show --name {1} --resource-group {2} | $bat_json" \
				| awk '{print $1}')

			if test -n "$choice"
				az config set defaults.functionapp="$choice" 2> /dev/null
				fz azure-functions
			end

		case azure-appservice-plans
			if test "$argv[2]" = "--help"
				print_azure_help
				printf 'list: azure app service plans using az\n'
				printf 'preview: azure app service plan details\n'
				print_dim 'action: none'
				return
			end

			set --local choice (az appservice plan list \
				| _jq '.[] | "\(.name)\u001f\(.resourceGroup)\u001f\(.location)\u001f\(.kind)\u001f\(.id)"' \
				| _awk "$awk_dim5" \
				| _fzf \
					--header "$account" \
					--preview "az appservice plan show --name {1} --resource-group {2} | $bat_json")

		case azure-appservice-webapps
			if test "$argv[2]" = "--help"
				print_azure_help
				printf 'list: azure app service webapps using az\n'
				printf 'preview: azure app service webapp details\n'
				print_dim 'action: none'
				return
			end

			set --local choice (az webapp list \
				| _jq '.[] | "\(.name)\u001f\(.resourceGroup)\u001f\(.location)\u001f\(.kind)\u001f\(.id)"' \
				| _awk "$awk_dim5" \
				| _fzf \
					--header "$account" \
					--preview "az webapp show --name {1} --resource-group {2} | $bat_json")

		case azure-container-registries
			if test "$argv[2]" = "--help"
				print_azure_help
				printf 'list: azure container registries using az\n'
				printf 'preview: azure container registry details\n'
				printf 'action: set default container registry and display container registry repositories\n'
				return
			end

			set --local choice (az acr list \
				| _jq '.[] | "\(.name) \(.loginServer)\u001f\(.location)\u001f\(.id)"' \
				| _awk "$awk_dim3" \
				| _fzf \
					--header "$account" \
					--preview "az acr show --name {1} | $bat_json" \
				| awk '{print $2}')

			if test -n "$choice"
				az config set defaults.acr="$choice" 2> /dev/null
				fz azure-container-registry-repositories
			end

		case azure-container-registry-manifests
			# prompt
			set --local registry (az config get defaults.acr 2> /dev/null | _jq .value)
			set --local repository (az config get defaults.acrepo 2> /dev/null | _jq .value)

			if test "$argv[2]" = "--help"
				print_azure_help
				print_info registry "$registry"
				print_info repository "$repository"
				printf 'list: azure container registry manifests using az\n'
				printf 'preview: container registry manifest metadata\n'
				print_dim 'action: none'
				return
			end

			set --local choice (az acr manifest list-metadata "$registry/$repository" --orderby time_desc 2> /dev/null \
				| _jq '.[] | "\(.digest)\u001f\(.lastUpdateTime)\u001f\(.imageSize)"' \
				| _awk "$awk_dim3" \
				| _fzf --query '' \
					--prompt "$argv[1] ($registry/$repository) ❯ " \
					--header "$account" \
					--preview "az acr manifest show-metadata $registry/$repository@{1} 2> /dev/null | $bat_json" \
				| awk '{print $1}')

		case azure-container-registry-repositories
			# prompt
			set --local registry (az config get defaults.acr 2> /dev/null | _jq .value)

			if test "$argv[2]" = "--help"
				print_azure_help
				print_info registry "$registry"
				printf 'list: azure container registry repositories using az\n'
				printf 'preview: azure container registry repository\n'
				printf 'action: set default container registry repository and display container registry manifests\n'
				return
			end

			set --local choice (az acr repository list 2> /dev/null \
				| _jq '.[]' \
				| _fzf --query '' \
					--prompt "$argv[1] ($registry) ❯ " \
					--header "$account" \
					--preview "az acr repository show --repository {1} 2> /dev/null | $bat_json" \
				| awk '{print $1}')

			if test -n "$choice"
				az config set defaults.acrepo="$choice" 2> /dev/null
				fz azure-container-registry-manifests
			end

		case azure-event-hub-namespaces
			if test "$argv[2]" = "--help"
				print_azure_help
				printf 'list: azure event-hub namespaces using az\n'
				printf 'preview: azure event-hub namespace details\n'
				printf 'action: ^ - fz azure-resources\n'
				return
			end

			set --local choice (az eventhubs namespace list --resource-group "$rg" \
				| _jq '.[] | "\(.name)\u001f\(.location)\u001f\(.id)"' \
				| _awk "$awk_dim3" \
				| _fzf \
					--header "$account" \
					--expect ^ \
					--preview "az eventhubs namespace show --name {1} | $bat_json")

			if test -n "$choice"
				set --local verb_ids (string split ' ' "$choice")
				set --local eventhubns $verb_ids[2]

				switch $verb_ids[1]

				case '^'
					fz azure-resources

				case '*'
					az config set defaults.eventhubns="$eventhubns" 2> /dev/null

				end
			end

		case azure-event-hubs
			set --local eventhubns (az config get defaults.eventhubns 2> /dev/null | _jq .value)

			if test "$argv[2]" = "--help"
				print_azure_help
				print_info "event-hub namespace" "$eventhubns"
				printf 'list: azure event-hubs using az\n'
				printf 'preview: azure event-hub details\n'
				printf 'action: ^ - fz azure-event-hub-namespaces\n'
				return
			end

			set --local choice (az eventhubs eventhub list --resource-group "$rg" --namespace-name "$iothub" \
				| _jq '.[] | "\(.name)\u001f\(.location)\u001f\(.id)"' \
				| _awk "$awk_dim3" \
				| _fzf \
					--prompt "$argv[1] ($eventhubns) ❯ " \
					--header "$account" \
					--expect ^ \
					--preview "az eventhubs eventhub show --name {1} | $bat_json")

			if test -n "$choice"
				set --local verb_ids (string split ' ' "$choice")

				switch $verb_ids[1]

				case '^'
					fz azure-event-hub-namespaces

				case '*'
				end
			end

		case azure-extensions
			if test "$argv[2]" = "--help"
				print_azure_help
				printf 'list: az extensions\n'
				printf 'preview: az extension detail\n'
				print_dim 'action: none'
				return
			end

			set --local choice (az extension list-available \
				| _jq '.[] | "\(.name)\u001f\(.installed)\u001f\(.version)"' \
				| _awk "$awk_dim3" \
				| _fzf \
					--header "$account" \
					--expect ^ \
					--preview "az extension show --name {1} | $bat_json")

		case azure-functions
			set --local functionapp (az config get defaults.functionapp 2> /dev/null | _jq .value)

			if test "$argv[2]" = "--help"
				print_azure_help
				print_info functionapp "$functionapp"
				printf 'list: azure functions using az\n'
				print_dim 'preview: none'
				print_dim 'action: none'
				return
			end

			set --local choice (az functionapp function list --name "$functionapp" --resource-group "$rg" \
				| _jq '.[] | "\(.name)\u001f\(.resourceGroup)\u001f\(.location)\u001f\(.kind)\u001f\(.id)"' \
				| _awk "$awk_dim5" \
				| _fzf \
					--prompt "$argv[1] ($functionapp) ❯ " \
					--header "$account")

		case azure-iot-dps
			if test "$argv[2]" = "--help"
				print_azure_help
				printf 'list: azure iot-dps using az\n'
				printf 'preview: azure iot-dps details\n'
				printf 'action: ^ - fz azure-resources\n'
				printf 'action: ctrl-alt-c - fz azure-iot-dps-certificates\n'
				return
			end

			set --local choice (az iot dps list \
				| _jq '.[] | "\(.name)\u001f\(.location)\u001f\(.id)"' \
				| _awk "$awk_dim3" \
				| _fzf \
					--header "$account" \
					--expect ^ \
					--expect ctrl-alt-c \
					--expect ctrl-alt-g \
					--preview "az iot dps show --name {1} | $bat_json")

			if test -n "$choice"
				set --local verb_ids (string split ' ' "$choice")
				set --local iotdps $verb_ids[2]

				switch $verb_ids[1]

				case '^'
					fz azure-resources

				case 'ctrl-alt-c'
					az config set defaults.iotdps="$iotdps" 2> /dev/null
					fz azure-iot-dps-certificates

				case 'ctrl-alt-g'
					az config set defaults.iotdps="$iotdps" 2> /dev/null
					fz azure-iot-dps-groups

				case '*'
					az config set defaults.iotdps="$iotdps" 2> /dev/null

				end
			end

		case azure-iot-dps-certificates
			set --local iotdps (az config get defaults.iotdps 2> /dev/null | _jq .value)

			if test "$argv[2]" = "--help"
				print_azure_help
				print_info iotdps "$iotdps"
				printf 'list: azure iot-dps certificates using az\n'
				printf 'preview: azure iot-dps certificate\n'
				printf 'action: ^ - fz azure-iot-dps\n'
				return
			end

			set --local choice (az iot dps certificate list --dps-name "$iotdps" \
				| _jq '.value | .[].name' \
				| _awk "$awk_dim3" \
				| _fzf \
					--prompt "$argv[1] ($iotdps) ❯ " \
					--header "$account" \
					--expect ^ \
					--preview "az iot dps certificate show --dps-name $iotdps --certificate-name {1} | $bat_json")

			if test -n "$choice"
				set --local verb_ids (string split ' ' "$choice")

				switch $verb_ids[1]

				case '^'
					fz azure-iot-dps

				case '*'
				end
			end

		case azure-iot-dps-groups
			set --local iotdps (az config get defaults.iotdps 2> /dev/null | _jq .value)

			if test "$argv[2]" = "--help"
				print_azure_help
				print_info iotdps "$iotdps"
				printf 'list: azure iot-dps enrollment groups using az\n'
				printf 'preview: azure iot-dps group\n'
				printf 'action: ^ - fz azure-iot-dps\n'
				return
			end

			set --local choice (az iot dps enrollment-group list --dps-name "$iotdps" \
				| _jq '.[] | .enrollmentGroupId' \
				| _awk "$awk_dim3" \
				| _fzf \
					--prompt "$argv[1] ($iotdps) ❯ " \
					--header "$account" \
					--expect ^ \
					--preview "az iot dps enrollment-group show --dps-name $iotdps --group-id {1} | $bat_json")

			if test -n "$choice"
				set --local verb_ids (string split ' ' "$choice")

				switch $verb_ids[1]

				case '^'
					fz azure-iot-dps

				case '*'
				end
			end

		case azure-iot-hubs
			if test "$argv[2]" = "--help"
				print_azure_help
				printf 'list: azure iot-hubs using az\n'
				printf 'preview: azure iot-hub details\n'
				printf 'action: ^ - fz azure-resources\n'
				printf 'action: ctrl-alt-e - fz azure-iot-hub-endpoints\n'
				printf 'action: ctrl-alt-r - fz azure-iot-hub-routes\n'
				return
			end

			set --local choice (az iot hub list \
				| _jq '.[] | "\(.name)\u001f\(.location)\u001f\(.id)"' \
				| _awk "$awk_dim3" \
				| _fzf \
					--header "$account" \
					--expect ^ \
					--expect ctrl-alt-e \
					--expect ctrl-alt-r \
					--preview "az iot hub show --name {1} | $bat_json")

			if test -n "$choice"
				set --local verb_ids (string split ' ' "$choice")
				set --local iothub $verb_ids[2]

				switch $verb_ids[1]

				case '^'
					fz azure-resources

				case 'ctrl-alt-e'
					az config set defaults.iothub="$iothub" 2> /dev/null
					fz azure-iot-hub-endpoints

				case 'ctrl-alt-r'
					az config set defaults.iothub="$iothub" 2> /dev/null
					fz azure-iot-hub-routes

				case '*'
					az config set defaults.iothub="$iothub" 2> /dev/null

				end
			end

		case azure-iot-hub-endpoints
			set --local iothub (az config get defaults.iothub 2> /dev/null | _jq .value)

			if test "$argv[2]" = "--help"
				print_azure_help
				print_info iothub "$iothub"
				printf 'list: azure iot-hub endpoints using az\n'
				printf 'preview: azure iot-hub endpoint\n'
				printf 'action: ^ - fz azure-iot-hubs\n'
				printf 'action: ctrl-alt-r - fz azure-iot-hub-routes\n'
				return
			end

			set --local choice (az iot hub message-endpoint list --hub-name "$iothub" \
				| _jq '.eventHubs | .[].name' \
				| _awk "$awk_dim3" \
				| _fzf \
					--prompt "$argv[1] ($iothub) ❯ " \
					--header "$account" \
					--expect ^ \
					--expect ctrl-alt-r \
					--preview "az iot hub message-endpoint show --hub-name $iothub --endpoint-name {1} | $bat_json")

			if test -n "$choice"
				set --local verb_ids (string split ' ' "$choice")

				switch $verb_ids[1]

				case '^'
					fz azure-iot-hubs

				case 'ctrl-alt-r'
					az config set defaults.iothub="$iothub" 2> /dev/null
					fz azure-iot-hub-routes

				case '*'
				end
			end

		case azure-iot-hub-routes
			set --local iothub (az config get defaults.iothub 2> /dev/null | _jq .value)

			if test "$argv[2]" = "--help"
				print_azure_help
				print_info iothub "$iothub"
				printf 'list: azure iot-hub routes using az\n'
				printf 'preview: azure iot-hub route\n'
				printf 'action: ^ - fz azure-iot-hubs\n'
				printf 'action: ctrl-alt-e - fz azure-iot-hub-endpoints\n'
				return
			end

			set --local choice (az iot hub message-route list --hub-name "$iothub" \
				| _jq '.[] | "\(.name)\u001f\(.source)\u001f\(.isEnabled)"' \
				| _awk "$awk_dim3" \
				| _fzf \
					--prompt "$argv[1] ($iothub) ❯ " \
					--header "$account" \
					--expect ^ \
					--expect ctrl-alt-e \
					--preview "az iot hub message-route show --hub-name $iothub --route-name {1} | $bat_json")

			if test -n "$choice"
				set --local verb_ids (string split ' ' "$choice")

				switch $verb_ids[1]

				case '^'
					fz azure-iot-hubs

				case 'ctrl-alt-e'
					az config set defaults.iothub="$iothub" 2> /dev/null
					fz azure-iot-hub-endpoints

				case '*'
				end
			end

		case azure-resource-groups
			if test "$argv[2]" = "--help"
				print_azure_help
				printf 'list: azure resource groups using az\n'
				printf 'preview: azure resources in this rg\n'
				printf 'action: ret - set default resource group and display its resources\n'
				printf 'action: ^ - fz azure-accounts\n'
				return
			end

			set --local choice (az group list \
				| _jq '.[] | "\(.name)\u001f\(.location)\u001f\(.id)"' \
				| _awk "$awk_dim3" \
				| _fzf \
					--header "$account" \
					--expect ^ \
					--preview 'az resource list --resource-group {1} | jq --raw-output ".[] | .name"')

			if test -n "$choice"
				set --local verb_ids (string split ' ' "$choice")
				set --local rg $verb_ids[2]

				switch $verb_ids[1]

				case '^'
					fz azure-accounts

				case '*'
					az config set defaults.rg="$rg" 2> /dev/null
					fz azure-resources "'$rg'"

				end
			end

		case azure-resources
			if test "$argv[2]" = "--help"
				print_azure_help
				printf 'list: azure resources using az\n'
				printf 'preview: azure resource\n'
				printf 'action: ret - depends on resource type\n'
				printf 'action: ^ - fz azure-resource-groups\n'
				return
			end

			set --local choice (az resource list \
				| _jq '.[] | "\(.name)\u001f\(.type)\u001f\(.resourceGroup)"' \
				| _awk '{printf "%s \x1b[38;2;98;114;164m%s %s\x1b[m\n", $1, $2, $3}' \
				| _fzf \
					--header "$account" \
					--expect ^ \
					--preview "az resource show --name {1} --resource-type {-2} --resource-group {-1} | $bat_json")

			if test -n "$choice"
				set --local verb_ids (string split ' ' "$choice")
				set --local resource $verb_ids[2..]

				switch $verb_ids[1]

				case '^'
					fz azure-resource-groups "$resource[3]"

				case '*'
					switch "$resource[2]"

					case "Microsoft.ContainerRegistry/registries"
						az config set defaults.acr="$resource[1]" 2> /dev/null
						fz azure-container-registry-repositories

					case "Microsoft.Storage/storageAccounts"
						az config set defaults.storageaccount="$resource[1]" 2> /dev/null
						fz azure-storage-containers

					end
				end
			end

		case azure-storage-accounts
			if test "$argv[2]" = "--help"
				print_azure_help
				printf 'list: azure storage accounts using az\n'
				printf 'preview: azure storage account details\n'
				printf 'action: ret - set default azure storage account and display its containers\n'
				printf 'action: ^ - fz azure-resources\n'
				return
			end

			set --local choice (az storage account list \
				| _jq '.[] | "\(.name)\u001f\(.location)\u001f\(.id)"' \
				| _awk "$awk_dim3" \
				| _fzf \
					--header "$account" \
					--expect ^ \
					--preview "az storage account show --name {1} | $bat_json")

			if test -n "$choice"
				set --local verb_ids (string split ' ' "$choice")
				set --local storageaccount $verb_ids[2]

				switch $verb_ids[1]

				case '^'
					fz azure-resources

				case '*'
					az config set defaults.storageaccount="$storageaccount" 2> /dev/null
					fz azure-storage-containers "'$storageaccount'"

				end
			end

		case azure-storage-blobs
			# header
			set --local storageaccount (az config get defaults.storageaccount 2> /dev/null | _jq .value)
			set --local container (az config get defaults.storagecontainer 2> /dev/null | _jq .value)

			if test "$argv[2]" = "--help"
				print_azure_help
				print_info "storage account" "$storageaccount"
				print_info "storage container" "$container"
				printf 'list: azure storage blobs using az\n'
				printf 'preview: blob details\n'
				printf 'action: ^ - fz azure-storage-containers\n'
				printf 'action: enter - download blob in /tmp\n'
				return
			end

			set --local choice (az storage blob list --account-name "$storageaccount" --container-name "$container" 2> /dev/null \
				| _jq '.[] | "\(.name)"' \
				| _fzf --query '' \
					--header "$storageaccount/$container" \
					--expect ^ \
					--expect enter \
					--preview "az storage blob show --account-name="$storageaccount" --container-name='$container' --name {1} 2> /dev/null | $bat_json")

			if test -n "$choice"
				set --local verb_ids (string split ' ' "$choice")

				switch $verb_ids[1]

				case '^'
					fz azure-storage-containers

				case 'enter'
					set --local download_dir "/tmp/fz/azure-storage-blobs/$storageaccount/$container/"(dirname "$verb_ids[2]")
					mkdir --parents "$download_dir"
					az storage blob download \
						--account-name="$storageaccount" \
						--container-name="$container" \
						--name "$verb_ids[2]" \
						--file "$download_dir/"(basename "$verb_ids[2]")

				case '*'

				end
			end

		case azure-storage-containers
			# header
			set --local storageaccount (az config get defaults.storageaccount 2> /dev/null | _jq .value)

			if test "$argv[2]" = "--help"
				print_azure_help
				print_info "storage account" "$storageaccount"
				printf 'list: azure storage containers using az\n'
				printf 'preview: azure storage container details\n'
				printf 'action: ret - set default azure storage container and display its blobs\n'
				printf 'action: ^ - fz azure-storage-accounts\n'
				return
			end

			set --local choice (az storage container list --auth-mode login --account-name "$storageaccount" 2> /dev/null \
				| _jq '.[] | "\(.name)"' \
				| _awk '{printf "%s\n", $1}' \
				| _fzf --query '' \
					--header "$storageaccount" \
					--expect ^ \
					--preview "az storage container show --account-name="$storageaccount" --name {1} 2> /dev/null | $bat_json")

			if test -n "$choice"
				set --local verb_ids (string split ' ' "$choice")
				set --local container $verb_ids[2]

				switch $verb_ids[1]

				case '^'
					fz azure-storage-accounts

				case '*'
					az config set defaults.storagecontainer="$container" 2> /dev/null
					fz azure-storage-blobs "'$container'"

				end
			end
		end

	case bins
		if test "$argv[2]" = "--help"
			printf 'list: system binaries\n'
			printf 'preview: which binary\n'
			printf 'action: launch binary\n'
			return
		end

		set --local choice (complete -C '' \
			| awk '{print $1}' \
			| _fzf --preview 'pacman --query --owns {1}; type {1};')

		if test -n "$choice"
			i3-msg --quiet "exec --no-startup-id $choice"
		end

	case bin-bookmarks
		set --local data_source '~/.local/share/bin-bookmarks/*.bookmarks'
		if test "$argv[2]" = "--help"
			printf "list: bookmarked binaries from $data_source\n"
			printf 'preview: which binary\n'
			printf 'action: launch binary\n'
			return
		end

		set --local choice (cat ~/.local/share/bin-bookmarks/*.bookmarks \
			| rg -v '^#' | rg -v '^$' \
			| _fzf \
				--header "$data_source" \
				--preview 'pacman --query --owns {1}; type {1};')

		if test -n "$choice"
			i3-msg --quiet "exec --no-startup-id $choice"
		end

	case bluetooth-controllers
		if test "$argv[2]" = "--help"
			printf 'list: bluetooth controllers\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		set --local candidate (bluetoothctl list | _fzf)

	case bluetooth-devices
		if test "$argv[2]" = "--help"
			printf 'list: bluetooth devices\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		set --local candidate (bluetoothctl devices | _fzf)

	case browser-bookmarks
		set --local data_source '~/.local/share/browser-bookmarks/*.bookmarks'
		if test "$argv[2]" = "--help"
			printf "list: browser bookmarks from $data_source\n"
			print_dim 'preview: none'
			printf 'action: open bookmark in default browser\n'
			return
		end

		cat ~/.local/share/browser-bookmarks/*.bookmarks \
			| rg -v '^#' | rg -v '^$' \
			| _awk '{printf "%-12s \x1b[36m%s\x1b[m %s\n", $1, $2, $3}' \
			| _fzf \
				--header "$data_source" \
			| sed 's#.*\(https*://\)#\1#' \
			| xargs xdg-open > /dev/null
		sleep 0.2

	case browser-tabs
		if test "$argv[2]" = "--help"
			printf 'list: browser tabs\n'
			print_dim 'preview: none'
			printf 'action: ret - activate browser tab\n'
			printf 'action: alt-d - delete browser tab\n'
			return
		end

		set --local choice (firefoxctl tab list \
			| _jq '.[] | "\(.id) \(.lastAccessed)\t\(.title) \u001b[38;2;98;114;164m\(.url)\u001b[m"' \
			| _fzf \
				--multi \
				--expect alt-d \
			| awk '{print $1}')

		if test -n "$choice"
			set --local verb_ids (string split ' ' "$choice")
			set --local ids "$verb_ids[2..]"

			switch $verb_ids[1]

			case 'alt-d'
				firefoxctl tab delete $ids

			case '*'
				firefoxctl tab activate "$ids[1]"
			end

		end

	case dbus-system-peers
		if test "$argv[2]" = "--help"
			printf 'list: dbus system peers\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		set --local candidate (busctl list --system \
			| _fzf \
				--header-lines=1)

	case dbus-user-peers
		if test "$argv[2]" = "--help"
			printf 'list: dbus user peers\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		set --local candidate (busctl list --user \
			| _fzf \
				--header-lines=1)

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

		set --local choice (_jq '.tasks | to_entries | .[] | "\(.key)\u001f\(.value)"' deno.jsonc \
			| _awk "{printf \"%s $comment%s$reset\n\", \$1, \$NF}" \
			| _fzf \
			| awk '{print $1}')

		if test -n "$choice"
			deno task "$choice"
		end

	case 'docker-*'
		if not systemctl is-active docker > /dev/null
			print_error 'docker daemon is not active'
			return 1
		end

		switch $argv[1]

		case docker-accounts
			if test "$argv[2]" = "--help"
				printf 'list: docker accounts if not handled by Secret Service\n'
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
			if test "$argv[2]" = "--help"
				printf 'list: docker containers\n'
				printf 'preview: docker container details\n'
				print_dim 'action: none'
				return
			end

			set --local choice (docker containers -a \
				| _fzf \
					--header-lines=1 \
					--preview "docker container inspect {1} | jq .[0] | $bat_json")

		case docker-images
			if test "$argv[2]" = "--help"
				printf 'list: docker images\n'
				printf 'preview: docker image details\n'
				print_dim 'action: none'
				return
			end

			set --local choice (docker images \
				| _fzf \
					--header-lines=1 \
					--preview "docker image inspect {3} | jq .[0] | $bat_json")

		case docker-images-dangling
			if test "$argv[2]" = "--help"
				printf 'list: docker dangling images\n'
				printf 'preview: docker image details\n'
				print_dim 'action: none'
				return
			end

			set --local choice (docker images --filter 'dangling=true' \
				| _fzf \
					--header-lines=1 \
					--preview "docker image inspect {3} | jq .[0] | $bat_json")

		case docker-networks
			if test "$argv[2]" = "--help"
				printf 'list: docker networks\n'
				printf 'preview: docker network details\n'
				print_dim 'action: none'
				return
			end

			set --local choice (docker networks \
				| _fzf \
					--header-lines=1 \
					--preview "docker network inspect {1} | jq .[0] | $bat_json")

		case docker-registries
			if test "$argv[2]" = "--help"
				printf 'list: docker registries\n'
				print_dim 'preview: none'
				print_dim 'action: none'
				return
			end

			jq --raw-output '.auths | keys | .[]' ~/.docker/config.json \
				| _fzf

		case docker-volumes
			if test "$argv[2]" = "--help"
				printf 'list: docker volumes\n'
				printf 'preview: docker volume details\n'
				print_dim 'action: none'
				return
			end

			set --local choice (docker volumes \
				| _fzf \
					--header-lines=1 \
					--preview "docker volume inspect {3} | jq .[0] | $bat_json")

		end

	case efi-variables
		if test "$argv[2]" = "--help"
			printf 'list: efi variables\n'
			printf 'preview: efi variable attributes and value\n'
			print_dim 'action: none'
			return
		end

		set --local choice (efivar --list \
			| _fzf --preview "efivar --name {}")

	case environment-variables
		if test "$argv[2]" = "--help"
			printf 'list: environment variables\n'
			printf 'preview: environment variable content and scope\n'
			print_dim 'action: none'
			return
		end

		_fzf_search_variables (set --show | psub) (set --names | psub)

	case eslint-rules
		set --local data_source "$HOME/.local/share/eslint/rules.json"
		if test "$argv[2]" = "--help"
			printf "list: eslint rules from $data_source\n"
			printf "preview: rules details\n"
			printf "action: visit rule documentation on eslint.org\n"
			return
		end


		set --local rule_id (cat "$data_source" \
			| _jq  '["id", "type", "recommended", "deprecated", "fixable", "description"],
							(.[] | [.id, "t:\(.type)", "r:\(.recommended)", "d:\(.deprecated)", "f:\(.fixable)", .description]) | @tsv' \
				| column -ts \t \
			| _fzf \
				--header-lines 1 \
				--preview "jq --color-output '.[] | select(.id==\"{1}\")' '$data_source'" \
			| awk '{print $1}')

		if test -n "$rule_id"
			if string match -r '@typescript' "$rule_id"
				set --local ts_rule_id (string sub --start 20 $rule_id)
				firefox-developer-edition "https://typescript-eslint.io/rules/$ts_rule_id"
			else if string match -r '@angular' "$rule_id"
				set --local ng_rule_id (string sub --start 16 $rule_id)
				firefox-developer-edition "https://github.com/angular-eslint/angular-eslint/blob/main/packages/eslint-plugin/docs/rules/$ng_rule_id.md"
			else if string match -r 'jest' "$rule_id"
				set --local jest_rule_id (string sub --start 5 $rule_id)
				firefox-developer-edition "https://github.com/jest-community/eslint-plugin-jest/blob/main/docs/rules/$jest_rule_id.md"
			else
				firefox-developer-edition "https://eslint.org/docs/rules/$rule_id"
			end
		end

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

	case 'git'
		if not git status &> /dev/null
			print_error 'not in a git repository'
			return 1
		end

		set --local candidate (printf 'branches\nlog\nremotes\nstatus\ntags\n' | _fzf)
		fz "git-$candidate"

	case 'git-*'
		if not git status &> /dev/null
			print_error 'not in a git repository'
			return 1
		end

		switch $argv[1]

		case git-branches
			if test "$argv[2]" = "--help"
				printf 'list: git branches\n'
				printf 'preview: git branch details\n'
				print_dim 'action: none'
				return
			end

			git branches --color=always | _fzf --preview 'git show {2} --color=always'

		case git-config
			if test "$argv[2]" = "--help"
				printf 'list: git config global and local\n'
				print_dim 'preview: none'
				print_dim 'action: none'
				return
			end

			cat (git config --list --global | sed 's/^/global /' | psub) \
				(git config --list --local | sed 's/^/local  /' | psub) \
				| _fzf

		case git-log
			if test "$argv[2]" = "--help"
				printf 'list: git log\n'
				printf 'preview: git commit details\n'
				print_dim 'action: none'
				return
			end

			_fzf_search_git_log

		case git-remotes
			if test "$argv[2]" = "--help"
				printf 'list: git remotes\n'
				print_dim 'preview: none'
				print_dim 'action: none'
				return
			end

			git remotes | _fzf --preview 'git remote show {1}'

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

		end

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

		set --local choice (gh repo list \
			| _fzf \
				--preview 'gh repo view {1}' | awk '{print $1}')

		if test -n "$choice"
			gh repo view --web "$choice"
		end

	case gpg-public-keys
		if not command -q gpg
			print_error 'gpg command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: gpg public keys\n'
			printf 'preview: gpg public key info\n'
			print_dim 'action: none'
			return
		end

		gpg --list-public-keys --with-colons \
			| rg uid \
			| awk -F ':' '{ print $10 }' \
			| _fzf \
				--preview 'gpg --list-public-keys {1..}'

	case 'i3-*'
		if not command -q i3-msg
			print_error 'i3-msg command not found'
			return 1
		end

		switch $argv[1]

		case i3-windows
			if test "$argv[2]" = "--help"
				printf 'list: i3 windows\n'
				print_dim 'preview: none'
				printf 'action: focus window\n'
				return
			end

			set --local jq_filter '.. | objects | select(.window_type == "normal") | "\(.id)\u001f\(.window_properties.class):\u001f\(.name)"'
			set --local choice (i3-msg -t get_tree \
				| _jq "$jq_filter" \
				| _awk '{printf "%s \x1b[36m%s\x1b[m %s\n", $1, $2, $3}' \
				| _fzf --with-nth=2.. \
				| awk '{print $1}')

			if test -n "$choice"
				i3-msg --quiet "[con_id=$choice] focus"
			end

		case i3-workspaces
			if test "$argv[2]" = "--help"
				set --local count (i3-msg -t get_workspaces | jq 'length')
				printf "list: i3 workspaces ($count)\n"
				print_dim 'preview: none'
				printf 'action: focus workspace\n'
				return
			end

			set --local choice (i3-msg -t get_workspaces \
				| _jq '.[] .name' \
				| _fzf)

			if test -n "$choice"
				i3-msg --quiet "workspace $choice"
			end

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

	case json-schemas
		set --local actions \
			"download schema to $tmpdir" \
			'open git repository'

		if test "$argv[2]" = "--help"
			printf 'list: JSON schemas fetched from https://schemastore.org\n'
			printf 'preview: JSON schema details\n'
			printf 'actions:\n'
			printf '- %s\n' $actions
			return
		end

		set --local candidate (curl --silent 'https://www.schemastore.org/api/json/catalog.json' \
			| _jq '.schemas | .[] | "\(.name)\u001f\(.fileMatch)\u001f\(.url)"' \
			| _awk "$awk_dim3" \
			| _fzf --preview "curl --silent {-1} | jq --color-output .")

		set --local action (printf '%s\n' $actions \
			| _fzf --prompt 'actions ❯ ' --header $candidate)

		set --local url (printf $candidate | awk '{print $NF}')

		switch $action

		case $actions[1]
			mkdir -p "$tmpdir"
			curl --silent "$url" --output-dir "$tmpdir" --remote-name
			printf "downloaded to $tmpdir\n"

		case $actions[2]
			print_error "not implemented yet"

		end

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

	case key-compositions
		if test "$argv[2]" = "--help"
			printf 'list: key compositions\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		cat /usr/share/X11/locale/en_US.UTF-8/Compose \
			~/.config/XCompose \
			| rg --invert-match '^#' | _fzf

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

	case mounts
		if test "$argv[2]" = "--help"
			printf 'list: mounts with labels\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		mount --show-label | _fzf

	case 'music-*'
		if not command -q mpc
			print_error 'mpc command not found'
			return 1
		end

		switch $argv[1]

		case music-albums
			if test "$argv[2]" = "--help"
				printf 'list: music albums using mpc\n'
				printf 'preview: music album tracks\n'
				print_dim 'action: none'
				return
			end

			mpc list album | _fzf --preview 'mpc search album {}'

		case music-artists
			if test "$argv[2]" = "--help"
				printf 'list: music artists using mpc\n'
				printf 'preview: music artist tracks\n'
				print_dim 'action: none'
				return
			end

			mpc list artist | _fzf --preview 'mpc search artist {}'

		case music-dates
			if test "$argv[2]" = "--help"
				printf 'list: music dates using mpc\n'
				printf 'preview: music dates tracks\n'
				print_dim 'action: none'
				return
			end

			mpc list date | _fzf --preview 'mpc search date {}'

		case music-playlists
			if test "$argv[2]" = "--help"
				printf 'list: music playlists using mpc\n'
				printf 'preview: music playlist tracks\n'
				print_dim 'action: none'
				return
			end

			mpc lsplaylist | _fzf --preview 'mpc playlist {}'

		end

	case network-ports
		if test "$argv[2]" = "--help"
			printf 'list: tcp and upd ports registered by IANA\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		cat /etc/services | tail --lines +3 | _fzf

	case npm-dependencies
		set --local data_source "$PWD/package.json"
		if not test -e "$data_source"
			print_error 'no package.json in current directory'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf "list: npm dependencies from $data_source\n"
			print_dim 'preview: none'
			printf 'action: visit npm dependency registry page\n'
			return
		end

		set --local choice (_jq '.dependencies | to_entries | .[] | "\(.key)\u001f\(.value)"' package.json \
			| _awk "{printf \"%s $comment%s$reset\n\", \$1, \$NF}" \
			| _fzf \
				--header "$data_source" \
			| awk '{print $1}')

		if test -n "$choice"
			firefox-developer-edition "https://www.npmjs.com/package/$choice"
		end

	case npm-scripts
		set --local data_source "$PWD/package.json"
		if not test -e "$data_source"
			print_error 'no package.json in current directory'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf "list: npm scripts from $data_source\n"
			print_dim 'preview: none'
			printf 'action: run npm script\n'
			return
		end

		set --local choice (_jq '.scripts | to_entries | .[] | "\(.key)\u001f\(.value)"' package.json \
			| _awk "{printf \"%s $comment%s$reset\n\", \$1, \$NF}" \
			| _fzf \
				--header "$data_source" \
			| awk '{print $1}')

		if test -n "$choice"
			npm run "$choice"
		end

	case org-roam-links
		if test "$argv[2]" = "--help"
			printf "list: https links from org-roam\n"
			print_dim 'preview: none'
			printf 'action: open https link in default browser\n'
			return
		end

		sqlite3 ~/.config/emacs/.local/cache/org-roam.db 'select dest from links where type = \'"https"\' order by dest' \
			| _awk '{gsub(/"/, "", $1); printf "https:%s\n", $1}' \
			| _fzf \
			| xargs xdg-open
		sleep 0.2

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
			| awk "$awk_dim2" \
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

	case 'pulseaudio-*'
		if not command -q pactl
			print_error 'pactl command not found'
			return 1
		end

		switch $argv[1]

		case pulseaudio-modules
			if test "$argv[2]" = "--help"
				printf 'list: pulseaudio modules\n'
				print_dim 'preview: none'
				print_dim 'action: none'
				return
			end

			set --local choice (pactl -f json list modules\
				| _jq '.[] | "\(.properties["module.description"])\u001f\(.name)"' \
				| _awk "$awk_dim2" \
				| _fzf \
				| awk '{print $1}')

		case pulseaudio-sinks
			if test "$argv[2]" = "--help"
				printf 'list: pulseaudio sinks\n'
				print_dim 'preview: none'
				print_dim 'action: none'
				return
			end

			set --local choice (pactl -f json list sinks \
				| _jq '.[] | "\(.description)\u001f\(.active_port)"' \
				| _awk "$awk_dim2" \
				| _fzf \
				| awk '{print $1}')

		case pulseaudio-sources
			if test "$argv[2]" = "--help"
				printf 'list: pulseaudio sources\n'
				print_dim 'preview: none'
				print_dim 'action: none'
				return
			end

			set --local choice (pactl -f json list sources \
				| _jq '.[] | "\(.description)\u001f\(.active_port)"' \
				| _awk "$awk_dim2" \
				| _fzf \
				| awk '{print $1}')

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

	case skopeo-bookmarks
		set --local data_source '~/.local/share/skopeo-bookmarks/*.bookmarks'
		if test "$argv[2]" = "--help"
			printf "list: bookmarked skopeo images from $data_source\n"
			printf  'preview: image tags\n'
			print_dim 'action: none'
			return
		end

		set --local choice (cat ~/.local/share/skopeo-bookmarks/*.bookmarks \
			| rg -v '^#' | rg -v '^$' \
			| _fzf \
				--header "$data_source" \
				--preview "skopeo list-tags {1}")

		if test -n "$choice"
			echo "$choice" > /tmp/fz-skopeo-image
			fz skopeo-tags
		end

	case skopeo-tags
		set --local skopeo_image (cat /tmp/fz-skopeo-image)
		if test "$argv[2]" = "--help"
			printf 'list: skopeo tags\n'
			printf 'preview: tag creation date\n'
			print_dim 'action: none'
			return
		end

		skopeo list-tags "$skopeo_image" | _jq '.Tags .[]' \
			| _fzf \
				--header "$skopeo_image" \
				--preview "skopeo inspect $skopeo_image:{1} | jq .Created"

	case sockets
		if test "$argv[2]" = "--help"
			printf 'list: sockets using ss\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		set --local choice (ss --all \
			| _fzf \
				--header-lines=1)

	case ssh-agent-keys
		if not command -q ssh-add
			print_error 'ssh-add command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: ssh-agent keys SHA256 fingerprints\n'
			printf 'preview: full public key\n'
			print_dim 'action: none'
			return
		end

		ssh-add -l | _fzf --preview 'ssh-add -L | rg {3}' --preview-window wrap

	case ssh-authorized-keys
		if test "$argv[2]" = "--help"
			printf 'list: keys in ~/.ssh/authorized_keys\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		cat ~/.ssh/authorized_keys | _fzf

	case ssh-hosts
		if test "$argv[2]" = "--help"
			printf 'list: ssh hosts in ~/.ssh configs\n'
			print_dim 'preview: none'
			printf 'action: connect to ssh server\n'
			return
		end

		set --local configs (fd config ~/.ssh/)
		set --local choice (cat $configs | rg 'Host ' | _fzf | awk '{print $2}')

		if test -n "$choice"
			ssh "$choice"
		end

	case 'starship-*'
		if not command -q starship
			print_error 'starship command not found'
			return 1
		end

		switch $argv[1]

		case starship-modules
			if test "$argv[2]" = "--help"
				printf 'list: starship modules\n'
				print_dim 'preview: none'
				print_dim 'action: none'
				return
			end

			starship module --list | tail --lines +3 | _fzf

		case starship-presets
			if test "$argv[2]" = "--help"
				printf 'list: starship presets\n'
				print_dim 'preview: none'
				print_dim 'action: none'
				return
			end

			starship preset --list | _fzf
		end

	case sysctl-values
		if not command -q sysctl
			print_error 'sysctl command not found'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: sysctl values\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		set --local choice (sysctl --all | _fzf)

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

	case trust-policies
		if test "$argv[2]" = "--help"
			printf 'list: anchors, certificates and blocklists from trust policy store (PKCS#11)\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		trust list | rg label | _fzf

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

		set --local choice (lsusb | _fzf --preview 'lsusb --verbose -d {6} 2> /dev/null')

	case user-groups
		if test "$argv[2]" = "--help"
			printf 'list: groups in /etc/group\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		set --local candidate (cat /etc/group | _fzf)

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

		set --local choice (code --list-extensions --show-versions 2> /dev/null | _fzf)

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
		set --local choice  (fd .code-workspace "$dir" \
			| _fzf --preview 'bat {} --plain --language json --color always')

		if test -n "$choice"
			code "$choice"
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

	case yarn-workspaces
		if not test -e './package.json'
			print_error 'no package.json in current directory'
			return 1
		end

		if test "$argv[2]" = "--help"
			printf 'list: yarn workspaces\n'
			print_dim 'preview: none'
			print_dim 'action: none'
			return
		end

		yarn workspaces list --json \
			| _jq '"\(.name)\u001f\(.location)"' \
			| _awk "$awk_dim2" \
			| _fzf

	# by default let the user discover and choose the input source
	case '*'
		set --local commands \
			acpi-devices \
			asdf-plugins \
			azure-accounts \
			azure-appservice-functionapps \
			azure-appservice-plans \
			azure-appservice-webapps \
			azure-container-registries \
			azure-container-registry-repositories \
			azure-container-registry-manifests \
			azure-event-hub-namespaces \
			azure-event-hubs \
			azure-extensions \
			azure-functions \
			azure-iot-dps \
			azure-iot-dps-certificates \
			azure-iot-dps-groups \
			azure-iot-hubs \
			azure-iot-hub-endpoints \
			azure-iot-hub-routes \
			azure-resource-groups \
			azure-resources \
			azure-storage-accounts \
			azure-storage-blobs \
			azure-storage-containers \
			bins \
			bin-bookmarks \
			bluetooth-controllers \
			bluetooth-devices \
			browser-bookmarks \
			browser-tabs \
			dbus-system-peers \
			dbus-user-peers \
			deno-tasks \
			docker-accounts \
			docker-containers \
			docker-images \
			docker-images-dangling \
			docker-networks \
			docker-registries \
			docker-volumes \
			efi-variables \
			environment-variables \
			eslint-rules \
			files \
			file-descriptors \
			fonts \
			git \
			git-branches \
			git-config \
			git-log \
			git-remotes \
			git-status \
			git-tags \
			github-repositories \
			gpg-public-keys \
			i3-windows \
			i3-workspaces \
			ip-addresses \
			json-schemas \
			kakoune-sessions \
			key-compositions \
			linux-kernel-modules \
			linux-namespaces \
			man-pages \
			media-types \
			monitors \
			mounts \
			music-albums \
			music-artists \
			music-dates \
			music-playlists \
			network-ports \
			npm-dependencies \
			npm-scripts \
			org-roam-links \
			pacman-mirrors \
			pacman-packages \
			pastel-colors \
			podman-pods \
			processes \
			pulseaudio-modules \
			pulseaudio-sinks \
			pulseaudio-sources \
			shell-abbreviations \
			shell-aliases \
			shell-functions \
			shell-history \
			shell-key-bindings \
			shell-prompts \
			shell-themes \
			skopeo-bookmarks \
			skopeo-tags \
			ssh-agent-keys \
			ssh-authorized-keys \
			ssh-hosts \
			sockets \
			starship-modules \
			starship-presets \
			sysctl-values \
			systemd \
			top-level-domains \
			trust-policies \
			usb-devices \
			user-groups \
			vscode-extensions \
			vscode-workspaces \
			xinput-devices \
			yarn-workspaces

		set --local enabled_commands
		for command in $commands
			set --local enabled 0

			switch $command
			case 'acpi-devices'
				command -q acpi

			case 'asdf-*'
				command -q asdf

			case 'azure-*'
				command -q az

			case 'bluetooth-*'
				command -q bluetoothctl

			case 'deno-*'
				test -e './deno.jsonc'

			case 'docker-*'
				systemctl is-active docker > /dev/null

			case fonts
				command -q fontpreview

			case 'git'
				git status &> /dev/null

			case 'git-*'
				git status &> /dev/null

			case 'gpg-*'
				command -q gpg

			case 'i3-*'
				command -q i3-msg

			case 'kakoune-*'
				command -q kak

			case monitors
				command -q xrandr

			case 'music-*'
				command -q mpc

			case 'npm-*'
				test -e './package.json'

			case pastel-colors
				command -q pastel

			case 'podman-*'
				command -q podman

			case 'pulseaudio-*'
				command -q pactl

			case 'sockets'
				command -q ss

			case 'starship-*'
				command -q starship

			case 'usb-*'
				command -q lsusb

			case 'vscode-*'
				command -q code

			case xinput-devices
				command -q xinput

			case 'yarn-*'
				test -e './package.json'

			case '*'
				true
			end

			if test "$status" = 0
				set enabled_commands $enabled_commands $command
			else
				set enabled_commands $enabled_commands (print_disabled $command)
			end
		end

		set --local selected_command (printf '%s\n' $enabled_commands | _fzf --prompt 'fz ❯ ' --preview 'fz {} --help')
		if test -n "$selected_command"
			fz $selected_command
		end
	end
end
