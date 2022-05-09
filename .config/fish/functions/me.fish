function me --description 'show info about current user and accounts'
	printf "\n🛒 Caddy"
	if systemctl is-active --quiet caddy
		printf " ✅:\n"
	else
		printf " ❌:\n"
	end
	rg --before-context 1 --no-line-number --no-filename reverse_proxy /etc/caddy/conf.d/

	printf "\n🧛 id:\n"
	id

	printf "\n🔑 SSH Agent (ssh-add -l):\n"
	ssh-add -l

	printf "\n☁️ Azure (az account show):\n"
	az account show

	printf "\n🐳 Docker (config.json .auths)"
	if systemctl is-active --quiet docker
		printf " ✅:\n"
	else
		printf " ❌:\n"
	end
	jq '.auths | keys' ~/.docker/config.json
end
