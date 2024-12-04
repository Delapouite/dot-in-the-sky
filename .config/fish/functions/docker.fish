function docker --description 'docker wrapper'
	switch $argv[1]

	case 'ls'
		docker containers
		printf "\n"
		docker images
		printf "\n"
		docker volumes
		printf "\n"
		docker networks

	case 'containers'
		command docker container ls --all --format 'table {{.ID}}\t{{slice .CreatedAt 0 10}}\t{{.Image}}\t{{.Command}}\t{{.RunningFor}}\t{{.Status}}\t{{.Ports}}\t{{.Names}}'

	case 'contexts'
		command docker context ls

	case 'images'
		command docker image ls --format 'table {{.ID}}\t{{slice .CreatedAt 0 10}}\t{{.Repository}}\t{{.Tag}}\t{{if .CreatedSince }}{{.CreatedSince}}{{else}}N/A{{end}}\t{{.Size}}'

	case 'networks'
		command docker network ls --format 'table {{.ID}}\t{{slice .CreatedAt 0 10}}\t{{.Driver}}\t{{.Scope}}\t{{.Name}}'

	case 'volumes'
		# volumes do not have ids, nor createdAt
		command docker volume ls --format 'table {{.Driver}}\t{{.Scope}}\t{{.Name}}'

	case '*'
		command docker $argv
	end
end

