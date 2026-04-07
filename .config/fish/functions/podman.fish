function podman --description 'podman wrapper'
	switch $argv[1]

	case 'ls'
		command podman pod ls
		printf "\n"
		command podman container ls --all
		printf "\n"
		command podman image ls
		printf "\n"
		command podman volume ls
		printf "\n"
		command podman network ls

	case 'volumes'
		command podman volume ls --format 'table {{.Driver}}\t{{.Scope}}\t{{.Name}}\t{{.CreatedAt}}'

	case '*'
		command podman $argv
	end
end

