function pacman --description 'pacman wrapper'
	switch $argv[1]

	case 'orphans'
		command pacman --query --deps --unrequired --quiet

	case '*'
		command pacman $argv
	end
end

