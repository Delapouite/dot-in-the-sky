function npm --description 'npm'
	switch $argv[1]

	case 'nuke'
		find . -name 'node_modules' -type d -prune -print -exec rm -rf '{}' \;

	case 'outdated'
		command npm outdated -l

	case '*'
		command npm $argv

	end
end

