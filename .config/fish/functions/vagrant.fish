function vagrant --description 'wrapper to change kitty theme depending on host env'
	switch $argv[1]
	case 'ssh'
		kitty @ set-colors -a "~/.config/kitty/themes/ocean.conf"
	end

	command vagrant $argv
end
