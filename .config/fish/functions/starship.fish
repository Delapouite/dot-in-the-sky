function starship --description 'starship'
	switch $argv[1]

	case 'toggle'
		set --global --export STARSHIP_CONFIG "/tmp/starship-$STARSHIP_SESSION_KEY.toml"
		if not test -e "$STARSHIP_CONFIG"
			cp ~/.config/starship.toml "$STARSHIP_CONFIG"
		end

		command starship toggle "$argv[2]"

	case '*'
		command starship $argv

	end
end
