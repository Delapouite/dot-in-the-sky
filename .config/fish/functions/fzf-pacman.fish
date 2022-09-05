function fzf-pacman --description 'browse pacman packages'
	pacman --query --quiet | \
	fzf \
		--layout=reverse \
		--preview 'pacman --query --info --list {}' \
		--bind 'enter:execute(pacman --query --info --list {} | bat)'	
end
