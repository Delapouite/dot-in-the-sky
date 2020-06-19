function tldr --description 'tealdeer + custom notes if they exist'
	command tldr $argv
	set md "$HOME/.local/share/tldr/$argv[1].md"
	if test -e "$md"
		bat -p "$md"
	end
end
