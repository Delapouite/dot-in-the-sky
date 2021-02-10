function ssh --description 'wrapper to change kitty theme depending on host env'
	if string match -q -r -- '\-prod-' $argv
		kitty @ set-colors -a "~/.config/kitty/themes/red-alert.conf"
	else if string match -q -r -- '\-staging-' $argv
		kitty @ set-colors -a "~/.config/kitty/themes/espresso-libre.conf"
	else if string match -q -r -- '\-qual-' $argv
		kitty @ set-colors -a "~/.config/kitty/themes/espresso-libre.conf"
	else if string match -q -r -- '\-dev-' $argv
		kitty @ set-colors -a "~/.config/kitty/themes/grass.conf"
	end

	command ssh $argv
end
