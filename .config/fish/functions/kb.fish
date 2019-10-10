function kb --description 'change keyboard layout'
	switch $argv[1]
		case 'bepo'
			setxkbmap fr bepo

		case 'delafayette'
			xkbcomp -w0 -I$HOME/.config/xkb $HOME/.config/xkb/keymap/delafayette $DISPLAY

		case '*'
			setxkbmap $argv
	end
end
