function mpc --description 'wrapper to add subcommands aliases to mpc'
	switch $argv[1]

	case 'album'
		command mpc search album $argv[2]

	case 'albums'
		command mpc list album | fzf

	case 'artist'
		command mpc search artist $argv[2]

	case 'artists'
		command mpc list artist | fzf

	case 'dates'
		command mpc list date | fzf

	case 'playlists'
		command mpc lsplaylists

	case '*'
		command mpc $argv
	end

end

set -l subcommands consume crossfade queued mixrampdb mixrampdelay next \
    pause play prev random repeat replaygain single seek seekthrough stop \
    toggle add insert clear crop del mv searchplay shuffle load lsplaylists \
    playlist rm save listall ls search search find findadd list stats mount \
    mount unmount outputs disable enable toggleoutput channels sendmessage \
    waitmessage subscribe idle idleloop version volume update rescan current

complete -c mpc -n "not __fish_seen_subcommand_from $subcommands" -a albums -d "List albums"
complete -c mpc -n "not __fish_seen_subcommand_from $subcommands" -a artists -d "List artists"
complete -c mpc -n "not __fish_seen_subcommand_from $subcommands" -a dates -d "List dates"
complete -c mpc -n "not __fish_seen_subcommand_from $subcommands" -a playlists -d "List playlists"

