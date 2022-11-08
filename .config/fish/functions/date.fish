function date --description 'default to ISO8601 and ts subcommand'
	switch $argv[1]
	case ''
		command date --iso-8601=seconds $argv

	case 'ts'
		# timestamp in seconds
		command date +%s
		# timestamp in milliseconds
		command date +%s%3N

	case '*'
		command date $argv
	end
end

