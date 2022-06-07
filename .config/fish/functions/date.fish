function date --description 'default to ISO8601'
	switch $argv[1]
	case ''
		command date --iso-8601=seconds $argv

	case '*'
		command date $argv
	end
end

