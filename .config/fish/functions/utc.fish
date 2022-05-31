function utc --description 'default to UTC ISO8601'
	command date --iso-8601=seconds --utc $argv
end
