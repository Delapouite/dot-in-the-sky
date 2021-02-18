function jwt --description 'inspect JWT'
	switch $argv[1]
	case 'validate'
		set json (command jwt decode --json $argv[2])
		set exp (echo "$json" | jq '.payload.exp')

		echo "$json" | jq .
		echo ""
		
		set ts (date +'%s')

		if test "$exp" -gt "$ts"
			echo 'not expired'
		else
			echo 'expired'
		end

	case '*'
		command jwt $argv
	end
end
