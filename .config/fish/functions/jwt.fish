function jwt --description 'inspect JWT'
	switch $argv[1]
	case 'inspect'
		set json (command jwt decode --json $argv[2])

		set exp (echo "$json" | jq '.payload.exp')

		set isoiat (echo "$json" | jq '.payload.iat | todate')
		set isoexp (echo "$json" | jq '.payload.exp | todate')

		echo "$json" | jq .

		echo ""

		echo "isoiat: $isoiat"
		echo "isoexp: $isoexp"

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
