function radix --description 'conversion between number radix'
	# bc only accepts uppercase hexa
	set -l num (string upper $argv[2])

	switch $argv[1]
	# h = hexa, d = deci, o = octal, b = binary
	# obase needs to be specified first:
	# https://unix.stackexchange.com/questions/199615/understand-ibase-and-obase-in-case-of-conversions-with-bc

	# h
	case 'hd'
		echo "obase=10; ibase=16; $num" | bc

	case 'ho'
		echo "obase=8; ibase=16; $num" | bc

	case 'hb'
		echo "obase=2; ibase=16; $num" | bc

	# d
	case 'dh'
		echo "obase=16; ibase=10; $num" | bc

	case 'do'
		echo "obase=8; ibase=10; $num" | bc

	case 'db'
		echo "obase=2; ibase=10; $num" | bc

	# o
	case 'oh'
		echo "obase=16; ibase=8; $num" | bc

	case 'od'
		echo "obase=10; ibase=8; $num" | bc

	case 'ob'
		echo "obase=2; ibase=8; $num" | bc

	# b
	case 'bh'
		echo "obase=16; ibase=2; $num" | bc

	case 'bd'
		echo "obase=10; ibase=2; $num" | bc

	case 'bo'
		echo "obase=8; ibase=2; $num" | bc

	case '*'
		echo 'unknown subcommand'
	end
end
