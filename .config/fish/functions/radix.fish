function radix --description 'conversion between number radix'
	# unfortunately bc's ibase can't be higher than 36
	# so conversion from base64 (tetrasexagesimal) is not possible

	if not command --query bc
		echo 'unknown command bc'
		return 1
	end

	# bc only accepts uppercase hexa
	set -l num (string upper $argv[2])

	switch $argv[1]
	# h = hexadecimal 16, d = decimal 10, o = octal 8, b = binary 2
	# obase needs to be specified first:
	# https://unix.stackexchange.com/questions/199615/understand-ibase-and-obase-in-case-of-conversions-with-bc

	# from h
	case 'h'
		echo h: $num
		echo h: (echo $num | sed ':a;s/\B[0-F]\{2\}\>/ &/;ta')
		echo d: (echo "obase=10; ibase=16; $num" | bc)
		echo d: (echo "obase=10; ibase=16; $num" | bc | sed ':a;s/\B[0-9]\{3\}\>/ &/;ta')
		echo o: (echo "obase=8; ibase=16; $num" | bc)
		echo b: (echo "obase=2; ibase=16; $num" | bc)
		echo b: (echo "obase=2; ibase=16; $num" | bc | sed ':a;s/\B[0-9]\{8\}\>/ &/;ta')
		echo b: (echo "obase=2; ibase=16; $num" | bc | sed ':a;s/\B[0-9]\{4\}\>/ &/;ta')

	case 'hd'
		echo "obase=10; ibase=16; $num" | bc

	case 'ho'
		echo "obase=8; ibase=16; $num" | bc

	case 'hb'
		echo "obase=2; ibase=16; $num" | bc

	# from d
	case 'd'
		echo h: (echo "obase=16; ibase=10; $num" | bc)
		echo h: (echo "obase=16; ibase=10; $num" | bc | sed ':a;s/\B[0-F]\{2\}\>/ &/;ta')
		echo d: $num
		echo d: (echo $num | sed ':a;s/\B[0-9]\{3\}\>/ &/;ta')
		echo o: (echo "obase=8; ibase=10; $num" | bc)
		echo b: (echo "obase=2; ibase=10; $num" | bc)
		echo b: (echo "obase=2; ibase=10; $num" | bc | sed ':a;s/\B[0-9]\{8\}\>/ &/;ta')
		echo b: (echo "obase=2; ibase=10; $num" | bc | sed ':a;s/\B[0-9]\{4\}\>/ &/;ta')

	case 'dh'
		echo "obase=16; ibase=10; $num" | bc

	case 'do'
		echo "obase=8; ibase=10; $num" | bc

	case 'db'
		echo "obase=2; ibase=10; $num" | bc

	# from o
	case 'o'
		echo h: (echo "obase=16; ibase=8; $num" | bc)
		echo h: (echo "obase=16; ibase=8; $num" | bc | sed ':a;s/\B[0-F]\{2\}\>/ &/;ta')
		echo d: (echo "obase=10; ibase=8; $num" | bc)
		echo d: (echo "obase=10; ibase=8; $num" | bc | sed ':a;s/\B[0-9]\{3\}\>/ &/;ta')
		echo o: $num
		echo b: (echo "obase=2; ibase=8; $num" | bc)
		echo b: (echo "obase=2; ibase=8; $num" | bc | sed ':a;s/\B[0-9]\{8\}\>/ &/;ta')
		echo b: (echo "obase=2; ibase=8; $num" | bc | sed ':a;s/\B[0-9]\{4\}\>/ &/;ta')

	case 'oh'
		echo "obase=16; ibase=8; $num" | bc

	case 'od'
		echo "obase=10; ibase=8; $num" | bc

	case 'ob'
		echo "obase=2; ibase=8; $num" | bc

	# from b
	case 'b'
		echo h: (echo "obase=16; ibase=2; $num" | bc)
		echo h: (echo "obase=16; ibase=2; $num" | bc | sed ':a;s/\B[0-F]\{2\}\>/ &/;ta')
		echo d: (echo "obase=10; ibase=2; $num" | bc)
		echo d: (echo "obase=10; ibase=2; $num" | bc | sed ':a;s/\B[0-9]\{3\}\>/ &/;ta')
		echo o: (echo "obase=8; ibase=2; $num" | bc)
		echo b: $num
		echo b: (echo $num | sed ':a;s/\B[0-9]\{8\}\>/ &/;ta')
		echo b: (echo $num | sed ':a;s/\B[0-9]\{4\}\>/ &/;ta')

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
