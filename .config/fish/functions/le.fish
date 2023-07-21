function le --description 'print sorted and colorful environment variables'
	env -0 | sort -z | tr '\0' '\n' | sed -E 's/([^=]*)=(.*)/\x1b[36m\1\x1b[m \2/g'
end

