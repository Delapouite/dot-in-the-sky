function xclipx --description 'output xclip clipboard in more meaningful ways'
	set --local content (command xclip -out -selection clipboard)

	if string match --quiet --regex '^[0-9]{10}$' -- $content
		# seconds
		command date --iso-8601=seconds --date "@$content"
	else if string match --quiet --regex '^[0-9]{13}$' -- $content
		# milliseconds
		set --local ms (string sub --length 10 $content)
		command date --iso-8601=seconds --date "@$ms"
	else
		echo 'clipboard content is none of the following formats: seconds, milliseconds'
	end
end
