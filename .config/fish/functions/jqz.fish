function jqz --description 'fzf + jq'
	if count $argv > /dev/null	
		echo '' | fzf --print-query --preview "cat $argv | jq -C {q}"
	else
		echo '' | fzf --print-query --preview "cat *.json | jq -C {q}"
	end
end
