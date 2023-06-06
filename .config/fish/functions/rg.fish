function rg --description 'ripgrep with different default values'
	command rg \
		--max-columns 200 \
		--max-columns-preview \
		$argv
end
