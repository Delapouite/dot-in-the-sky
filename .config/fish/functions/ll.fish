function ll --description 'list files verbosely'
	eza \
		--classify \
		--color-scale all \
		--color-scale-mode fixed \
		--git \
		--links \
		--long \
		--octal-permissions \
		$argv
end

# see also lh, llh
