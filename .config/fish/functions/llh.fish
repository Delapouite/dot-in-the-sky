function llh --description 'list all files verbosely'
	eza \
		--all \
		--classify \
		--color-scale all \
		--color-scale-mode fixed \
		--git \
		--links \
		--long \
		--octal-permissions \
		$argv
end

# see also ll, lh
