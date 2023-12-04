function lld --description 'list directories verbosely'
	eza \
		--classify \
		--color-scale all \
		--color-scale-mode fixed \
		--git \
		--links \
		--long \
		--octal-permissions \
		--only-dirs \
		$argv
end

# see also lh, llh
