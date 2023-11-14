function lt --description 'list files verbosely as a tree'
	eza \
		--classify \
		--color-scale all \
		--color-scale-mode fixed \
		--git \
		--links \
		--long \
		--octal-permissions \
		--tree \
		$argv
end

