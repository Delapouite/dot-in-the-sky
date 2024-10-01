function pandock --description 'run pandoc/minimal container'
	docker run \
		--rm \
		--volume "$(pwd):/data" \
		--user $(id -u):$(id -g) \
		pandoc/minimal \
		$argv
end

