# Get list of commands

	openssl help

# `genpkey` is encouraged over `genrsa`, `gendsa`

	openssl genpkey -algorithm RSA -out key.pem
