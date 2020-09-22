# Get list of commands

	openssl help

# `genpkey` is encouraged over `genrsa`, `gendsa`

	openssl genpkey -algorithm RSA -out key.pem

# Extract public key from private key

	openssl pkey -in private.pem -pubout -out public.pem

# See also

- openssl-pkey(1)
