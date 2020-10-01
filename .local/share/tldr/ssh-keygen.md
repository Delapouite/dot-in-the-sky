# Extract public key from private key (comment is lost)

	ssh-keygen -y -f ~/.ssh/id_rsa > ~/.ssh/id_rsa.pub

# Display a certificate

	ssh-keygen -L -f id_rsa-cert.pub
