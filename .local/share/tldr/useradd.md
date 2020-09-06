# This is the low level utility

To obtain almost the same effect than the `adduser` wizard:

	sudo useradd -d /home/delapouite -m -s /bin/bash \ -c FullName,Phone,OtherInfo delapouite && passwd delapouite

# See also

- usermod(8)
- userdel(8)

- groupadd(8)
- groupmode(8)
- groupdel(8)
