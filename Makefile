build:
	stack build --allow-different-user

install: build
	cp "`stack path --allow-different-user --local-install-root`/bin/attendancebot" /usr/local/bin/
