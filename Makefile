build:
	stack build

install:
	cp "`stack path --allow-different-user --local-install-root`/bin/attendancebot" /usr/local/bin/
