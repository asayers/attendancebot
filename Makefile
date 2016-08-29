build:
	stack build --pedantic

install:
	systemctl stop attendancebot
	cp "`stack path --allow-different-user --local-install-root`/bin/attendancebot" /usr/local/bin/
	systemctl start attendancebot

tags:
	hasktags -cx src
