build:
	stack build --pedantic

build_dir != stack path --allow-different-user --local-install-root

install:
	cp attendancebot.service /etc/systemd/system/
	systemctl daemon-reload
	systemctl stop attendancebot
	cp "$(build_dir)/bin/attendancebot" /usr/local/bin/
	systemctl is-enabled attendancebot.service && systemctl start attendancebot

tags:
	hasktags -cx src
