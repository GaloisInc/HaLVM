BINARIES=WebServer
include ../../standard.mk

run: WebServer
	-sudo xl destroy WebServer
	sudo xl create WebServer.config -c

stop:
	sudo xl destroy WebServer
