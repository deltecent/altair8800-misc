all: server client

server: server.o io.o drive.o display.o
	cc -o server server.o io.o drive.o display.o -lncurses

server.o: server.c fdc.h io.h drive.h display.h

client: client.o io.o drive.o display.o
	cc -o client client.o io.o drive.o display.o -lncurses

client.o: client.c fdc.h io.h drive.h display.h

io.o: io.c io.h

drive.o: drive.c drive.h

display.o: display.c fdc.h drive.h display.h
