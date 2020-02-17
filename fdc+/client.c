#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>
#include <errno.h>
#include <termios.h>
#include <getopt.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/select.h>
#include "io.h"
#include "fdc.h"
#include "drive.h"
#include "display.h"

#define	BPS		137	// Bytes Per Sector
#define SPT		32	// Sectors Per Track
#define	TRACKS		77	// Tracks
#define TRACK_LEN	BPS * SPT

int cmdStat(int drive, int hlf, int track, void *buffer);
int cmdRead(int drive, int track, int length, void *buffer);
int cmdWrit(int drive, int track, int length, void *buffer);

char *port;
int baud = B460800;
char buffer[MAX_TRACK_LEN];

/*
** Flag Variables
*/
int f_help = 0;
int f_verbose = 0;    /* flag variables */

struct option longopts[] = {
   { "port",	required_argument,	NULL,		'p' },
   { "baud",	required_argument,	NULL,		'b' },
   { "help",	no_argument,     	&f_help,	1 },
   { "verbose",	no_argument,		&f_verbose,	1 },
   { 0, 0, 0, 0 }
};

int main(int argc, char **argv)
{
	int running = 1;
	int track;
	int bytes;
	int c;

	while ((c = getopt_long(argc, argv, ":p:b:vh", longopts, NULL)) != -1) {
		switch (c) {
			case 'p':
				port = optarg;
				printf("Using Port: %s\n", optarg);
				break;

			case 'b':
				baud = atoi(optarg);
				printf("Baud: %d\n", baud);
				break;

			case 'v':
				f_verbose = 1;
				break;

			case 'h':
				f_help = 1;
				break;

			case 0:
				/* getopt_long() set a variable, just keep going */
				break;
		}
	}
   
	displayInit();

	if(openPort(port, baud) == -1) {
		displayReset();
		perror("Unable to open serial port");

		exit(errno);
	}


	for (track = 0; track < 79 && running; track++) {
		switch(toupper(displayGetch())) {
			case 'C':
				displayError("", 0);
				break;

			case 'Q':
				running = 0;
				break;

			default:
				break;
		}

		cmdStat(0, 1, track, buffer);
		bytes = cmdRead(0, track, TRACK_LEN, buffer);
		cmdStat(0, 0, track, buffer);
		if (bytes == TRACK_LEN) {
			cmdStat(3, 1, track, buffer);
			cmdWrit(3, track, TRACK_LEN, buffer);
			cmdStat(3, 0, track, buffer);
		}
		else {
			displayError("INVALID TRACK LENGTH", 0);
		}
	}

	for (track = 0; track < 79 && running; track++) {
		switch(toupper(displayGetch())) {
			case 'C':
				displayError("", 0);
				break;

			case 'Q':
				running = 0;
				break;

			default:
				break;
		}

		cmdStat(0, 1, track, buffer);
		cmdStat(0, 0, track, buffer);
		cmdStat(1, 1, track, buffer);
		cmdStat(1, 0, track, buffer);
		cmdStat(2, 1, track, buffer);
		cmdStat(2, 0, track, buffer);
		cmdStat(3, 1, track, buffer);
		cmdStat(3, 0, track, buffer);
		cmdStat(4, 1, track, buffer);
		cmdStat(4, 0, track, buffer);
	}

	displayReset();

	return 0;
}

int cmdStat(int drive, int hlf, int track, void *buffer)
{
	int bytes;
	crblk_t	cmd = {'S', 'T', 'A', 'T'};

	cmd.lsb1 = drive;
	cmd.msb1 = hlf;
	cmd.lsb2 = track & 0xff;
	cmd.msb2 = (track & 0xff00) >> 8;

	sendBuf(&cmd, sizeof(cmd), 5);

	/*
	** Wait for response from server
	*/
	bytes = recvBuf(&cmd, sizeof(cmd), 5);

	return 0;
}

int cmdRead(int drive, int track, int length, void *buffer)
{
	int bytes;
	crblk_t	cmd = {'R', 'E', 'A', 'D'};

	cmd.lsb1 = track & 0xff;
	cmd.msb1 = (track & 0xff00) >> 8;
	cmd.msb1 |= (drive << 4);
	cmd.lsb2 = length & 0xff;
	cmd.msb2 = (length & 0xff00) >> 8;

	sendBuf(&cmd, sizeof(cmd), 5);

	/*
	** Wait for response from server
	*/
	bytes = recvBuf(buffer, length, 5);

	return bytes;
}

int cmdWrit(int drive, int track, int length, void *buffer)
{
	int bytes;
	crblk_t	cmd = {'W', 'R', 'I', 'T'};

	cmd.lsb1 = track & 0xff;
	cmd.msb1 = (track & 0xff00) >> 8;
	cmd.msb1 |= (drive << 4);
	cmd.lsb2 = length & 0xff;
	cmd.msb2 = (length & 0xff00) >> 8;

	sendBuf(&cmd, sizeof(cmd), 5);

	/*
	** Wait for write ready
	*/
	bytes = recvBuf(&cmd, sizeof(crblk_t), 5);

	/*
	** Send track
	*/
	sendBuf(buffer, length, 5);

	/*
	** Wait for write status
	*/
	bytes = recvBuf(&cmd, sizeof(crblk_t), 5);

	return 0;
}

