#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <getopt.h>
#include <errno.h>
#include <termios.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/select.h>
#include "io.h"
#include "fdc.h"
#include "drive.h"
#include "display.h"

static void help();
static int processCmd(crblk_t *cmd);
static int statCmd(crblk_t *cmd);
static int readCmd(crblk_t *cmd);
static int writCmd(crblk_t *cmd);
static int writResp(crblk_t *cmd, int resp);

char *port = NULL;
int baud = B460800;

/*
** Flag Variables
*/
int f_help = FALSE;
int f_verbose = FALSE;

struct option longopts[] = {
   { "port",	required_argument,	NULL,		'p' },
   { "baud",	required_argument,	NULL,		'b' },
   { "load0",	required_argument,	NULL,		'0' },
   { "load1",	required_argument,	NULL,		'1' },
   { "load2",	required_argument,	NULL,		'2' },
   { "load3",	required_argument,	NULL,		'3' },
   { "ro",	required_argument,	NULL,		'r' },
   { "help",	no_argument,     	&f_help,	1 },
   { "verbose",	no_argument,		&f_verbose,	1 },
   { 0, 0, 0, 0 }
};

int main(int argc, char **argv)
{
	int running = TRUE;
	int bytes;
	crblk_t	cmd;
	int c;

	memset(&drvstat, 0, sizeof(drvstat));

	displayInit();

	while ((c = getopt_long(argc, argv, ":0:1:2:3:p:b:r:vh", longopts, NULL)) != -1) {
		switch (c) {
			case '0':
			case '1':
			case '2':
			case '3':
				mountDrive(c-'0', optarg);
				break;

			case 'r':
				writeProtect(atoi(optarg), TRUE);
				break;

			case 'p':
				port = optarg;
				break;

			case 'b':
				baud = atoi(optarg);
				break;

			case 'v':
				f_verbose = TRUE;
				break;

			case 'h':
				f_help = TRUE;
				break;

			case 0:
				/* getopt_long() set a variable, just keep going */
				break;
		}
	}

	if (f_help) {
		displayReset();
		help();
		exit(1);
	}

	if (port == NULL) {
		displayReset();
		help();
		printf("You must specify a serial port with '-p' option.\n\n");
		exit(1);
	}

	if(openPort(port, baud) == -1) {
		displayReset();
		perror("Unable to open serial port");
		exit(errno);
	}

	/*
	** Command loop
	*/
	while (running) {

		switch(toupper(displayGetch())) {
			case 'C':
				displayError("", 0);
				break;

			case 'Q':
				running = FALSE;
				break;

			case 'V':
				f_verbose = !f_verbose;
				break;

			default:
				break;
		}

		/*
		** Wait for command from FDC+
		*/
		bytes = recvBuf(&cmd, sizeof(cmd), 1);

		if (bytes && f_verbose) {
			displayBuffer("", &cmd, sizeof(cmd));
		}

		if (bytes == sizeof(cmd)) {
			processCmd(&cmd);
		}
		else {
			displayCommand("----");
			displayBlock(0xff, 0, 0);
		}
	}

	closePort();
	unmountAll();
	displayReset();
}

static void help()
{
	printf("\n%s\n", FDC_NAME);
	printf("%s\n\n", FDC_COPYRIGHT);
	printf("Serial Disk Server compatible with the FDC+ Enhanced Floppy Disk\n");
	printf("Controller for the Altair 8800 available at http://www.deramp.com\n\n");
	printf("server [options] -p port\n\n");
	printf("Options:\n");
	printf("-[0-3] file\tMount disk image file to drive 0-3\n");
	printf("\t\tThe FDC+ supports 330K 8 inch and 75K Minidisk images\n");
	printf("-b baud\t\tSet serial port speed (default=460800)\n");
	printf("-p port\t\tSerial port (required)\n");
	printf("-r [0-3]\tMake drive 0-3 read only\n");
	printf("-v\t\tVerbose display\n\n");
}

static int processCmd(crblk_t *cmd)
{
	if (!strncmp(cmd->cmd, "STAT", 4)) {
		statCmd(cmd);
	}
	else if (!strncmp(cmd->cmd, "READ", 4)) {
		readCmd(cmd);
	}
	else if (!strncmp(cmd->cmd, "WRIT", 4)) {
		writCmd(cmd);
	}

	return -1;
}

static int statCmd(crblk_t *cmd)
{
	int i;
	uint16_t data = 0;

	displayCommand("STAT");
	displayBlock(0xff, 0, 0);

	/*
	** Save head load status and track for drive
	*/
	if (cmd->lsb1 < MAX_DRIVES) {
		drvstat[cmd->lsb1].hdld = cmd->msb1;
		drvstat[cmd->lsb1].track = WORD(cmd->lsb2, cmd->msb2);

		displayHead(cmd->lsb1, drvstat[cmd->lsb1].hdld);
		displayTrack(cmd->lsb1, drvstat[cmd->lsb1].track);
	}

	for (i=0; i < MAX_DRIVES; i++) {
		data <<= 1;
		data |= drvstat[i].mounted;
	}

	cmd->lsb2 = LSB(data);
	cmd->msb2 = MSB(data);

	if (f_verbose) {
		displayBuffer("", cmd, sizeof(crblk_t));
	}

	return (sendBuf(cmd, sizeof(crblk_t), 5));
}

static int readCmd(crblk_t *cmd)
{
	int length;
	int drive;
	int track;
	int bytes;

	displayCommand("READ");

	drive = cmd->msb1 >> 4;
	track = WORD(cmd->lsb1, cmd->msb1) & 0x0fff;
	length = WORD(cmd->lsb2, cmd->msb2);

	if (f_verbose) {
		displayBlock(drive, track, length);
	}

	if (drive < MAX_DRIVES) {
		drvstat[drive].track = track;
	}

	if (readTrack(drive, track, length, trackbuf) != length) {
		return (-1);
	}

	if (f_verbose) {
		displayBuffer("", trackbuf, length);
	}

	return (sendBuf(trackbuf, length, 5));
}

static int writCmd(crblk_t *cmd)
{
	int length;
	int drive;
	int track;

	displayCommand("WRIT");

	drive = cmd->msb1 >> 4;
	track = WORD(cmd->lsb1, cmd->msb1) & 0x0fff;
	length = WORD(cmd->lsb2, cmd->msb2);

	if (f_verbose) {
		displayBlock(drive, track, length);
	}

	if (drive >= MAX_DRIVES) {
		return(writResp(cmd, FDC_NOT_READY));
	}

	drvstat[drive].track = track;

	writResp(cmd, FDC_OK);

	/*
	** Wait for track
	*/
	if (recvBuf(trackbuf, length, 5) == 0) {
		displayError("RECEIVE TIMEOUT", 0);
	}

	if (f_verbose) {
		displayBuffer("", trackbuf, length);
	}

	memcpy(cmd->cmd, "WSTA", 4);

	if (writeTrack(drive, track, length, trackbuf) == length) {
		writResp(cmd, FDC_OK);
	}
	else {
		writResp(cmd, FDC_WRITE_ERR);
		return(-1);
	}

	return(0);
}

static int writResp(crblk_t *cmd, int resp)
{
	cmd->lsb1 = LSB(resp);
	cmd->msb1 = MSB(resp);

	if (f_verbose) {
		displayBuffer("", cmd, sizeof(crblk_t));
	}

	/*
	** Send WRIT response
	*/
	return(sendBuf(cmd, sizeof(crblk_t), 5));
}

