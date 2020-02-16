#include <string.h>
#include <ctype.h>
#include <libgen.h>
#include <ncurses.h>
#include <sys/errno.h>
#include "display.h"
#include "fdc.h"
#include "drive.h"

#define	MAX_LINE	LINES-1
#define MAX_COL		COLS-1

#define TITLE_LINE	MAX_LINE
#define	TITLE_COL	0

#define COPY_LINE	MAX_LINE
#define	COPY_COL	MAX_COL-30

#define	PORT_LINE	0
#define PORT_COL	0
#define PORT_TEXT	"PORT:"

#define	BAUD_LINE	0
#define BAUD_COL	PORT_COL+sizeof(PORT_TEXT)+20
#define BAUD_TEXT	"BAUD RATE:"

#define COMMAND_LINE	0
#define COMMAND_COL	BAUD_COL+sizeof(BAUD_TEXT)+14
#define	COMMAND_TEXT	"COMMAND:"

#define	BLOCK_LINE	COMMAND_LINE
#define BLOCK_COL	COMMAND_COL + sizeof(COMMAND_TEXT) + 7

#define	DRIVE_LINE	2
#define	DRIVE_COL	0
#define	DRIVE_TEXT	"Disk -                               Disk Enable -  Head Load -  Track --  RO -"
#define	DRIVE_NUM	5
#define	DRIVE_FILE	8
#define	DRIVE_ENA	49
#define	DRIVE_HEAD	62
#define	DRIVE_TRACK	71
#define	DRIVE_RO	78

#define ERROR_LINE	DRIVE_LINE + 5
#define ERROR_COL	0
#define	ERROR_TEXT	"ERROR:"

#define	BUFFER_LINE	MAX_LINE-12
#define	BUFFER_COL	0

void displayInit()
{
	int d;

	initscr();

	LINES = 24;
	COLS = 80;

	raw();
	noecho();
	curs_set(0);
	timeout(0);

	clear();

	move(TITLE_LINE, TITLE_COL);
	printw("FDC+ Serial Drive Server v1.0");
	move(COPY_LINE, COPY_COL);
	printw("(C) 2020 Deltec Enterprises LLC");

	move(PORT_LINE, PORT_COL);
	printw(PORT_TEXT);

	move(BAUD_LINE, BAUD_COL);
	printw(BAUD_TEXT);

	move(COMMAND_LINE, COMMAND_COL);
	printw(COMMAND_TEXT);

	move(ERROR_LINE, ERROR_COL);
	printw(ERROR_TEXT);

	for (d = 0; d < 4; d++) {
		move(DRIVE_LINE + d, DRIVE_COL);
		printw(DRIVE_TEXT);
		move(DRIVE_LINE + d, DRIVE_NUM);
		printw("%d", d);
	}

	refresh();
}

void displayReset()
{
	endwin();
}

int displayGetch()
{
	return getch();
}

void displayPort(char *port)
{
	move(PORT_LINE, PORT_COL+sizeof(PORT_TEXT));
	printw("%-20.20s", basename(port));
	refresh();
}

void displayBaud(int baud)
{
	move(BAUD_LINE, BAUD_COL+sizeof(BAUD_TEXT));
	printw("%d", baud);
	refresh();
}

void displayCommand(char *command)
{
	move(COMMAND_LINE, COMMAND_COL+sizeof(COMMAND_TEXT));
	printw("%-4.4s", command);
	clrtoeol();
	refresh();
}

void displayBlock(int drive, int track, int length)
{
	move(BLOCK_LINE, BLOCK_COL);
	printw("D:%02d T:%02d L:%04d", drive, track, length);
	refresh();
}

void displayError(char *string, int err)
{
	char error[80];

	strcpy(error, string);

	if (err) {
		strcat(error, " (");
		strcat(error, strerror(err));
		strcat(error, ")");

		/*
		** Reset errno
		*/
		errno = 0;
	}

	move(ERROR_LINE, ERROR_COL+sizeof(ERROR_TEXT));
	printw("%-60.60s", error);
	refresh();
}

void displayHead(int drive, int head)
{
	int d;

	if (drive < 0 || drive > 3) {
		return;
	}

	for (d = 0; d < 4; d++) {
		move(DRIVE_LINE + d, DRIVE_ENA);
		printw("%c", (d == drive) ? '*' : '-');

		move(DRIVE_LINE + d, DRIVE_HEAD);
		if (d == drive) {
			printw("%c", (head) ? '*' : '-');
		}
		else {
			printw("-");
		}
	}

	refresh();
}

void displayTrack(int drive, int track)
{
	if (drive < 0 || drive > 3) {
		return;
	}

	move(DRIVE_LINE + drive, DRIVE_TRACK);
	printw("%02d", track);

	refresh();
}

void displayMount(int drive, char *path)
{
	if (drive < 0 || drive > 3) {
		return;
	}

	move(DRIVE_LINE + drive, DRIVE_FILE);
	printw("%-25.25s", (path != NULL) ? path : "");

	refresh();
}

void displayRO(int drive, int wp)
{
	if (drive < 0 || drive > 3) {
		return;
	}

	move(DRIVE_LINE + drive, DRIVE_RO);
	printw("%c", (wp) ? '*' : '-');

	refresh();
}

void displayBuffer(char *prefix, void *buffer, int length)
{
	int i;
	int maxlen;
	int l = BUFFER_LINE;
	int c = BUFFER_COL;

	if (!length) {
		return;
	}

	for (i = BUFFER_LINE; i < BUFFER_LINE + 10; i++) {
		move(i, 5);
		clrtoeol();
	}

	maxlen = (length > 160) ? 160 : length;

	for (i=0; i<maxlen; i++) {
		if (!(i % 16)) {
			if (i) {
				l++;
				c = 0;
			}
			move(l, c);
			printw("%04X: ", i);
		}

		move(l, 6 + (i % 16) * 3);
		printw("%02X ", * (uint8_t *) buffer);
		move(l, 54 + i % 16);
		if (isprint(* (uint8_t *) buffer)) {
			printw("%c", * (uint8_t *) buffer);
		}
		else {
			printw(".");
		}

		buffer++;
	}

	refresh();
}
