#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <termios.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/select.h>
#include "fdc.h"
#include "drive.h"
#include "display.h"

drvstat_t drvstat[MAX_DRIVES];
uint8_t trackbuf[MAX_TRACK_LEN];

int mountDrive(int drive, char *filename)
{
	if (drive >= MAX_DRIVES) {
		return -1;
	}

	strncpy(drvstat[drive].filename, filename, MAX_PATH);

	if ((drvstat[drive].fd = open(filename, O_RDWR)) == -1) {
		displayError("MOUNT", errno);
	}
	else {
		drvstat[drive].mounted = TRUE;
		displayMount(drive, filename);
	}

	return drvstat[drive].fd;
}

int unmountDrive(int drive)
{
	if (drive >= MAX_DRIVES) {
		return -1;
	}

	if (drvstat[drive].mounted && drvstat[drive].fd != -1) {
		close(drvstat[drive].fd);

		drvstat[drive].mounted = FALSE;
		drvstat[drive].fd = -1;

		displayMount(drive, NULL);
	}

	return 0;
}

int writeProtect(int drive, int flag)
{
	if (drive >= MAX_DRIVES) {
		return -1;
	}

	drvstat[drive].readonly = (flag) ? TRUE : FALSE;

	displayRO(drive, flag);

	return (0);
}

int readTrack(int drive, int track, int length, void *buffer)
{
	int offset;

	if (drive >= MAX_DRIVES) {
		displayError("INVALID DRIVE", 0);
		return -1;
	}

	if (drvstat[drive].mounted && drvstat[drive].fd == -1) {
		displayError("DISK NOT MOUNTED", 0);
		return -1;
	}

	offset = track * length;

	displayTrack(drive, track);

	/*
	** Seek to track
	*/
	if (lseek(drvstat[drive].fd, offset, SEEK_SET) != offset) {
		displayError("LSEEK", errno);
		return(-1);
	}

	/*
	** Read track
	*/
	return (read(drvstat[drive].fd, buffer, length));
}

int writeTrack(int drive, int track, int length, void *buffer)
{
	int offset;

	if (drive >= MAX_DRIVES) {
		displayError("INVALID DRIVE", 0);
		return (-1);
	}

	if (drvstat[drive].readonly) {
		displayError("DISK READ ONLY", 0);
		return (-1);
	}

	if (drvstat[drive].mounted && drvstat[drive].fd == -1) {
		displayError("DISK NOT MOUNTED", 0);
		return (-1);
	}

	offset = track * length;

	displayTrack(drive, track);

	/*
	** Seek to track
	*/
	if (lseek(drvstat[drive].fd, offset, SEEK_SET) != offset) {
		displayError("LSEEK", errno);
		return (-1);
	}

	/*
	** Write track
	*/
	return (write(drvstat[drive].fd, buffer, length));
}

void showStatus()
{
	int drive;

	for (drive = 0; drive < MAX_DRIVES; drive++) {
//		printf("Drive %02d: ", drive);
//		printf("mounted=%d ", drvstat[drive].mounted);
//		printf("track=%02d ", drvstat[drive].track);
//		printf("readonly=%d ", drvstat[drive].readonly);
//		printf("file=%s\n", drvstat[drive].filename);
	}
}

