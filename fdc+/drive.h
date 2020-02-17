#ifndef	_DISK_H
#define	_DISK_H

#define	MAX_DRIVES	16
#define	MAX_TRACK_LEN	(137*32)
#define	MAX_PATH	128

typedef struct DRVSTAT {
	int		fd;
	char		filename[MAX_PATH];
	uint8_t		mounted;
	uint8_t		readonly;
	uint8_t		hdld;
	uint16_t	track;
} drvstat_t;

extern drvstat_t drvstat[MAX_DRIVES];
extern uint8_t trackbuf[MAX_TRACK_LEN];

extern int mountDrive(int drive, char *filename);
extern int unmountDrive(int drive);
extern void unmountAll();
extern int writeProtect(int drive, int flag);
extern int readTrack(int drive, int track, int length, void *buffer);
extern int writeTrack(int drive, int track, int length, void *buffer);

#endif
