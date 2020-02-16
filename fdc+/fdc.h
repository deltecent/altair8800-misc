#ifndef	_FDC_H
#define	_FDC_H

#include <stdlib.h>

#ifndef TRUE
#define	TRUE	1
#endif

#ifndef FALSE
#define FALSE	!TRUE
#endif

#define	LSB(b)		(b & 0xff)
#define	MSB(b)		((b & 0xff00) >> 8)
#define	WORD(lsb,msb)	((msb << 8) | lsb)

#define	FDC_OK		0x00
#define	FDC_NOT_READY	0x01
#define	FDC_CHKSUM_ERR	0x02
#define	FDC_WRITE_ERR	0x03

/*
** FDC+ Command / Response Block
*/
typedef struct CRBLOCK {
	char		cmd[4];

	union {
		struct {
			uint8_t	lsb1;
			uint8_t	msb1;
		};
		uint16_t	param1;
	};

	union {
		struct {
			uint8_t	lsb2;
			uint8_t	msb2;
		};
		uint16_t	param2;
	};
} crblk_t;

#endif
