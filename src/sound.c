/* sound.c -- sound support.
   Copyright (C) 1998 Free Software Foundation.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Written by Gerd Moellmann <gerd@gnu.org>.  Tested with Luigi's
   driver on FreeBSD 2.2.7 with a SoundBlaster 16.  */

#include <config.h>

#if defined HAVE_SOUND

#include <lisp.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <dispextern.h>
#include <errno.h>

/* FreeBSD has machine/soundcard.h.  Voxware sound driver docs mention
   sys/soundcard.h.  So, let's try whatever's there.  */

#ifdef HAVE_MACHINE_SOUNDCARD_H
#include <machine/soundcard.h>
#endif
#ifdef HAVE_SYS_SOUNDCARD_H
#include <sys/soundcard.h>
#endif

#define max(X, Y) ((X) > (Y) ? (X) : (Y))
#define min(X, Y) ((X) < (Y) ? (X) : (Y))
#define abs(X)    ((X) < 0 ? -(X) : (X))

/* Structure forward declarations.  */

struct sound_file;
struct sound_device;

/* The file header of RIFF-WAVE files (*.wav).  Files are always in
   little-endian byte-order.  */

struct wav_header
{
  u_int32_t magic;
  u_int32_t length;
  u_int32_t chunk_type;
  u_int32_t chunk_format;
  u_int32_t chunk_length;
  u_int16_t format;
  u_int16_t channels;
  u_int32_t sample_rate;
  u_int32_t bytes_per_second;
  u_int16_t sample_size;
  u_int16_t precision;
  u_int32_t chunk_data;
  u_int32_t data_length;
};

/* The file header of Sun adio files (*.au).  Files are always in
   big-endian byte-order.  */

struct au_header
{
  /* ASCII ".snd" */
  u_int32_t magic_number;
  
  /* Offset of data part from start of file. Minimum value is 24.  */
  u_int32_t data_offset;
  
  /* Size of data part, 0xffffffff if unknown.  */
  u_int32_t data_size;

  /* Data encoding format.
     1	8-bit ISDN u-law
     2  8-bit linear PCM (REF-PCM)
     3  16-bit linear PCM
     4	24-bit linear PCM
     5	32-bit linear PCM
     6	32-bit IEEE floating-point
     7	64-bit IEEE floating-point
     23 8-bit u-law compressed using CCITT 0.721 ADPCM voice data
     encoding scheme.  */
  u_int32_t encoding;

  /* Number of samples per second.  */
  u_int32_t sample_rate;

  /* Number of interleaved channels.  */
  u_int32_t channels;
};

/* Maximum of all sound file headers sizes.  */

#define MAX_SOUND_HEADER_BYTES \
     max (sizeof (struct wav_header), sizeof (struct au_header))

/* Interface structure for sound devices.  */

struct sound_device
{
  /* The name of the device or null meaning use a default device name.  */
  char *file;

  /* File descriptor of the device.  */
  int fd;

  /* Device-dependent format.  */
  int format;

  /* Volume (0..100).  Zero means unspecified.  */
  int volume;

  /* Sample size.  */
  int sample_size;

  /* Sample rate.  */
  int sample_rate;

  /* Bytes per second.  */
  int bps;

  /* 1 = mono, 2 = stereo, 0 = don't set.  */
  int channels;
  
  /* Open device SD.  */
  void (* open) P_ ((struct sound_device *sd));

  /* Close device SD.  */
  void (* close) P_ ((struct sound_device *sd));

  /* Configure SD accoring to device-dependent parameters.  */
  void (* configure) P_ ((struct sound_device *device));
  
  /* Choose a device-dependent format for outputting sound file SF.  */
  void (* choose_format) P_ ((struct sound_device *sd,
			      struct sound_file *sf));

  /* Write NYBTES bytes from BUFFER to device SD.  */
  void (* write) P_ ((struct sound_device *sd, char *buffer, int nbytes));

  /* A place for devices to store additional data.  */
  void *data;
};

/* An enumerator for each supported sound file type.  */

enum sound_type
{
  RIFF,
  SUN_AUDIO
};

/* Interface structure for sound files.  */

struct sound_file
{
  /* The type of the file.  */
  enum sound_type type;

  /* File descriptor of the file.  */
  int fd;

  /* Pointer to sound file header.  This contains the first
     MAX_SOUND_HEADER_BYTES read from the file.  */
  char *header;

  /* Play sound file SF on device SD.  */
  void (* play) P_ ((struct sound_file *sf, struct sound_device *sd)); 
};

/* Indices of attributes in a sound attributes vector.  */

enum sound_attr
{
  SOUND_FILE,
  SOUND_DEVICE,
  SOUND_VOLUME,
  SOUND_ATTR_SENTINEL
};

/* Symbols.  */

extern Lisp_Object QCfile;
Lisp_Object QCvolume, QCdevice;
Lisp_Object Qsound;
Lisp_Object Qplay_sound_hook;

/* These are set during `play-sound' so that sound_cleanup has
   access to them.  */

struct sound_device *sound_device;
struct sound_file *sound_file;

/* Function prototypes.  */

static void vox_open P_ ((struct sound_device *));
static void vox_configure P_ ((struct sound_device *));
static void vox_close P_ ((struct sound_device *sd));
static void vox_choose_format P_ ((struct sound_device *, struct sound_file *));
static void vox_init P_ ((struct sound_device *));
static void vox_write P_ ((struct sound_device *, char *, int));
static void sound_perror P_ ((char *));
static int parse_sound P_ ((Lisp_Object, Lisp_Object *));
static void find_sound_file_type P_ ((struct sound_file *));
static u_int32_t le2hl P_ ((u_int32_t));
static u_int16_t le2hs P_ ((u_int16_t));
static u_int32_t be2hl P_ ((u_int32_t));
static u_int16_t be2hs P_ ((u_int16_t));
static int wav_init P_ ((struct sound_file *));
static void wav_play P_ ((struct sound_file *, struct sound_device *));
static int au_init P_ ((struct sound_file *));
static void au_play P_ ((struct sound_file *, struct sound_device *));



/***********************************************************************
			       General
 ***********************************************************************/

/* Like perror, but signals an error.  */

static void
sound_perror (msg)
     char *msg;
{
  error ("%s: %s", msg, strerror (errno));
}


/* Parse sound specification SOUND, and fill ATTRS with what is
   found.  Value is non-zero if SOUND Is a valid sound specification.
   A valid sound specification is a list starting with the symbol
   `sound'.  The rest of the list is a property list which may
   contain the following key/value pairs:

   - `:file FILE'

   FILE is the sound file to play.  If it isn't an absolute name,
   it's searched under `data-directory'.

   - `:device DEVICE'

   DEVICE is the name of the device to play on, e.g. "/dev/dsp2".
   If not specified, a default device is used.

   - `:volume VOL'

   VOL must be an integer in the range 0..100.  */

static int
parse_sound (sound, attrs)
     Lisp_Object sound;
     Lisp_Object *attrs;
{
  /* SOUND must be a list starting with the symbol `sound'.  */
  if (!CONSP (sound) || !EQ (XCAR (sound), Qsound))
    return 0;

  sound = XCDR (sound);
  attrs[SOUND_FILE] = Fplist_get (sound, QCfile);
  attrs[SOUND_DEVICE] = Fplist_get (sound, QCdevice);
  attrs[SOUND_VOLUME] = Fplist_get (sound, QCvolume);

  /* File name must be specified.  */
  if (!STRINGP (attrs[SOUND_FILE]))
    return 0;

  /* Volume must be in the range 0..100 or unspecified.  */
  if (!NILP (attrs[SOUND_VOLUME]))
    {
      if (!INTEGERP (attrs[SOUND_VOLUME]))
	return 0;
      if (XINT (attrs[SOUND_VOLUME]) < 0
	  || XINT (attrs[SOUND_VOLUME]) > 100)
	return 0;
    }

  /* Device must be a string or unspecified.  */
  if (!NILP (attrs[SOUND_DEVICE])
      && !STRINGP (attrs[SOUND_DEVICE]))
    return 0;

  return 1;
}


/* Find out the type of the sound file whose file descriptor is FD.
   SF is the sound file structure to fill in.  */

static void
find_sound_file_type (sf)
     struct sound_file *sf;
{
  if (!wav_init (sf)
      && !au_init (sf))
    error ("Unknown sound file format");
}


/* Function installed by play-sound with record_unwind_protect.  */

static Lisp_Object
sound_cleanup (arg)
     Lisp_Object arg;
{
  if (sound_device)
    {
      sound_device->close (sound_device);
      if (sound_file->fd > 0)
	close (sound_file->fd);
    }
}


DEFUN ("play-sound", Fplay_sound, Splay_sound, 1, 1, 0,
  "Play sound SOUND.")
  (sound)
     Lisp_Object sound;
{
  Lisp_Object attrs[SOUND_ATTR_SENTINEL];
  char *header;
  Lisp_Object file;
  struct gcpro gcpro1, gcpro2;
  int nbytes;
  char *msg;
  struct sound_device sd;
  struct sound_file sf;
  Lisp_Object args[2];
  int count = specpdl_ptr - specpdl;

  file = Qnil;
  GCPRO2 (sound, file);
  bzero (&sd, sizeof sd);
  bzero (&sf, sizeof sf);
  sf.header = (char *) alloca (MAX_SOUND_HEADER_BYTES);
  
  sound_device = &sd;
  sound_file = &sf;
  record_unwind_protect (sound_cleanup, Qnil);

  /* Parse the sound specification.  Give up if it is invalid.  */
  if (!parse_sound (sound, attrs))
    {
      UNGCPRO;
      error ("Invalid sound specification");
    }

  /* Open the sound file.  */
  sf.fd = openp (Fcons (Vdata_directory, Qnil),
		 attrs[SOUND_FILE], "", &file, 0);
  if (sf.fd < 0)
    sound_perror ("Open sound file");

  /* Read the first bytes from the file.  */
  nbytes = read (sf.fd, sf.header, MAX_SOUND_HEADER_BYTES);
  if (nbytes < 0)
    sound_perror ("Reading sound file header");

  /* Find out the type of sound file.  Give up if we can't tell.  */
  find_sound_file_type (&sf);

  /* Set up a device.  */
  if (STRINGP (attrs[SOUND_DEVICE]))
    {
      int len = XSTRING (attrs[SOUND_DEVICE])->size;
      sd.file = (char *) alloca (len + 1);
      strcpy (sd.file, XSTRING (attrs[SOUND_DEVICE])->data);
    }
  if (INTEGERP (attrs[SOUND_VOLUME]))
    sd.volume = XFASTINT (attrs[SOUND_VOLUME]);

  args[0] = Qplay_sound_hook;
  args[1] = sound;
  Frun_hook_with_args (make_number (2), args);

  vox_init (&sd);
  sd.open (&sd);

  sf.play (&sf, &sd);
  close (sf.fd);
  sf.fd = -1;
  sd.close (&sd);
  sound_device = NULL;
  sound_file = NULL;
  UNGCPRO;
  unbind_to (count, Qnil);
  return Qnil;
}


/***********************************************************************
			Byte-order Conversion
 ***********************************************************************/

/* Convert 32-bit value VALUE which is in little-endian byte-order
   to host byte-order.  */

static u_int32_t
le2hl (value)
     u_int32_t value;
{
#ifdef WORDS_BIG_ENDIAN
  unsigned char *p = (unsigned char *) &value;
  value = p[0] + (p[1] << 8) + (p[2] << 16) + (p[3] << 24);
#endif
  return value;
}


/* Convert 16-bit value VALUE which is in little-endian byte-order
   to host byte-order.  */

static u_int16_t
le2hs (value)
     u_int16_t value;
{
#ifdef WORDS_BIG_ENDIAN
  unsigned char *p = (unsigned char *) &value;
  value = p[0] + (p[1] << 8);
#endif
  return value;
}


/* Convert 32-bit value VALUE which is in big-endian byte-order
   to host byte-order.  */

static u_int32_t
be2hl (value)
     u_int32_t value;
{
#ifndef WORDS_BIG_ENDIAN
  unsigned char *p = (unsigned char *) &value;
  value = p[3] + (p[2] << 8) + (p[1] << 16) + (p[0] << 24);
#endif
  return value;
}


/* Convert 16-bit value VALUE which is in big-endian byte-order
   to host byte-order.  */

static u_int16_t
be2hs (value)
     u_int16_t value;
{
#ifndef WORDS_BIG_ENDIAN
  unsigned char *p = (unsigned char *) &value;
  value = p[1] + (p[0] << 8);
#endif
  return value;
}



/***********************************************************************
			  RIFF-WAVE (*.wav)
 ***********************************************************************/

/* Try to initialize sound file SF from SF->header.  SF->header
   contains the first MAX_SOUND_HEADER_BYTES number of bytes from the
   sound file.  If the file is a WAV-format file, set up interface
   functions in SF and convert header fields to host byte-order.
   Value is non-zero if the file is a WAV file.  */

static int
wav_init (sf)
     struct sound_file *sf;
{
  struct wav_header *header = (struct wav_header *) sf->header;
  
  if (bcmp (sf->header, "RIFF", 4) != 0)
    return 0;

  /* WAV files are in little-endian order.  Convert the header
     if on a big-endian machine.  */
  header->magic = le2hl (header->magic);
  header->length = le2hl (header->length);
  header->chunk_type = le2hl (header->chunk_type);
  header->chunk_format = le2hl (header->chunk_format);
  header->chunk_length = le2hl (header->chunk_length);
  header->format = le2hs (header->format);
  header->channels = le2hs (header->channels);
  header->sample_rate = le2hl (header->sample_rate);
  header->bytes_per_second = le2hl (header->bytes_per_second);
  header->sample_size = le2hs (header->sample_size);
  header->precision = le2hs (header->precision);
  header->chunk_data = le2hl (header->chunk_data);
  header->data_length = le2hl (header->data_length);

  /* Set up the interface functions for WAV.  */
  sf->type = RIFF;
  sf->play = wav_play;

  return 1;
}  


/* Play RIFF-WAVE audio file SF on sound device SD.  */

static void
wav_play (sf, sd)
     struct sound_file *sf;
     struct sound_device *sd;
{
  struct wav_header *header = (struct wav_header *) sf->header;
  char *buffer;
  int nbytes;
  int blksize = 2048;

  /* Let the device choose a suitable device-dependent format
     for the file.  */
  sd->choose_format (sd, sf);
  
  /* Configure the device.  */
  sd->sample_size = header->sample_size;
  sd->sample_rate = header->sample_rate;
  sd->bps = header->bytes_per_second;
  sd->channels = header->channels;
  sd->configure (sd);

  /* Copy sound data to the device.  The WAV file specification is
     actually more complex.  This simple scheme worked with all WAV
     files I found so far.  If someone feels inclined to implement the
     whole RIFF-WAVE spec, please do.  */
  buffer = (char *) alloca (blksize);
  lseek (sf->fd, sizeof *header, SEEK_SET);
  
  while ((nbytes = read (sf->fd, buffer, blksize)) > 0)
    sd->write (sd, buffer, nbytes);

  if (nbytes < 0)
    sound_perror ("Reading sound file");
}



/***********************************************************************
			   Sun Audio (*.au)
 ***********************************************************************/

/* Sun audio file encodings.  */ 

enum au_encoding
{
  AU_ENCODING_ULAW_8 = 1,
  AU_ENCODING_8,
  AU_ENCODING_16,
  AU_ENCODING_24,
  AU_ENCODING_32,
  AU_ENCODING_IEEE32,
  AU_ENCODING_IEEE64,
  AU_COMPRESSED = 23
};


/* Try to initialize sound file SF from SF->header.  SF->header
   contains the first MAX_SOUND_HEADER_BYTES number of bytes from the
   sound file.  If the file is a AU-format file, set up interface
   functions in SF and convert header fields to host byte-order.
   Value is non-zero if the file is an AU file.  */

static int
au_init (sf)
     struct sound_file *sf;
{
  struct au_header *header = (struct au_header *) sf->header;
  
  if (bcmp (sf->header, ".snd", 4) != 0)
    return 0;
  
  header->magic_number = be2hl (header->magic_number);
  header->data_offset = be2hl (header->data_offset);
  header->data_size = be2hl (header->data_size);
  header->encoding = be2hl (header->encoding);
  header->sample_rate = be2hl (header->sample_rate);
  header->channels = be2hl (header->channels);
  
  /* Set up the interface functions for AU.  */
  sf->type = SUN_AUDIO;
  sf->play = au_play;

  return 1;
}


/* Play Sun audio file SF on sound device SD.  */

static void
au_play (sf, sd)
     struct sound_file *sf;
     struct sound_device *sd;
{
  struct au_header *header = (struct au_header *) sf->header;
  int blksize = 2048;
  char *buffer;
  int nbytes;

  sd->sample_size = 0;
  sd->sample_rate = header->sample_rate;
  sd->bps = 0;
  sd->channels = header->channels;
  sd->choose_format (sd, sf);
  sd->configure (sd);
      
  /* Seek */
  lseek (sf->fd, header->data_offset, SEEK_SET);
  
  /* Copy sound data to the device.  */
  buffer = (char *) alloca (blksize);
  while ((nbytes = read (sf->fd, buffer, blksize)) > 0)
    sd->write (sd, buffer, nbytes);

  if (nbytes < 0)
    sound_perror ("Reading sound file");
}



/***********************************************************************
		       Voxware Driver Interface
 ***********************************************************************/

/* This driver is available on GNU/Linux, and the free BSDs.  FreeBSD
   has a compatible own driver aka Luigi's driver.  */


/* Open device SD.  If SD->file is non-null, open that device,
   otherwise use a default device name.  */

static void
vox_open (sd)
     struct sound_device *sd;
{
  char *file;
  
  /* Open the sound device.  Default is /dev/dsp.  */
  if (sd->file)
    file = sd->file;
  else
    file = "/dev/dsp";
  
  sd->fd = open (file, O_WRONLY);
  if (sd->fd < 0)
    sound_perror (file);
}


/* Configure device SD from parameters in it.  */

static void
vox_configure (sd)
     struct sound_device *sd;
{
  int requested;
  
  xassert (sd->fd >= 0);

  /* Device parameters apparently depend on each other in undocumented
     ways (not to imply that there is any real documentation).  Be
     careful when reordering the calls below.  */
  if (sd->sample_size > 0
      && ioctl (sd->fd, SNDCTL_DSP_SAMPLESIZE, &sd->sample_size) < 0)
    sound_perror ("Setting sample size");
  
  if (sd->bps > 0
      && ioctl (sd->fd, SNDCTL_DSP_SPEED, &sd->bps) < 0)
    sound_perror ("Setting speed");

  if (sd->sample_rate > 0
      && ioctl (sd->fd, SOUND_PCM_WRITE_RATE, &sd->sample_rate) < 0)
    sound_perror ("Setting sample rate");

  requested = sd->format;
  if (ioctl (sd->fd, SNDCTL_DSP_SETFMT, &sd->format) < 0)
    sound_perror ("Setting format");
  else if (requested != sd->format)
    error ("Setting format");

  if (sd->channels > 1
      && ioctl (sd->fd, SNDCTL_DSP_STEREO, &sd->channels) < 0)
    sound_perror ("Setting channels");

  if (sd->volume > 0
      && ioctl (sd->fd, SOUND_MIXER_WRITE_PCM, &sd->volume) < 0)
    sound_perror ("Setting volume");
}


/* Close device SD if it is open.  */

static void
vox_close (sd)
     struct sound_device *sd;
{
  if (sd->fd >= 0)
    {
      /* Flush sound data, and reset the device.  */
      ioctl (sd->fd, SNDCTL_DSP_SYNC, NULL);
      ioctl (sd->fd, SNDCTL_DSP_RESET, NULL);

      /* Close the device.  */
      close (sd->fd);
      sd->fd = -1;
    }
}


/* Choose device-dependent format for device SD from sound file SF.  */

static void
vox_choose_format (sd, sf)
     struct sound_device *sd;
     struct sound_file *sf;
{
  if (sf->type == RIFF)
    {
      struct wav_header *h = (struct wav_header *) sf->header;
      if (h->precision == 8)
	sd->format = AFMT_U8;
      else if (h->precision == 16)
	sd->format = AFMT_S16_LE;
      else
	error ("Unsupported WAV file format");
    }
  else if (sf->type == SUN_AUDIO)
    {
      struct au_header *header = (struct au_header *) sf->header;
      switch (header->encoding)
	{
	case AU_ENCODING_ULAW_8:
	case AU_ENCODING_IEEE32:
	case AU_ENCODING_IEEE64:
	  sd->format = AFMT_MU_LAW;
	  break;
	  
	case AU_ENCODING_8:
	case AU_ENCODING_16:
	case AU_ENCODING_24:
	case AU_ENCODING_32:
	  sd->format = AFMT_S16_LE;
	  break;

	default:
	  error ("Unsupported AU file format");
	}
    }
  else
    abort ();
}


/* Initialize device SD.  Set up the interface functions in the device
   structure.  */

static void
vox_init (sd)
     struct sound_device *sd;
{
  sd->fd = -1;
  sd->open = vox_open;
  sd->close = vox_close;
  sd->configure = vox_configure;
  sd->choose_format = vox_choose_format;
  sd->write = vox_write;
}


/* Write NBYTES bytes from BUFFER to device SD.  */

static void
vox_write (sd, buffer, nbytes)
     struct sound_device *sd;
     char *buffer;
     int nbytes;
{
  int nwritten = write (sd->fd, buffer, nbytes);
  if (nwritten < 0)
    sound_perror ("Writing to sound device");
}



/***********************************************************************
			    Initialization
 ***********************************************************************/

void
syms_of_sound ()
{
  QCdevice = intern (":device");
  staticpro (&QCdevice);
  QCvolume = intern (":volume");
  staticpro (&QCvolume);
  Qsound = intern ("sound");
  staticpro (&Qsound);
  Qplay_sound_hook = intern ("play-sound-hook");
  staticpro (&Qplay_sound_hook);

  defsubr (&Splay_sound);
}


void
init_sound ()
{
}

#endif /* HAVE_SOUND */
