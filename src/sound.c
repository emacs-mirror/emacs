/* sound.c -- sound support.
   Copyright (C) 1998, 1999, 2001 Free Software Foundation.

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

#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>
#include "lisp.h"
#include "dispextern.h"
#include "atimer.h"
#include <signal.h>
#include "syssignal.h"

#ifndef MSDOS
#include <sys/ioctl.h>
#endif

/* FreeBSD has machine/soundcard.h.  Voxware sound driver docs mention
   sys/soundcard.h.  So, let's try whatever's there.  */

#ifdef HAVE_MACHINE_SOUNDCARD_H
#include <machine/soundcard.h>
#endif
#ifdef HAVE_SYS_SOUNDCARD_H
#include <sys/soundcard.h>
#endif
#ifdef HAVE_SOUNDCARD_H
#include <soundcard.h>
#endif

#ifndef DEFAULT_SOUND_DEVICE
#define DEFAULT_SOUND_DEVICE "/dev/dsp"
#endif

#define abs(X)    ((X) < 0 ? -(X) : (X))

/* Structure forward declarations.  */

struct sound;
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

  /* Choose a device-dependent format for outputting sound S.  */
  void (* choose_format) P_ ((struct sound_device *sd,
			      struct sound *s));

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

struct sound
{
  /* The type of the file.  */
  enum sound_type type;

  /* File descriptor of a sound file.  */
  int fd;

  /* Pointer to sound file header.  This contains header_size bytes
     read from the start of a sound file.  */
  char *header;

  /* Number of bytes raed from sound file.  This is always <=
     MAX_SOUND_HEADER_BYTES.  */
  int header_size;

  /* Sound data, if a string.  */
  Lisp_Object data;

  /* Play sound file S on device SD.  */
  void (* play) P_ ((struct sound *s, struct sound_device *sd));
};

/* Indices of attributes in a sound attributes vector.  */

enum sound_attr
{
  SOUND_FILE,
  SOUND_DATA,
  SOUND_DEVICE,
  SOUND_VOLUME,
  SOUND_ATTR_SENTINEL
};

/* Symbols.  */

extern Lisp_Object QCfile, QCdata;
Lisp_Object QCvolume, QCdevice;
Lisp_Object Qsound;
Lisp_Object Qplay_sound_functions;

/* These are set during `play-sound-internal' so that sound_cleanup has
   access to them.  */

struct sound_device *current_sound_device;
struct sound *current_sound;

/* Function prototypes.  */

static void vox_open P_ ((struct sound_device *));
static void vox_configure P_ ((struct sound_device *));
static void vox_close P_ ((struct sound_device *sd));
static void vox_choose_format P_ ((struct sound_device *, struct sound *));
static void vox_init P_ ((struct sound_device *));
static void vox_write P_ ((struct sound_device *, char *, int));
static void sound_perror P_ ((char *));
static void sound_warning P_ ((char *));
static int parse_sound P_ ((Lisp_Object, Lisp_Object *));
static void find_sound_type P_ ((struct sound *));
static u_int32_t le2hl P_ ((u_int32_t));
static u_int16_t le2hs P_ ((u_int16_t));
static u_int32_t be2hl P_ ((u_int32_t));
static int wav_init P_ ((struct sound *));
static void wav_play P_ ((struct sound *, struct sound_device *));
static int au_init P_ ((struct sound *));
static void au_play P_ ((struct sound *, struct sound_device *));

#if 0 /* Currently not used.  */
static u_int16_t be2hs P_ ((u_int16_t));
#endif



/***********************************************************************
			       General
 ***********************************************************************/

/* Like perror, but signals an error.  */

static void
sound_perror (msg)
     char *msg;
{
  int saved_errno = errno;

  turn_on_atimers (1);
#ifdef SIGIO
  sigunblock (sigmask (SIGIO));
#endif
  if (saved_errno != 0)
    error ("%s: %s", msg, strerror (saved_errno));
  else
    error ("%s", msg);
}


/* Display a warning message.  */

static void
sound_warning (msg)
     char *msg;
{
  message (msg);
}


/* Parse sound specification SOUND, and fill ATTRS with what is
   found.  Value is non-zero if SOUND Is a valid sound specification.
   A valid sound specification is a list starting with the symbol
   `sound'.  The rest of the list is a property list which may
   contain the following key/value pairs:

   - `:file FILE'

   FILE is the sound file to play.  If it isn't an absolute name,
   it's searched under `data-directory'.

   - `:data DATA'

   DATA is a string containing sound data.  Either :file or :data
   may be present, but not both.

   - `:device DEVICE'

   DEVICE is the name of the device to play on, e.g. "/dev/dsp2".
   If not specified, a default device is used.

   - `:volume VOL'

   VOL must be an integer in the range [0, 100], or a float in the
   range [0, 1].  */

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
  attrs[SOUND_DATA] = Fplist_get (sound, QCdata);
  attrs[SOUND_DEVICE] = Fplist_get (sound, QCdevice);
  attrs[SOUND_VOLUME] = Fplist_get (sound, QCvolume);

  /* File name or data must be specified.  */
  if (!STRINGP (attrs[SOUND_FILE])
      && !STRINGP (attrs[SOUND_DATA]))
    return 0;

  /* Volume must be in the range 0..100 or unspecified.  */
  if (!NILP (attrs[SOUND_VOLUME]))
    {
      if (INTEGERP (attrs[SOUND_VOLUME]))
	{
	  if (XINT (attrs[SOUND_VOLUME]) < 0
	      || XINT (attrs[SOUND_VOLUME]) > 100)
	    return 0;
	}
      else if (FLOATP (attrs[SOUND_VOLUME]))
	{
	  if (XFLOAT_DATA (attrs[SOUND_VOLUME]) < 0
	      || XFLOAT_DATA (attrs[SOUND_VOLUME]) > 1)
	    return 0;
	}
      else
	return 0;
    }

  /* Device must be a string or unspecified.  */
  if (!NILP (attrs[SOUND_DEVICE])
      && !STRINGP (attrs[SOUND_DEVICE]))
    return 0;

  return 1;
}


/* Find out the type of the sound file whose file descriptor is FD.
   S is the sound file structure to fill in.  */

static void
find_sound_type (s)
     struct sound *s;
{
  if (!wav_init (s) && !au_init (s))
    error ("Unknown sound format");
}


/* Function installed by play-sound-internal with record_unwind_protect.  */

static Lisp_Object
sound_cleanup (arg)
     Lisp_Object arg;
{
  if (current_sound_device)
    {
      if (current_sound_device->close)
	current_sound_device->close (current_sound_device);
      if (current_sound->fd > 0)
	emacs_close (current_sound->fd);
    }

  return Qnil;
}


DEFUN ("play-sound-internal", Fplay_sound_internal, Splay_sound_internal, 1, 1, 0,
       doc: /* Play sound SOUND.

Internal use only, use `play-sound' instead.  */)
     (sound)
     Lisp_Object sound;
{
  Lisp_Object attrs[SOUND_ATTR_SENTINEL];
  Lisp_Object file;
  struct gcpro gcpro1, gcpro2;
  struct sound_device sd;
  struct sound s;
  Lisp_Object args[2];
  int count = specpdl_ptr - specpdl;

  file = Qnil;
  GCPRO2 (sound, file);
  bzero (&sd, sizeof sd);
  bzero (&s, sizeof s);
  current_sound_device = &sd;
  current_sound = &s;
  record_unwind_protect (sound_cleanup, Qnil);
  s.header = (char *) alloca (MAX_SOUND_HEADER_BYTES);

  /* Parse the sound specification.  Give up if it is invalid.  */
  if (!parse_sound (sound, attrs))
    error ("Invalid sound specification");

  if (STRINGP (attrs[SOUND_FILE]))
    {
      /* Open the sound file.  */
      s.fd = openp (Fcons (Vdata_directory, Qnil),
		    attrs[SOUND_FILE], Qnil, &file, Qnil);
      if (s.fd < 0)
	sound_perror ("Could not open sound file");

      /* Read the first bytes from the file.  */
      s.header_size = emacs_read (s.fd, s.header, MAX_SOUND_HEADER_BYTES);
      if (s.header_size < 0)
	sound_perror ("Invalid sound file header");
    }
  else
    {
      s.data = attrs[SOUND_DATA];
      s.header_size = min (MAX_SOUND_HEADER_BYTES, STRING_BYTES (XSTRING (s.data)));
      bcopy (XSTRING (s.data)->data, s.header, s.header_size);
    }

  /* Find out the type of sound.  Give up if we can't tell.  */
  find_sound_type (&s);

  /* Set up a device.  */
  if (STRINGP (attrs[SOUND_DEVICE]))
    {
      int len = XSTRING (attrs[SOUND_DEVICE])->size;
      sd.file = (char *) alloca (len + 1);
      strcpy (sd.file, XSTRING (attrs[SOUND_DEVICE])->data);
    }

  if (INTEGERP (attrs[SOUND_VOLUME]))
    sd.volume = XFASTINT (attrs[SOUND_VOLUME]);
  else if (FLOATP (attrs[SOUND_VOLUME]))
    sd.volume = XFLOAT_DATA (attrs[SOUND_VOLUME]) * 100;

  args[0] = Qplay_sound_functions;
  args[1] = sound;
  Frun_hook_with_args (2, args);

  /* There is only one type of device we currently support, the VOX
     sound driver.  Set up the device interface functions for that
     device.  */
  vox_init (&sd);

  /* Open the device.  */
  sd.open (&sd);

  /* Play the sound.  */
  s.play (&s, &sd);

  /* Close the input file, if any.  */
  if (!STRINGP (s.data))
    {
      emacs_close (s.fd);
      s.fd = -1;
    }

  /* Close the device.  */
  sd.close (&sd);

  /* Clean up.  */
  current_sound_device = NULL;
  current_sound = NULL;
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


#if 0 /* Currently not used.  */

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

#endif /* 0 */


/***********************************************************************
			  RIFF-WAVE (*.wav)
 ***********************************************************************/

/* Try to initialize sound file S from S->header.  S->header
   contains the first MAX_SOUND_HEADER_BYTES number of bytes from the
   sound file.  If the file is a WAV-format file, set up interface
   functions in S and convert header fields to host byte-order.
   Value is non-zero if the file is a WAV file.  */

static int
wav_init (s)
     struct sound *s;
{
  struct wav_header *header = (struct wav_header *) s->header;

  if (s->header_size < sizeof *header
      || bcmp (s->header, "RIFF", 4) != 0)
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
  s->type = RIFF;
  s->play = wav_play;

  return 1;
}


/* Play RIFF-WAVE audio file S on sound device SD.  */

static void
wav_play (s, sd)
     struct sound *s;
     struct sound_device *sd;
{
  struct wav_header *header = (struct wav_header *) s->header;

  /* Let the device choose a suitable device-dependent format
     for the file.  */
  sd->choose_format (sd, s);

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
  if (STRINGP (s->data))
    sd->write (sd, XSTRING (s->data)->data + sizeof *header,
	       STRING_BYTES (XSTRING (s->data)) - sizeof *header);
  else
    {
      char *buffer;
      int nbytes;
      int blksize = 2048;

      buffer = (char *) alloca (blksize);
      lseek (s->fd, sizeof *header, SEEK_SET);

      while ((nbytes = emacs_read (s->fd, buffer, blksize)) > 0)
	sd->write (sd, buffer, nbytes);

      if (nbytes < 0)
	sound_perror ("Error reading sound file");
    }
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


/* Try to initialize sound file S from S->header.  S->header
   contains the first MAX_SOUND_HEADER_BYTES number of bytes from the
   sound file.  If the file is a AU-format file, set up interface
   functions in S and convert header fields to host byte-order.
   Value is non-zero if the file is an AU file.  */

static int
au_init (s)
     struct sound *s;
{
  struct au_header *header = (struct au_header *) s->header;

  if (s->header_size < sizeof *header
      || bcmp (s->header, ".snd", 4) != 0)
    return 0;

  header->magic_number = be2hl (header->magic_number);
  header->data_offset = be2hl (header->data_offset);
  header->data_size = be2hl (header->data_size);
  header->encoding = be2hl (header->encoding);
  header->sample_rate = be2hl (header->sample_rate);
  header->channels = be2hl (header->channels);

  /* Set up the interface functions for AU.  */
  s->type = SUN_AUDIO;
  s->play = au_play;

  return 1;
}


/* Play Sun audio file S on sound device SD.  */

static void
au_play (s, sd)
     struct sound *s;
     struct sound_device *sd;
{
  struct au_header *header = (struct au_header *) s->header;

  sd->sample_size = 0;
  sd->sample_rate = header->sample_rate;
  sd->bps = 0;
  sd->channels = header->channels;
  sd->choose_format (sd, s);
  sd->configure (sd);

  if (STRINGP (s->data))
    sd->write (sd, XSTRING (s->data)->data + header->data_offset,
	       STRING_BYTES (XSTRING (s->data)) - header->data_offset);
  else
    {
      int blksize = 2048;
      char *buffer;
      int nbytes;

      /* Seek */
      lseek (s->fd, header->data_offset, SEEK_SET);

      /* Copy sound data to the device.  */
      buffer = (char *) alloca (blksize);
      while ((nbytes = emacs_read (s->fd, buffer, blksize)) > 0)
	sd->write (sd, buffer, nbytes);

      if (nbytes < 0)
	sound_perror ("Error reading sound file");
    }
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
    file = DEFAULT_SOUND_DEVICE;

  sd->fd = emacs_open (file, O_WRONLY, 0);
  if (sd->fd < 0)
    sound_perror (file);
}


/* Configure device SD from parameters in it.  */

static void
vox_configure (sd)
     struct sound_device *sd;
{
  int val;

  xassert (sd->fd >= 0);

  /* On GNU/Linux, it seems that the device driver doesn't like to be
     interrupted by a signal.  Block the ones we know to cause
     troubles.  */
  turn_on_atimers (0);
#ifdef SIGIO
  sigblock (sigmask (SIGIO));
#endif

  val = sd->format;
  if (ioctl (sd->fd, SNDCTL_DSP_SETFMT, &sd->format) < 0
      || val != sd->format)
    sound_perror ("Could not set sound format");

  val = sd->channels != 1;
  if (ioctl (sd->fd, SNDCTL_DSP_STEREO, &val) < 0
      || val != (sd->channels != 1))
    sound_perror ("Could not set stereo/mono");

  /* I think bps and sampling_rate are the same, but who knows.
     Check this. and use SND_DSP_SPEED for both.  */
  if (sd->sample_rate > 0)
    {
      val = sd->sample_rate;
      if (ioctl (sd->fd, SNDCTL_DSP_SPEED, &sd->sample_rate) < 0)
	sound_perror ("Could not set sound speed");
      else if (val != sd->sample_rate)
	sound_warning ("Could not set sample rate");
    }

  if (sd->volume > 0)
    {
      int volume = sd->volume & 0xff;
      volume |= volume << 8;
      /* This may fail if there is no mixer.  Ignore the failure.  */
      ioctl (sd->fd, SOUND_MIXER_WRITE_PCM, &volume);
    }

  turn_on_atimers (1);
#ifdef SIGIO
  sigunblock (sigmask (SIGIO));
#endif
}


/* Close device SD if it is open.  */

static void
vox_close (sd)
     struct sound_device *sd;
{
  if (sd->fd >= 0)
    {
      /* On GNU/Linux, it seems that the device driver doesn't like to
	 be interrupted by a signal.  Block the ones we know to cause
	 troubles.  */
#ifdef SIGIO
      sigblock (sigmask (SIGIO));
#endif
      turn_on_atimers (0);

      /* Flush sound data, and reset the device.  */
      ioctl (sd->fd, SNDCTL_DSP_SYNC, NULL);

      turn_on_atimers (1);
#ifdef SIGIO
      sigunblock (sigmask (SIGIO));
#endif

      /* Close the device.  */
      emacs_close (sd->fd);
      sd->fd = -1;
    }
}


/* Choose device-dependent format for device SD from sound file S.  */

static void
vox_choose_format (sd, s)
     struct sound_device *sd;
     struct sound *s;
{
  if (s->type == RIFF)
    {
      struct wav_header *h = (struct wav_header *) s->header;
      if (h->precision == 8)
	sd->format = AFMT_U8;
      else if (h->precision == 16)
	sd->format = AFMT_S16_LE;
      else
	error ("Unsupported WAV file format");
    }
  else if (s->type == SUN_AUDIO)
    {
      struct au_header *header = (struct au_header *) s->header;
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
  int nwritten = emacs_write (sd->fd, buffer, nbytes);
  if (nwritten < 0)
    sound_perror ("Error writing to sound device");
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
  Qplay_sound_functions = intern ("play-sound-functions");
  staticpro (&Qplay_sound_functions);

  defsubr (&Splay_sound_internal);
}


void
init_sound ()
{
}

#endif /* HAVE_SOUND */
