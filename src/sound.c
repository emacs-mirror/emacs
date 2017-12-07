/* sound.c -- sound support.

Copyright (C) 1998-1999, 2001-2017 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by Gerd Moellmann <gerd@gnu.org>.  Tested with Luigi's
   driver on FreeBSD 2.2.7 with a SoundBlaster 16.  */

/*
  Modified by Ben Key <Bkey1@tampabay.rr.com> to add a partial
  implementation of the play-sound specification for Windows.

  Notes:
  In the Windows implementation of play-sound-internal only the
  :file and :volume keywords are supported.  The :device keyword,
  if present, is ignored.  The :data keyword, if present, will
  cause an error to be generated.

  The Windows implementation of play-sound is implemented via the
  Windows API functions mciSendString, waveOutGetVolume, and
  waveOutSetVolume which are exported by Winmm.dll.
*/

#include <config.h>

#if defined HAVE_SOUND

/* BEGIN: Common Includes */
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>

#include "lisp.h"
#include "atimer.h"
#include "syssignal.h"
/* END: Common Includes */


/* BEGIN: Non Windows Includes */
#ifndef WINDOWSNT

#include <byteswap.h>

#include <sys/ioctl.h>

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
#ifdef HAVE_ALSA
#ifdef ALSA_SUBDIR_INCLUDE
#include <alsa/asoundlib.h>
#else
#include <asoundlib.h>
#endif /* ALSA_SUBDIR_INCLUDE */
#endif /* HAVE_ALSA */

/* END: Non Windows Includes */

#else /* WINDOWSNT */

/* BEGIN: Windows Specific Includes */
#include <stdio.h>
#include <limits.h>
#include <mbstring.h>
#include <windows.h>
#include <mmsystem.h>

#include "coding.h"
#include "w32common.h"
#include "w32.h"
/* END: Windows Specific Includes */

#endif /* WINDOWSNT */

/* BEGIN: Common Definitions */

/* Indices of attributes in a sound attributes vector.  */

enum sound_attr
{
  SOUND_FILE,
  SOUND_DATA,
  SOUND_DEVICE,
  SOUND_VOLUME,
  SOUND_ATTR_SENTINEL
};

/* END: Common Definitions */

/* BEGIN: Non Windows Definitions */
#ifndef WINDOWSNT

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
  /* If a string, the name of the device; otherwise use a default.  */
  Lisp_Object file;

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
  void (* open) (struct sound_device *sd);

  /* Close device SD.  */
  void (* close) (struct sound_device *sd);

  /* Configure SD according to device-dependent parameters.  */
  void (* configure) (struct sound_device *device);

  /* Choose a device-dependent format for outputting sound S.  */
  void (* choose_format) (struct sound_device *sd,
                          struct sound *s);

  /* Return a preferred data size in bytes to be sent to write (below)
     each time.  2048 is used if this is NULL.  */
  ptrdiff_t (* period_size) (struct sound_device *sd);

  /* Write NYBTES bytes from BUFFER to device SD.  */
  void (* write) (struct sound_device *sd, const char *buffer,
                  ptrdiff_t nbytes);

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

  /* Number of bytes read from sound file.  This is always <=
     MAX_SOUND_HEADER_BYTES.  */
  int header_size;

  /* Sound data, if a string.  */
  Lisp_Object data;

  /* Play sound file S on device SD.  */
  void (* play) (struct sound *s, struct sound_device *sd);
};

/* These are set during `play-sound-internal' so that sound_cleanup has
   access to them.  */

static struct sound_device *current_sound_device;
static struct sound *current_sound;

/* Function prototypes.  */

static void vox_write (struct sound_device *, const char *, ptrdiff_t);
static bool wav_init (struct sound *);
static void wav_play (struct sound *, struct sound_device *);
static bool au_init (struct sound *);
static void au_play (struct sound *, struct sound_device *);

/* END: Non Windows Definitions */
#else /* WINDOWSNT */

/* BEGIN: Windows Specific Definitions */
static int do_play_sound (const char *, unsigned long);
/*
  END: Windows Specific Definitions */
#endif /* WINDOWSNT */


/***********************************************************************
			       General
 ***********************************************************************/

/* BEGIN: Common functions */

#ifndef WINDOWSNT
/* Like perror, but signals an error.  */

static _Noreturn void
sound_perror (const char *msg)
{
  int saved_errno = errno;

  turn_on_atimers (1);
#ifdef USABLE_SIGIO
  {
    sigset_t unblocked;
    sigemptyset (&unblocked);
    sigaddset (&unblocked, SIGIO);
    pthread_sigmask (SIG_UNBLOCK, &unblocked, 0);
  }
#endif
  if (saved_errno != 0)
    error ("%s: %s", msg, emacs_strerror (saved_errno));
  else
    error ("%s", msg);
}

/* Display a warning message.  */

static void
sound_warning (const char *msg)
{
  message1 (msg);
}
#endif	/* !WINDOWSNT */


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

static bool
parse_sound (Lisp_Object sound, Lisp_Object *attrs)
{
  /* SOUND must be a list starting with the symbol `sound'.  */
  if (!CONSP (sound) || !EQ (XCAR (sound), Qsound))
    return 0;

  sound = XCDR (sound);
  attrs[SOUND_FILE] = Fplist_get (sound, QCfile);
  attrs[SOUND_DATA] = Fplist_get (sound, QCdata);
  attrs[SOUND_DEVICE] = Fplist_get (sound, QCdevice);
  attrs[SOUND_VOLUME] = Fplist_get (sound, QCvolume);

#ifndef WINDOWSNT
  /* File name or data must be specified.  */
  if (!STRINGP (attrs[SOUND_FILE])
      && !STRINGP (attrs[SOUND_DATA]))
    return 0;
#else /* WINDOWSNT */
  /*
    Data is not supported in Windows.  Therefore a
    File name MUST be supplied.
  */
  if (!STRINGP (attrs[SOUND_FILE]))
    {
      return 0;
    }
#endif /* WINDOWSNT */

  /* Volume must be in the range 0..100 or unspecified.  */
  if (!NILP (attrs[SOUND_VOLUME]))
    {
      if (INTEGERP (attrs[SOUND_VOLUME]))
	{
	  EMACS_INT volume = XINT (attrs[SOUND_VOLUME]);
	  if (! (0 <= volume && volume <= 100))
	    return 0;
	}
      else if (FLOATP (attrs[SOUND_VOLUME]))
	{
	  double volume = XFLOAT_DATA (attrs[SOUND_VOLUME]);
	  if (! (0 <= volume && volume <= 1))
	    return 0;
	}
      else
	return 0;
    }

#ifndef WINDOWSNT
  /* Device must be a string or unspecified.  */
  if (!NILP (attrs[SOUND_DEVICE])
      && !STRINGP (attrs[SOUND_DEVICE]))
    return 0;
#endif  /* WINDOWSNT */
  /*
    Since device is ignored in Windows, it does not matter
    what it is.
   */
  return 1;
}

/* END: Common functions */

/* BEGIN: Non Windows functions */
#ifndef WINDOWSNT

/* Return S's value as a string if S is a string, otherwise DEFAULT_VALUE.  */

static char const *
string_default (Lisp_Object s, char const *default_value)
{
  return STRINGP (s) ? SSDATA (s) : default_value;
}


/* Find out the type of the sound file whose file descriptor is FD.
   S is the sound file structure to fill in.  */

static void
find_sound_type (struct sound *s)
{
  if (!wav_init (s) && !au_init (s))
    error ("Unknown sound format");
}


/* Function installed by play-sound-internal with record_unwind_protect_void.  */

static void
sound_cleanup (void)
{
  if (current_sound_device->close)
    current_sound_device->close (current_sound_device);
  if (current_sound->fd > 0)
    emacs_close (current_sound->fd);
  xfree (current_sound_device);
  xfree (current_sound);
}

/***********************************************************************
			Byte-order Conversion
 ***********************************************************************/

/* Convert 32-bit value VALUE which is in little-endian byte-order
   to host byte-order.  */

static u_int32_t
le2hl (u_int32_t value)
{
#ifdef WORDS_BIGENDIAN
  value = bswap_32 (value);
#endif
  return value;
}


/* Convert 16-bit value VALUE which is in little-endian byte-order
   to host byte-order.  */

static u_int16_t
le2hs (u_int16_t value)
{
#ifdef WORDS_BIGENDIAN
  value = bswap_16 (value);
#endif
  return value;
}


/* Convert 32-bit value VALUE which is in big-endian byte-order
   to host byte-order.  */

static u_int32_t
be2hl (u_int32_t value)
{
#ifndef WORDS_BIGENDIAN
  value = bswap_32 (value);
#endif
  return value;
}

/***********************************************************************
			  RIFF-WAVE (*.wav)
 ***********************************************************************/

/* Try to initialize sound file S from S->header.  S->header
   contains the first MAX_SOUND_HEADER_BYTES number of bytes from the
   sound file.  If the file is a WAV-format file, set up interface
   functions in S and convert header fields to host byte-order.
   Value is true if the file is a WAV file.  */

static bool
wav_init (struct sound *s)
{
  struct wav_header *header = (struct wav_header *) s->header;

  if (s->header_size < sizeof *header
      || memcmp (s->header, "RIFF", 4) != 0)
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
wav_play (struct sound *s, struct sound_device *sd)
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
    sd->write (sd, SSDATA (s->data) + sizeof *header,
	       SBYTES (s->data) - sizeof *header);
  else
    {
      ptrdiff_t nbytes = 0;
      ptrdiff_t blksize = sd->period_size ? sd->period_size (sd) : 2048;
      ptrdiff_t data_left = header->data_length;
      USE_SAFE_ALLOCA;
      char *buffer = SAFE_ALLOCA (blksize);
      lseek (s->fd, sizeof *header, SEEK_SET);
      while (data_left > 0
             && (nbytes = emacs_read (s->fd, buffer, blksize)) > 0)
        {
          /* Don't play possible garbage at the end of file */
          if (data_left < nbytes) nbytes = data_left;
          data_left -= nbytes;
          sd->write (sd, buffer, nbytes);
        }

      if (nbytes < 0)
	sound_perror ("Error reading sound file");
      SAFE_FREE ();
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
  AU_COMPRESSED = 23,
  AU_ENCODING_ALAW_8 = 27
};


/* Try to initialize sound file S from S->header.  S->header
   contains the first MAX_SOUND_HEADER_BYTES number of bytes from the
   sound file.  If the file is a AU-format file, set up interface
   functions in S and convert header fields to host byte-order.
   Value is true if the file is an AU file.  */

static bool
au_init (struct sound *s)
{
  struct au_header *header = (struct au_header *) s->header;

  if (s->header_size < sizeof *header
      || memcmp (s->header, ".snd", 4) != 0)
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
au_play (struct sound *s, struct sound_device *sd)
{
  struct au_header *header = (struct au_header *) s->header;

  sd->sample_size = 0;
  sd->sample_rate = header->sample_rate;
  sd->bps = 0;
  sd->channels = header->channels;
  sd->choose_format (sd, s);
  sd->configure (sd);

  if (STRINGP (s->data))
    sd->write (sd, SSDATA (s->data) + header->data_offset,
	       SBYTES (s->data) - header->data_offset);
  else
    {
      ptrdiff_t blksize = sd->period_size ? sd->period_size (sd) : 2048;
      ptrdiff_t nbytes;

      /* Seek */
      lseek (s->fd, header->data_offset, SEEK_SET);

      /* Copy sound data to the device.  */
      USE_SAFE_ALLOCA;
      char *buffer = SAFE_ALLOCA (blksize);
      while ((nbytes = emacs_read (s->fd, buffer, blksize)) > 0)
	sd->write (sd, buffer, nbytes);

      if (nbytes < 0)
	sound_perror ("Error reading sound file");
      SAFE_FREE ();
    }
}


/***********************************************************************
		       Voxware Driver Interface
 ***********************************************************************/

/* This driver is available on GNU/Linux, and the free BSDs.  FreeBSD
   has a compatible own driver aka Luigi's driver.  */


/* Open device SD.  If SD->file is a string, open that device,
   otherwise use a default device name.  */

static void
vox_open (struct sound_device *sd)
{
  /* Open the sound device (eg /dev/dsp).  */
  char const *file = string_default (sd->file, DEFAULT_SOUND_DEVICE);
  sd->fd = emacs_open (file, O_WRONLY, 0);
  if (sd->fd < 0)
    sound_perror (file);
}


/* Configure device SD from parameters in it.  */

static void
vox_configure (struct sound_device *sd)
{
  int val;
#ifdef USABLE_SIGIO
  sigset_t oldset, blocked;
#endif

  eassert (sd->fd >= 0);

  /* On GNU/Linux, it seems that the device driver doesn't like to be
     interrupted by a signal.  Block the ones we know to cause
     troubles.  */
  turn_on_atimers (0);
#ifdef USABLE_SIGIO
  sigemptyset (&blocked);
  sigaddset (&blocked, SIGIO);
  pthread_sigmask (SIG_BLOCK, &blocked, &oldset);
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
#ifdef USABLE_SIGIO
  pthread_sigmask (SIG_SETMASK, &oldset, 0);
#endif
}


/* Close device SD if it is open.  */

static void
vox_close (struct sound_device *sd)
{
  if (sd->fd >= 0)
    {
      /* On GNU/Linux, it seems that the device driver doesn't like to
	 be interrupted by a signal.  Block the ones we know to cause
	 troubles.  */
#ifdef USABLE_SIGIO
      sigset_t blocked, oldset;
      sigemptyset (&blocked);
      sigaddset (&blocked, SIGIO);
      pthread_sigmask (SIG_BLOCK, &blocked, &oldset);
#endif
      turn_on_atimers (0);

      /* Flush sound data, and reset the device.  */
      ioctl (sd->fd, SNDCTL_DSP_SYNC, NULL);

      turn_on_atimers (1);
#ifdef USABLE_SIGIO
      pthread_sigmask (SIG_SETMASK, &oldset, 0);
#endif

      /* Close the device.  */
      emacs_close (sd->fd);
      sd->fd = -1;
    }
}


/* Choose device-dependent format for device SD from sound file S.  */

static void
vox_choose_format (struct sound_device *sd, struct sound *s)
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
    emacs_abort ();
}


/* Initialize device SD.  Set up the interface functions in the device
   structure.  */

static bool
vox_init (struct sound_device *sd)
{
  /* Open the sound device (eg /dev/dsp).  */
  char const *file = string_default (sd->file, DEFAULT_SOUND_DEVICE);
  int fd = emacs_open (file, O_WRONLY, 0);
  if (fd >= 0)
    emacs_close (fd);
  else
    return 0;

  sd->fd = -1;
  sd->open = vox_open;
  sd->close = vox_close;
  sd->configure = vox_configure;
  sd->choose_format = vox_choose_format;
  sd->write = vox_write;
  sd->period_size = NULL;

  return 1;
}

/* Write NBYTES bytes from BUFFER to device SD.  */

static void
vox_write (struct sound_device *sd, const char *buffer, ptrdiff_t nbytes)
{
  if (emacs_write_sig (sd->fd, buffer, nbytes) != nbytes)
    sound_perror ("Error writing to sound device");
}

#ifdef HAVE_ALSA
/***********************************************************************
		       ALSA Driver Interface
 ***********************************************************************/

/* This driver is available on GNU/Linux. */

#ifndef DEFAULT_ALSA_SOUND_DEVICE
#define DEFAULT_ALSA_SOUND_DEVICE "default"
#endif

static _Noreturn void
alsa_sound_perror (const char *msg, int err)
{
  error ("%s: %s", msg, snd_strerror (err));
}

struct alsa_params
{
  snd_pcm_t *handle;
  snd_pcm_hw_params_t *hwparams;
  snd_pcm_sw_params_t *swparams;
  snd_pcm_uframes_t period_size;
};

/* Open device SD.  If SD->file is a string, open that device,
   otherwise use a default device name.  */

static void
alsa_open (struct sound_device *sd)
{
  /* Open the sound device.  Default is "default".  */
  struct alsa_params *p = xmalloc (sizeof *p);
  char const *file = string_default (sd->file, DEFAULT_ALSA_SOUND_DEVICE);
  int err;

  p->handle = NULL;
  p->hwparams = NULL;
  p->swparams = NULL;

  sd->fd = -1;
  sd->data = p;


  err = snd_pcm_open (&p->handle, file, SND_PCM_STREAM_PLAYBACK, 0);
  if (err < 0)
    alsa_sound_perror (file, err);
}

static ptrdiff_t
alsa_period_size (struct sound_device *sd)
{
  struct alsa_params *p = (struct alsa_params *) sd->data;
  int fact = snd_pcm_format_size (sd->format, 1) * sd->channels;
  return p->period_size * (fact > 0 ? fact : 1);
}

static void
alsa_configure (struct sound_device *sd)
{
  int val, err, dir;
  unsigned uval;
  struct alsa_params *p = (struct alsa_params *) sd->data;
  snd_pcm_uframes_t buffer_size;

  eassert (p->handle != 0);

  err = snd_pcm_hw_params_malloc (&p->hwparams);
  if (err < 0)
    alsa_sound_perror ("Could not allocate hardware parameter structure", err);

  err = snd_pcm_sw_params_malloc (&p->swparams);
  if (err < 0)
    alsa_sound_perror ("Could not allocate software parameter structure", err);

  err = snd_pcm_hw_params_any (p->handle, p->hwparams);
  if (err < 0)
    alsa_sound_perror ("Could not initialize hardware parameter structure", err);

  err = snd_pcm_hw_params_set_access (p->handle, p->hwparams,
                                      SND_PCM_ACCESS_RW_INTERLEAVED);
  if (err < 0)
    alsa_sound_perror ("Could not set access type", err);

  val = sd->format;
  err = snd_pcm_hw_params_set_format (p->handle, p->hwparams, val);
  if (err < 0)
    alsa_sound_perror ("Could not set sound format", err);

  uval = sd->sample_rate;
  err = snd_pcm_hw_params_set_rate_near (p->handle, p->hwparams, &uval, 0);
  if (err < 0)
    alsa_sound_perror ("Could not set sample rate", err);

  val = sd->channels;
  err = snd_pcm_hw_params_set_channels (p->handle, p->hwparams, val);
  if (err < 0)
    alsa_sound_perror ("Could not set channel count", err);

  err = snd_pcm_hw_params (p->handle, p->hwparams);
  if (err < 0)
    alsa_sound_perror ("Could not set parameters", err);


  err = snd_pcm_hw_params_get_period_size (p->hwparams, &p->period_size, &dir);
  if (err < 0)
    alsa_sound_perror ("Unable to get period size for playback", err);

  err = snd_pcm_hw_params_get_buffer_size (p->hwparams, &buffer_size);
  if (err < 0)
    alsa_sound_perror ("Unable to get buffer size for playback", err);

  err = snd_pcm_sw_params_current (p->handle, p->swparams);
  if (err < 0)
    alsa_sound_perror ("Unable to determine current swparams for playback",
                       err);

  /* Start the transfer when the buffer is almost full */
  err = snd_pcm_sw_params_set_start_threshold (p->handle, p->swparams,
                                               (buffer_size / p->period_size)
                                               * p->period_size);
  if (err < 0)
    alsa_sound_perror ("Unable to set start threshold mode for playback", err);

  /* Allow the transfer when at least period_size samples can be processed */
  err = snd_pcm_sw_params_set_avail_min (p->handle, p->swparams, p->period_size);
  if (err < 0)
    alsa_sound_perror ("Unable to set avail min for playback", err);

  err = snd_pcm_sw_params (p->handle, p->swparams);
  if (err < 0)
    alsa_sound_perror ("Unable to set sw params for playback\n", err);

  snd_pcm_hw_params_free (p->hwparams);
  p->hwparams = NULL;
  snd_pcm_sw_params_free (p->swparams);
  p->swparams = NULL;

  err = snd_pcm_prepare (p->handle);
  if (err < 0)
    alsa_sound_perror ("Could not prepare audio interface for use", err);

  if (sd->volume > 0)
    {
      int chn;
      snd_mixer_t *handle;
      snd_mixer_elem_t *e;
      if (snd_mixer_open (&handle, 0) >= 0)
        {
	  char const *file = string_default (sd->file,
					     DEFAULT_ALSA_SOUND_DEVICE);
          if (snd_mixer_attach (handle, file) >= 0
              && snd_mixer_load (handle) >= 0
              && snd_mixer_selem_register (handle, NULL, NULL) >= 0)
            for (e = snd_mixer_first_elem (handle);
                 e;
                 e = snd_mixer_elem_next (e))
              {
                if (snd_mixer_selem_has_playback_volume (e))
                  {
                    long pmin, pmax, vol;
                    snd_mixer_selem_get_playback_volume_range (e, &pmin, &pmax);
                    vol = pmin + (sd->volume * (pmax - pmin)) / 100;

                    for (chn = 0; chn <= SND_MIXER_SCHN_LAST; chn++)
                      snd_mixer_selem_set_playback_volume (e, chn, vol);
                  }
              }
          snd_mixer_close (handle);
        }
    }
}


/* Close device SD if it is open.  */

static void
alsa_close (struct sound_device *sd)
{
  struct alsa_params *p = (struct alsa_params *) sd->data;
  if (p)
    {
      if (p->hwparams)
        snd_pcm_hw_params_free (p->hwparams);
      if (p->swparams)
        snd_pcm_sw_params_free (p->swparams);
      if (p->handle)
        {
          snd_pcm_drain (p->handle);
          snd_pcm_close (p->handle);
        }
      xfree (p);
    }
}

/* Choose device-dependent format for device SD from sound file S.  */

static void
alsa_choose_format (struct sound_device *sd, struct sound *s)
{
  if (s->type == RIFF)
    {
      struct wav_header *h = (struct wav_header *) s->header;
      if (h->precision == 8)
	sd->format = SND_PCM_FORMAT_U8;
      else if (h->precision == 16)
          sd->format = SND_PCM_FORMAT_S16_LE;
      else
	error ("Unsupported WAV file format");
    }
  else if (s->type == SUN_AUDIO)
    {
      struct au_header *header = (struct au_header *) s->header;
      switch (header->encoding)
	{
	case AU_ENCODING_ULAW_8:
	  sd->format = SND_PCM_FORMAT_MU_LAW;
          break;
	case AU_ENCODING_ALAW_8:
	  sd->format = SND_PCM_FORMAT_A_LAW;
          break;
	case AU_ENCODING_IEEE32:
          sd->format = SND_PCM_FORMAT_FLOAT_BE;
          break;
	case AU_ENCODING_IEEE64:
	  sd->format = SND_PCM_FORMAT_FLOAT64_BE;
	  break;
	case AU_ENCODING_8:
	  sd->format = SND_PCM_FORMAT_S8;
	  break;
	case AU_ENCODING_16:
	  sd->format = SND_PCM_FORMAT_S16_BE;
	  break;
	case AU_ENCODING_24:
	  sd->format = SND_PCM_FORMAT_S24_BE;
	  break;
	case AU_ENCODING_32:
	  sd->format = SND_PCM_FORMAT_S32_BE;
	  break;

	default:
	  error ("Unsupported AU file format");
	}
    }
  else
    emacs_abort ();
}


/* Write NBYTES bytes from BUFFER to device SD.  */

static void
alsa_write (struct sound_device *sd, const char *buffer, ptrdiff_t nbytes)
{
  struct alsa_params *p = (struct alsa_params *) sd->data;

  /* The the third parameter to snd_pcm_writei is frames, not bytes. */
  int fact = snd_pcm_format_size (sd->format, 1) * sd->channels;
  ptrdiff_t nwritten = 0;
  int err;

  while (nwritten < nbytes)
    {
      snd_pcm_uframes_t frames = (nbytes - nwritten)/fact;
      if (frames == 0) break;

      err = snd_pcm_writei (p->handle, buffer + nwritten, frames);
      if (err < 0)
        {
          if (err == -EPIPE)
            {	/* under-run */
              err = snd_pcm_prepare (p->handle);
              if (err < 0)
                alsa_sound_perror ("Can't recover from underrun, prepare failed",
                                   err);
            }
          else if (err == -ESTRPIPE)
            {
              while ((err = snd_pcm_resume (p->handle)) == -EAGAIN)
                sleep (1);	/* wait until the suspend flag is released */
              if (err < 0)
                {
                  err = snd_pcm_prepare (p->handle);
                  if (err < 0)
                    alsa_sound_perror ("Can't recover from suspend, "
                                       "prepare failed",
                                       err);
                }
            }
          else
            alsa_sound_perror ("Error writing to sound device", err);

        }
      else
        nwritten += err * fact;
    }
}

static void
snd_error_quiet (const char *file, int line, const char *function, int err,
		 const char *fmt)
{
}

/* Initialize device SD.  Set up the interface functions in the device
   structure.  */

static bool
alsa_init (struct sound_device *sd)
{
  /* Open the sound device.  Default is "default".  */
  char const *file = string_default (sd->file, DEFAULT_ALSA_SOUND_DEVICE);
  snd_pcm_t *handle;
  int err;

  snd_lib_error_set_handler ((snd_lib_error_handler_t) snd_error_quiet);
  err = snd_pcm_open (&handle, file, SND_PCM_STREAM_PLAYBACK, 0);
  snd_lib_error_set_handler (NULL);
  if (err < 0)
      return 0;
  snd_pcm_close (handle);

  sd->fd = -1;
  sd->open = alsa_open;
  sd->close = alsa_close;
  sd->configure = alsa_configure;
  sd->choose_format = alsa_choose_format;
  sd->write = alsa_write;
  sd->period_size = alsa_period_size;

  return 1;
}

#endif /* HAVE_ALSA */


/* END: Non Windows functions */
#else /* WINDOWSNT */

/* BEGIN: Windows specific functions */

#define SOUND_WARNING(func, error, text)		\
  do {							\
    char buf[1024];					\
    char err_string[MAXERRORLENGTH];			\
    func (error, err_string, sizeof (err_string));	\
    _snprintf (buf, sizeof (buf), "%s\nMCI Error: %s",	\
	       text, err_string);			\
    message_with_string ("%s", build_string (buf), 1);	\
  } while (0)

static int
do_play_sound (const char *psz_file, unsigned long ui_volume)
{
  int i_result = 0;
  MCIERROR mci_error = 0;
  char sz_cmd_buf_a[520];
  char sz_ret_buf_a[520];
  MMRESULT mm_result = MMSYSERR_NOERROR;
  unsigned long ui_volume_org = 0;
  BOOL b_reset_volume = FALSE;
  char warn_text[560];

  /* Since UNICOWS.DLL includes only a stub for mciSendStringW, we
     need to encode the file in the ANSI codepage on Windows 9X even
     if w32_unicode_filenames is non-zero.  */
  if (w32_major_version <= 4 || !w32_unicode_filenames)
    {
      char fname_a[MAX_PATH], shortname[MAX_PATH], *fname_to_use;

      filename_to_ansi (psz_file, fname_a);
      fname_to_use = fname_a;
      /* If the file name is not encodable in ANSI, try its short 8+3
	 alias.  This will only work if w32_unicode_filenames is
	 non-zero.  */
      if (_mbspbrk ((const unsigned char *)fname_a,
		    (const unsigned char *)"?"))
	{
	  if (w32_get_short_filename (psz_file, shortname, MAX_PATH))
	    fname_to_use = shortname;
	  else
	    mci_error = MCIERR_FILE_NOT_FOUND;
	}

      if (!mci_error)
	{
	  memset (sz_cmd_buf_a, 0, sizeof (sz_cmd_buf_a));
	  memset (sz_ret_buf_a, 0, sizeof (sz_ret_buf_a));
	  sprintf (sz_cmd_buf_a,
		   "open \"%s\" alias GNUEmacs_PlaySound_Device wait",
		   fname_to_use);
	  mci_error = mciSendStringA (sz_cmd_buf_a,
				      sz_ret_buf_a, sizeof (sz_ret_buf_a), NULL);
	}
    }
  else
    {
      wchar_t sz_cmd_buf_w[520];
      wchar_t sz_ret_buf_w[520];
      wchar_t fname_w[MAX_PATH];

      filename_to_utf16 (psz_file, fname_w);
      memset (sz_cmd_buf_w, 0, sizeof (sz_cmd_buf_w));
      memset (sz_ret_buf_w, 0, sizeof (sz_ret_buf_w));
      /* _swprintf is not available on Windows 9X, so we construct the
	 UTF-16 command string by hand.  */
      wcscpy (sz_cmd_buf_w, L"open \"");
      wcscat (sz_cmd_buf_w, fname_w);
      wcscat (sz_cmd_buf_w, L"\" alias GNUEmacs_PlaySound_Device wait");
      mci_error = mciSendStringW (sz_cmd_buf_w,
				  sz_ret_buf_w, ARRAYELTS (sz_ret_buf_w) , NULL);
    }
  if (mci_error != 0)
    {
      strcpy (warn_text,
	      "mciSendString: 'open' command failed to open sound file ");
      strcat (warn_text, psz_file);
      SOUND_WARNING (mciGetErrorString, mci_error, warn_text);
      i_result = (int) mci_error;
      return i_result;
    }
  if ((ui_volume > 0) && (ui_volume != UINT_MAX))
    {
      mm_result = waveOutGetVolume ((HWAVEOUT) WAVE_MAPPER, &ui_volume_org);
      if (mm_result == MMSYSERR_NOERROR)
        {
          b_reset_volume = TRUE;
          mm_result = waveOutSetVolume ((HWAVEOUT) WAVE_MAPPER, ui_volume);
          if (mm_result != MMSYSERR_NOERROR)
            {
	      SOUND_WARNING (waveOutGetErrorText, mm_result,
			     "waveOutSetVolume: failed to set the volume level"
			     " of the WAVE_MAPPER device.\n"
			     "As a result, the user selected volume level will"
			     " not be used.");
            }
        }
      else
        {
          SOUND_WARNING (waveOutGetErrorText, mm_result,
			 "waveOutGetVolume: failed to obtain the original"
                         " volume level of the WAVE_MAPPER device.\n"
                         "As a result, the user selected volume level will"
                         " not be used.");
        }
    }
  memset (sz_cmd_buf_a, 0, sizeof (sz_cmd_buf_a));
  memset (sz_ret_buf_a, 0, sizeof (sz_ret_buf_a));
  strcpy (sz_cmd_buf_a, "play GNUEmacs_PlaySound_Device wait");
  mci_error = mciSendStringA (sz_cmd_buf_a, sz_ret_buf_a, sizeof (sz_ret_buf_a),
			      NULL);
  if (mci_error != 0)
    {
      strcpy (warn_text,
	      "mciSendString: 'play' command failed to play sound file ");
      strcat (warn_text, psz_file);
      SOUND_WARNING (mciGetErrorString, mci_error, warn_text);
      i_result = (int) mci_error;
    }
  memset (sz_cmd_buf_a, 0, sizeof (sz_cmd_buf_a));
  memset (sz_ret_buf_a, 0, sizeof (sz_ret_buf_a));
  strcpy (sz_cmd_buf_a, "close GNUEmacs_PlaySound_Device wait");
  mci_error = mciSendStringA (sz_cmd_buf_a, sz_ret_buf_a, sizeof (sz_ret_buf_a),
			      NULL);
  if (b_reset_volume == TRUE)
    {
      mm_result = waveOutSetVolume ((HWAVEOUT) WAVE_MAPPER, ui_volume_org);
      if (mm_result != MMSYSERR_NOERROR)
        {
          SOUND_WARNING (waveOutGetErrorText, mm_result,
			 "waveOutSetVolume: failed to reset the original"
                         " volume level of the WAVE_MAPPER device.");
        }
    }
  return i_result;
}

/* END: Windows specific functions */

#endif /* WINDOWSNT */

DEFUN ("play-sound-internal", Fplay_sound_internal, Splay_sound_internal, 1, 1, 0,
       doc: /* Play sound SOUND.

Internal use only, use `play-sound' instead.  */)
  (Lisp_Object sound)
{
  Lisp_Object attrs[SOUND_ATTR_SENTINEL];
  ptrdiff_t count = SPECPDL_INDEX ();

#ifdef WINDOWSNT
  unsigned long ui_volume_tmp = UINT_MAX;
  unsigned long ui_volume = UINT_MAX;
#endif /* WINDOWSNT */

  /* Parse the sound specification.  Give up if it is invalid.  */
  if (!parse_sound (sound, attrs))
    error ("Invalid sound specification");

  Lisp_Object file = Qnil;

#ifndef WINDOWSNT
  current_sound_device = xzalloc (sizeof *current_sound_device);
  current_sound = xzalloc (sizeof *current_sound);
  record_unwind_protect_void (sound_cleanup);
  char headerbuf[MAX_SOUND_HEADER_BYTES];
  current_sound->header = headerbuf;

  if (STRINGP (attrs[SOUND_FILE]))
    {
      /* Open the sound file.  */
      current_sound->fd = openp (list1 (Vdata_directory),
				 attrs[SOUND_FILE], Qnil, &file, Qnil, false);
      if (current_sound->fd < 0)
	sound_perror ("Could not open sound file");

      /* Read the first bytes from the file.  */
      current_sound->header_size
	= emacs_read (current_sound->fd, current_sound->header,
		      MAX_SOUND_HEADER_BYTES);
      if (current_sound->header_size < 0)
	sound_perror ("Invalid sound file header");
    }
  else
    {
      current_sound->data = attrs[SOUND_DATA];
      current_sound->header_size = min (MAX_SOUND_HEADER_BYTES, SBYTES (current_sound->data));
      memcpy (current_sound->header, SDATA (current_sound->data),
	      current_sound->header_size);
    }

  /* Find out the type of sound.  Give up if we can't tell.  */
  find_sound_type (current_sound);

  /* Set up a device.  */
  current_sound_device->file = attrs[SOUND_DEVICE];

  if (INTEGERP (attrs[SOUND_VOLUME]))
    current_sound_device->volume = XFASTINT (attrs[SOUND_VOLUME]);
  else if (FLOATP (attrs[SOUND_VOLUME]))
    current_sound_device->volume = XFLOAT_DATA (attrs[SOUND_VOLUME]) * 100;

  CALLN (Frun_hook_with_args, Qplay_sound_functions, sound);

#ifdef HAVE_ALSA
  if (!alsa_init (current_sound_device))
#endif
    if (!vox_init (current_sound_device))
      error ("No usable sound device driver found");

  /* Open the device.  */
  current_sound_device->open (current_sound_device);

  /* Play the sound.  */
  current_sound->play (current_sound, current_sound_device);

#else /* WINDOWSNT */

  file = Fexpand_file_name (attrs[SOUND_FILE], Vdata_directory);
  file = ENCODE_FILE (file);
  if (INTEGERP (attrs[SOUND_VOLUME]))
    {
      ui_volume_tmp = XFASTINT (attrs[SOUND_VOLUME]);
    }
  else if (FLOATP (attrs[SOUND_VOLUME]))
    {
      ui_volume_tmp = XFLOAT_DATA (attrs[SOUND_VOLUME]) * 100;
    }

  CALLN (Frun_hook_with_args, Qplay_sound_functions, sound);

  /*
    Based on some experiments I have conducted, a value of 100 or less
    for the sound volume is much too low.  You cannot even hear it.
    A value of UINT_MAX indicates that you wish for the sound to played
    at the maximum possible volume.  A value of UINT_MAX/2 plays the
    sound at 50% maximum volume.  Therefore the value passed to do_play_sound
    (and thus to waveOutSetVolume) must be some fraction of UINT_MAX.
    The following code adjusts the user specified volume level appropriately.
  */
  if ((ui_volume_tmp > 0) && (ui_volume_tmp <= 100))
    {
      ui_volume = ui_volume_tmp * (UINT_MAX / 100);
    }
  (void)do_play_sound (SSDATA (file), ui_volume);

#endif /* WINDOWSNT */

  return unbind_to (count, Qnil);
}

/***********************************************************************
			    Initialization
 ***********************************************************************/

void
syms_of_sound (void)
{
  DEFSYM (QCdevice, ":device");
  DEFSYM (QCvolume, ":volume");
  DEFSYM (Qsound, "sound");
  DEFSYM (Qplay_sound_functions, "play-sound-functions");

  defsubr (&Splay_sound_internal);
}

#endif /* HAVE_SOUND */
