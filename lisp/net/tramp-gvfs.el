;;; tramp-gvfs.el --- Tramp access functions for GVFS daemon  -*- lexical-binding:t -*-

;; Copyright (C) 2009-2025 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes
;; Package: tramp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Access functions for the GVFS daemon from Tramp.  Tested with GVFS
;; 1.0 (Ubuntu 8.10, Gnome 2.24).  It has been reported also to run
;; with GVFS 0.2.5 (Ubuntu 8.04, Gnome 2.22), but there is an
;; incompatibility with the mount_info structure, which has been
;; worked around.

;; It has also been tested with GVFS 1.6 (Ubuntu 10.04, Gnome 2.30),
;; where the default_location has been added to mount_info (see
;; <https://bugzilla.gnome.org/show_bug.cgi?id=561998>.

;; With GVFS 1.14 (Ubuntu 12.10, Gnome 3.6) the interfaces have been
;; changed, again.  So we must introspect the D-Bus interfaces.

;; All actions to mount a remote location, and to retrieve mount
;; information, are performed by D-Bus messages.  File operations
;; themselves are performed via the mounted filesystem in ~/.gvfs.
;; Consequently, GNU Emacs with enabled D-Bus bindings is a
;; precondition.

;; The GVFS D-Bus interface is said to be unstable.  There were even
;; no introspection data before GVFS 1.14.  The interface, as
;; discovered during development time, is given in respective
;; comments.

;; The user option `tramp-gvfs-methods' contains the list of supported
;; connection methods.  Per default, these are "afp", "dav", "davs",
;; "gdrive", "mtp", "nextcloud" and "sftp".

;; "gdrive" and "nextcloud" connection methods require a respective
;; account in GNOME Online Accounts, with enabled "Files" service.

;; The "mtp" connection method is responsible for media devices,
;; like cell phones, tablets, cameras etc.  The device must already be
;; connected via USB, before accessing it.

;; Other possible connection methods are "ftp", "http", "https" and
;; "smb".  When one of these methods is added to the list, the remote
;; access for that method is performed via GVFS instead of the native
;; Tramp implementation.  However, this is not recommended.  These
;; methods are listed here for the benefit of file archives, see
;; tramp-archive.el.

;; GVFS offers even more connection methods.  The complete list of
;; connection methods of the actual GVFS implementation can be
;; retrieved by:
;;
;; (message
;;  "%s"
;;  (mapcar
;;   #'car
;;   (dbus-call-method
;;    :session tramp-gvfs-service-daemon tramp-gvfs-path-mounttracker
;;    tramp-gvfs-interface-mounttracker "ListMountableInfo")))

;; See also /usr/share/gvfs/mounts

;; Note that all other connection methods are not tested, beside the
;; ones offered for customization in `tramp-gvfs-methods'.  If you
;; request an additional connection method to be supported, please
;; drop me a note.

;; For hostname completion, information is retrieved from the zeroconf
;; daemon (for the "afp", "dav", "davs", and "sftp" methods).  The
;; zeroconf daemon is pre-configured to discover services in the
;; "local" domain.  If another domain should be used for discovering
;; services, the user option `tramp-gvfs-zeroconf-domain' can be set
;; accordingly.

;; Restrictions:
;;
;; * Two shares of the same SMB server cannot be mounted in parallel.

;;; Code:

;; D-Bus support in the Emacs core can be disabled with configuration
;; option "--without-dbus".  Declare used subroutines and variables.
(declare-function dbus-get-unique-name "dbusbind.c")

(eval-when-compile (require 'cl-lib))
(require 'tramp)
(require 'dbus)
(require 'url-parse)
(require 'url-util)

;; Pacify byte-compiler.
(declare-function file-notify-callback "filenotify")
(declare-function zeroconf-init "zeroconf")
(declare-function zeroconf-list-service-types "zeroconf")
(declare-function zeroconf-list-services "zeroconf")
(declare-function zeroconf-service-host "zeroconf")
(declare-function zeroconf-service-port "zeroconf")
(declare-function zeroconf-service-txt "zeroconf")
(defvar tramp-gvfs-dbus-event-vector)

;; We don't call `dbus-ping', because this would load dbus.el.
(defconst tramp-gvfs-enabled
  (ignore-errors
    (and (featurep 'dbusbind)
	 (tramp-compat-funcall 'dbus-get-unique-name :session)
	 (or (tramp-process-running-p "gvfs-fuse-daemon")
	     (tramp-process-running-p "gvfsd-fuse"))))
  "Non-nil when GVFS is available.")

;;;###tramp-autoload
(defcustom tramp-gvfs-methods
  '("afp" "dav" "davs" "gdrive" "mtp" "nextcloud" "sftp")
  "List of methods for remote files, accessed with GVFS."
  :group 'tramp
  :version "28.1"
  :type '(repeat (choice (const "afp")
			 (const "dav")
			 (const "davs")
			 (const "ftp")
			 (const "gdrive")
			 (const "http")
			 (const "https")
			 (const "mtp")
			 (const "nextcloud")
			 (const "sftp")
			 (const "smb")))
  :link '(tramp-info-link :tag "Tramp manual" tramp-gvfs-methods))

;;;###tramp-autoload
(defconst tramp-goa-methods '("gdrive" "nextcloud")
  "List of methods which require registration at GNOME Online Accounts.")

;; Remove GNOME Online Accounts methods if not supported.
(unless (and tramp-gvfs-enabled
	     (member tramp-goa-service (dbus-list-known-names :session)))
  (dolist (method tramp-goa-methods)
    (setq tramp-gvfs-methods (delete method tramp-gvfs-methods))))

;;;###tramp-autoload
(defvar tramp-media-methods '("afc" "gphoto2" "mtp")
  "List of GVFS methods which are covered by the \"mtp\" method.
They are checked during start up via
`tramp-gvfs-interface-remotevolumemonitor'.")

(defsubst tramp-gvfs-service-volumemonitor (method)
  "Return the well known name of the volume monitor responsible for METHOD."
  (symbol-value
   (intern-soft (format "tramp-gvfs-service-%s-volumemonitor" method))))

;; Remove media methods if not supported.
(when tramp-gvfs-enabled
  (dolist (method tramp-media-methods)
    (unless (member (tramp-gvfs-service-volumemonitor method)
		    (dbus-list-known-names :session))
      (setq tramp-media-methods (delete method tramp-media-methods)))))

(defcustom tramp-gvfs-zeroconf-domain "local"
  "Zeroconf domain to be used for discovering services, like host names."
  :group 'tramp
  :version "23.2"
  :type 'string)

;; Add the methods to `tramp-methods', in order to allow minibuffer
;; completion.  Add defaults for `tramp-default-host-alist'.
;;;###tramp-autoload
(when (featurep 'dbusbind)
  (tramp--with-startup
   (dolist (method tramp-gvfs-methods)
     (unless (assoc method tramp-methods)
       (add-to-list 'tramp-methods `(,method)))
     (when (member method tramp-goa-methods)
       (add-to-list 'tramp-default-host-alist `(,method nil ""))))))

(defconst tramp-gvfs-path-tramp (concat dbus-path-emacs "/Tramp")
  "The preceding object path for own objects.")

(defconst tramp-gvfs-service-daemon "org.gtk.vfs.Daemon"
  "The well known name of the GVFS daemon.")

(defconst tramp-gvfs-path-mounttracker "/org/gtk/vfs/mounttracker"
  "The object path of the GVFS daemon.")

(defconst tramp-gvfs-interface-mounttracker "org.gtk.vfs.MountTracker"
  "The mount tracking interface in the GVFS daemon.")

;; Introspection data exist since GVFS 1.14.  If there are no such
;; data, we expect an earlier interface.
(defconst tramp-gvfs-methods-mounttracker
  (and tramp-gvfs-enabled
       (dbus-introspect-get-method-names
	:session tramp-gvfs-service-daemon tramp-gvfs-path-mounttracker
	tramp-gvfs-interface-mounttracker))
  "The list of supported methods of the mount tracking interface.")

(defconst tramp-gvfs-listmountableinfo
  (if (member "ListMountableInfo" tramp-gvfs-methods-mounttracker)
      "ListMountableInfo"
    "listMountableInfo")
  "The name of the \"listMountableInfo\" method.
It has been changed in GVFS 1.14.")

(defconst tramp-gvfs-listmounttypes
  (if (member "ListMountTypes" tramp-gvfs-methods-mounttracker)
      "ListMountTypes"
    "listMountTypes")
  "The name of the \"listMountTypes\" method.
It has been changed in GVFS 1.14.")

(defconst tramp-gvfs-mounttypes
  (and tramp-gvfs-enabled
       (dbus-call-method
	:session tramp-gvfs-service-daemon tramp-gvfs-path-mounttracker
	tramp-gvfs-interface-mounttracker tramp-gvfs-listmounttypes))
  "The list of supported mount types of the mount tracking interface.")

(defconst tramp-gvfs-listmounts
  (if (member "ListMounts" tramp-gvfs-methods-mounttracker)
      "ListMounts"
    "listMounts")
  "The name of the \"listMounts\" method.
It has been changed in GVFS 1.14.")

(defconst tramp-gvfs-mountlocation
  (if (member "MountLocation" tramp-gvfs-methods-mounttracker)
      "MountLocation"
    "mountLocation")
  "The name of the \"mountLocation\" method.
It has been changed in GVFS 1.14.")

(defconst tramp-gvfs-mountlocation-signature
  (and tramp-gvfs-enabled
       (dbus-introspect-get-signature
	:session tramp-gvfs-service-daemon tramp-gvfs-path-mounttracker
	tramp-gvfs-interface-mounttracker tramp-gvfs-mountlocation))
  "The D-Bus signature of the \"mountLocation\" method.
It has been changed in GVFS 1.14.")

;; <interface name='org.gtk.vfs.MountTracker'>
;;   <method name='listMountableInfo'>
;;     <arg name='mountables'  type='a(ssasib)'  direction='out'/>
;;   </method>
;;   <method name='listMountTypes'>
;;     <arg name='mount_types' type='as'         direction='out'/>
;;   </method>
;;   <method name='listMounts'>
;;     <arg name='mount_info_list'
;;          type='a{sosssssbay{aya{say}}ay}'
;;          direction='out'/>
;;   </method>
;;   <method name='mountLocation'>
;;     <arg name='mount_spec'  type='{aya{say}}' direction='in'/>
;;     <arg name='dbus_id'     type='s'          direction='in'/>
;;     <arg name='object_path' type='o'          direction='in'/>
;;   </method>
;;   <signal name='mounted'>
;;     <arg name='mount_info'
;;          type='{sosssssbay{aya{say}}ay}'/>
;;   </signal>
;;   <signal name='unmounted'>
;;     <arg name='mount_info'
;;          type='{sosssssbay{aya{say}}ay}'/>
;;   </signal>
;; </interface>
;;
;; STRUCT		mountable
;;   STRING		  type
;;   STRING		  scheme
;;   ARRAY STRING	  scheme_aliases
;;   INT32		  default_port
;;   BOOLEAN		  host_is_inet
;;
;; STRUCT		mount_info
;;   STRING		  dbus_id
;;   OBJECT_PATH	  object_path
;;   STRING		  display_name
;;   STRING		  stable_name
;;   STRING		  x_content_types	Since GVFS 1.0 only !!!
;;   STRING		  icon
;;   STRING		  preferred_filename_encoding
;;   BOOLEAN		  user_visible
;;   ARRAY BYTE		  fuse_mountpoint
;;   STRUCT		  mount_spec
;;     ARRAY BYTE	    mount_prefix
;;     ARRAY
;;       STRUCT		    mount_spec_item
;;         STRING	      key (type, user, domain, host, server,
;;                                 share, volume, port, ssl)
;;         ARRAY BYTE	      value
;;   ARRAY BYTE           default_location	Since GVFS 1.5 only !!!

(defconst tramp-gvfs-interface-mountoperation "org.gtk.vfs.MountOperation"
  "Used by the dbus-proxying implementation of GMountOperation.")

;; <interface name='org.gtk.vfs.MountOperation'>
;;   <method name='askPassword'>
;;     <arg name='message'        type='s' direction='in'/>
;;     <arg name='default_user'   type='s' direction='in'/>
;;     <arg name='default_domain' type='s' direction='in'/>
;;     <arg name='flags'          type='u' direction='in'/>
;;     <arg name='handled'        type='b' direction='out'/>
;;     <arg name='aborted'        type='b' direction='out'/>
;;     <arg name='password'       type='s' direction='out'/>
;;     <arg name='username'       type='s' direction='out'/>
;;     <arg name='domain'         type='s' direction='out'/>
;;     <arg name='anonymous'      type='b' direction='out'/>
;;     <arg name='password_save'  type='u' direction='out'/>
;;   </method>
;;   <method name='askQuestion'>
;;     <arg name='message' type='s'  direction='in'/>
;;     <arg name='choices' type='as' direction='in'/>
;;     <arg name='handled' type='b'  direction='out'/>
;;     <arg name='aborted' type='b'  direction='out'/>
;;     <arg name='choice'  type='u'  direction='out'/>
;;   </method>
;; </interface>

;; The following flags are used in "askPassword".  They are defined in
;; /usr/include/glib-2.0/gio/gioenums.h.

(defconst tramp-gvfs-password-need-password 1
  "Operation requires a password.")

(defconst tramp-gvfs-password-need-username 2
  "Operation requires a username.")

(defconst tramp-gvfs-password-need-domain 4
  "Operation requires a domain.")

(defconst tramp-gvfs-password-saving-supported 8
  "Operation supports saving settings.")

(defconst tramp-gvfs-password-anonymous-supported 16
  "Operation supports anonymous users.")

;; Since: 2.58
(defconst tramp-gvfs-password-tcrypt 32
  "Operation takes TCRYPT parameters.")

;; For the time being, we just need org.goa.Account and org.goa.Files
;; interfaces.  We document the other ones, just in case.

;;;###tramp-autoload
(defconst tramp-goa-service "org.gnome.OnlineAccounts"
  "The well known name of the GNOME Online Accounts service.")

(defconst tramp-goa-path "/org/gnome/OnlineAccounts"
  "The object path of the GNOME Online Accounts.")

(defconst tramp-goa-path-accounts (concat tramp-goa-path "/Accounts")
  "The object path of the GNOME Online Accounts accounts.")

(defconst tramp-goa-interface-documents "org.gnome.OnlineAccounts.Documents"
  "The documents interface of the GNOME Online Accounts.")

;; <interface name='org.gnome.OnlineAccounts.Documents'>
;; </interface>

(defconst tramp-goa-interface-printers "org.gnome.OnlineAccounts.Printers"
  "The printers interface of the GNOME Online Accounts.")

;; <interface name='org.gnome.OnlineAccounts.Printers'>
;; </interface>

(defconst tramp-goa-interface-files "org.gnome.OnlineAccounts.Files"
  "The files interface of the GNOME Online Accounts.")

;; <interface name='org.gnome.OnlineAccounts.Files'>
;;   <property type='b' name='AcceptSslErrors' access='read'/>
;;   <property type='s' name='Uri' access='read'/>
;; </interface>

(defconst tramp-goa-interface-contacts "org.gnome.OnlineAccounts.Contacts"
  "The contacts interface of the GNOME Online Accounts.")

;; <interface name='org.gnome.OnlineAccounts.Contacts'>
;;   <property type='b' name='AcceptSslErrors' access='read'/>
;;   <property type='s' name='Uri' access='read'/>
;; </interface>

(defconst tramp-goa-interface-calendar "org.gnome.OnlineAccounts.Calendar"
  "The calendar interface of the GNOME Online Accounts.")

;; <interface name='org.gnome.OnlineAccounts.Calendar'>
;;   <property type='b' name='AcceptSslErrors' access='read'/>
;;   <property type='s' name='Uri' access='read'/>
;; </interface>

(defconst tramp-goa-interface-oauth2based "org.gnome.OnlineAccounts.OAuth2Based"
  "The oauth2based interface of the GNOME Online Accounts.")

;; <interface name='org.gnome.OnlineAccounts.OAuth2Based'>
;;   <method name='GetAccessToken'>
;;     <arg type='s' name='access_token' direction='out'/>
;;     <arg type='i' name='expires_in' direction='out'/>
;;   </method>
;;   <property type='s' name='ClientId' access='read'/>
;;   <property type='s' name='ClientSecret' access='read'/>
;; </interface>

(defconst tramp-goa-interface-account "org.gnome.OnlineAccounts.Account"
  "The account interface of the GNOME Online Accounts.")

;; <interface name='org.gnome.OnlineAccounts.Account'>
;;   <method name='Remove'/>
;;   <method name='EnsureCredentials'>
;;     <arg type='i' name='expires_in' direction='out'/>
;;   </method>
;;   <property type='s' name='ProviderType' access='read'/>
;;   <property type='s' name='ProviderName' access='read'/>
;;   <property type='s' name='ProviderIcon' access='read'/>
;;   <property type='s' name='Id' access='read'/>
;;   <property type='b' name='IsLocked' access='read'/>
;;   <property type='b' name='IsTemporary' access='readwrite'/>
;;   <property type='b' name='AttentionNeeded' access='read'/>
;;   <property type='s' name='Identity' access='read'/>
;;   <property type='s' name='PresentationIdentity' access='read'/>
;;   <property type='b' name='MailDisabled' access='readwrite'/>
;;   <property type='b' name='CalendarDisabled' access='readwrite'/>
;;   <property type='b' name='ContactsDisabled' access='readwrite'/>
;;   <property type='b' name='ChatDisabled' access='readwrite'/>
;;   <property type='b' name='DocumentsDisabled' access='readwrite'/>
;;   <property type='b' name='MapsDisabled' access='readwrite'/>
;;   <property type='b' name='MusicDisabled' access='readwrite'/>
;;   <property type='b' name='PrintersDisabled' access='readwrite'/>
;;   <property type='b' name='PhotosDisabled' access='readwrite'/>
;;   <property type='b' name='FilesDisabled' access='readwrite'/>
;;   <property type='b' name='TicketingDisabled' access='readwrite'/>
;;   <property type='b' name='TodoDisabled' access='readwrite'/>
;;   <property type='b' name='ReadLaterDisabled' access='readwrite'/>
;; </interface>

(defconst tramp-goa-identity-regexp
  (rx
   bol (? (group (regexp tramp-user-regexp)))
   "@" (? (group (regexp tramp-host-regexp)))
   (? ":" (group (regexp tramp-port-regexp))))
  "Regexp matching GNOME Online Accounts \"PresentationIdentity\" property.")

(defconst tramp-goa-interface-mail "org.gnome.OnlineAccounts.Mail"
  "The mail interface of the GNOME Online Accounts.")

;; <interface name='org.gnome.OnlineAccounts.Mail'>
;;   <property type='s' name='EmailAddress' access='read'/>
;;   <property type='s' name='Name' access='read'/>
;;   <property type='b' name='ImapSupported' access='read'/>
;;   <property type='b' name='ImapAcceptSslErrors' access='read'/>
;;   <property type='s' name='ImapHost' access='read'/>
;;   <property type='b' name='ImapUseSsl' access='read'/>
;;   <property type='b' name='ImapUseTls' access='read'/>
;;   <property type='s' name='ImapUserName' access='read'/>
;;   <property type='b' name='SmtpSupported' access='read'/>
;;   <property type='b' name='SmtpAcceptSslErrors' access='read'/>
;;   <property type='s' name='SmtpHost' access='read'/>
;;   <property type='b' name='SmtpUseAuth' access='read'/>
;;   <property type='b' name='SmtpAuthLogin' access='read'/>
;;   <property type='b' name='SmtpAuthPlain' access='read'/>
;;   <property type='b' name='SmtpAuthXoauth2' access='read'/>
;;   <property type='b' name='SmtpUseSsl' access='read'/>
;;   <property type='b' name='SmtpUseTls' access='read'/>
;;   <property type='s' name='SmtpUserName' access='read'/>
;; </interface>

(defconst tramp-goa-interface-chat "org.gnome.OnlineAccounts.Chat"
  "The chat interface of the GNOME Online Accounts.")

;; <interface name='org.gnome.OnlineAccounts.Chat'>
;; </interface>

(defconst tramp-goa-interface-photos "org.gnome.OnlineAccounts.Photos"
  "The photos interface of the GNOME Online Accounts.")

;; <interface name='org.gnome.OnlineAccounts.Photos'>
;; </interface>

(defconst tramp-goa-path-manager (concat tramp-goa-path "/Manager")
  "The object path of the GNOME Online Accounts manager.")

(defconst tramp-goa-interface-documents "org.gnome.OnlineAccounts.Manager"
  "The manager interface of the GNOME Online Accounts.")

;; <interface name='org.gnome.OnlineAccounts.Manager'>
;;   <method name='AddAccount'>
;;     <arg type='s' name='provider' direction='in'/>
;;     <arg type='s' name='identity' direction='in'/>
;;     <arg type='s' name='presentation_identity' direction='in'/>
;;     <arg type='a{sv}' name='credentials' direction='in'/>
;;     <arg type='a{ss}' name='details' direction='in'/>
;;     <arg type='o' name='account_object_path' direction='out'/>
;;   </method>
;; </interface>

;; The basic structure for GNOME Online Accounts.
(cl-defstruct (tramp-goa-account (:type list) :named) method user host port)

;;;###tramp-autoload
(defconst tramp-gvfs-service-afc-volumemonitor "org.gtk.vfs.AfcVolumeMonitor"
  "The well known name of the AFC volume monitor.")

;; This one is not needed yet.
(defconst tramp-gvfs-service-goa-volumemonitor "org.gtk.vfs.GoaVolumeMonitor"
  "The well known name of the GOA volume monitor.")

;;;###tramp-autoload
(defconst tramp-gvfs-service-gphoto2-volumemonitor
  "org.gtk.vfs.GPhoto2VolumeMonitor"
  "The well known name of the GPhoto2 volume monitor.")

;;;###tramp-autoload
(defconst tramp-gvfs-service-mtp-volumemonitor "org.gtk.vfs.MTPVolumeMonitor"
  "The well known name of the MTP volume monitor.")

(defconst tramp-gvfs-path-remotevolumemonitor
  "/org/gtk/Private/RemoteVolumeMonitor"
  "The object path of the remote volume monitor.")

(defconst tramp-gvfs-interface-remotevolumemonitor
  "org.gtk.Private.RemoteVolumeMonitor"
  "The volume monitor interface.")

;; <interface name='org.gtk.Private.RemoteVolumeMonitor'>
;;   <method name="IsSupported">
;;     <arg type='b' name='is_supported' direction='out'/>
;;   </method>
;;   <method name="List">
;;     <arg type='a(ssssbbbbbbbbuasa{ss}sa{sv})' name='drives' direction='out'/>
;;     <arg type='a(ssssssbbssa{ss}sa{sv})' name='volumes' direction='out'/>
;;     <arg type='a(ssssssbsassa{sv})' name='mounts' direction='out'/>
;;   </method>
;;   <method name="CancelOperation">
;;     <arg type='s' name='cancellation_id' direction='in'/>
;;     <arg type='b' name='was_cancelled' direction='out'/>
;;   </method>
;;   <method name="MountUnmount">
;;     <arg type='s' name='id' direction='in'/>
;;     <arg type='s' name='cancellation_id' direction='in'/>
;;     <arg type='u' name='unmount_flags' direction='in'/>
;;     <arg type='s' name='mount_op_id' direction='in'/>
;;   </method>
;;   <method name="VolumeMount">
;;     <arg type='s' name='id' direction='in'/>
;;     <arg type='s' name='cancellation_id' direction='in'/>
;;     <arg type='u' name='mount_flags' direction='in'/>
;;     <arg type='s' name='mount_op_id' direction='in'/>
;;   </method>
;;   <method name="DriveEject">
;;     <arg type='s' name='id' direction='in'/>
;;     <arg type='s' name='cancellation_id' direction='in'/>
;;     <arg type='u' name='unmount_flags' direction='in'/>
;;     <arg type='s' name='mount_op_id' direction='in'/>
;;   </method>
;;   <method name="DrivePollForMedia">
;;     <arg type='s' name='id' direction='in'/>
;;     <arg type='s' name='cancellation_id' direction='in'/>
;;   </method>
;;   <method name="DriveStart">
;;     <arg type='s' name='id' direction='in'/>
;;     <arg type='s' name='cancellation_id' direction='in'/>
;;     <arg type='u' name='flags' direction='in'/>
;;     <arg type='s' name='mount_op_id' direction='in'/>
;;   </method>
;;   <method name="DriveStop">
;;     <arg type='s' name='id' direction='in'/>
;;     <arg type='s' name='cancellation_id' direction='in'/>
;;     <arg type='u' name='unmount_flags' direction='in'/>
;;     <arg type='s' name='mount_op_id' direction='in'/>
;;   </method>
;;   <method name="MountOpReply">
;;     <arg type='s' name='mount_op_id' direction='in'/>
;;     <arg type='i' name='result' direction='in'/>
;;     <arg type='s' name='user_name' direction='in'/>
;;     <arg type='s' name='domain' direction='in'/>
;;     <arg type='s' name='encoded_password' direction='in'/>
;;     <arg type='i' name='password_save' direction='in'/>
;;     <arg type='i' name='choice' direction='in'/>
;;     <arg type='b' name='anonymous' direction='in'/>
;;   </method>
;;   <signal name="DriveChanged">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='(ssssbbbbbbbbuasa{ss}sa{sv})' name='drive'/>
;;   </signal>
;;   <signal name="DriveConnected">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='(ssssbbbbbbbbuasa{ss}sa{sv})' name='drive'/>
;;   </signal>
;;   <signal name="DriveDisconnected">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='(ssssbbbbbbbbuasa{ss}sa{sv})' name='drive'/>
;;   </signal>
;;   <signal name="DriveEjectButton">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='(ssssbbbbbbbbuasa{ss}sa{sv})' name='drive'/>
;;   </signal>
;;   <signal name="DriveStopButton">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='(ssssbbbbbbbbuasa{ss}sa{sv})' name='drive'/>
;;   </signal>
;;   <signal name="VolumeChanged">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='(ssssssbbssa{ss}sa{sv})' name='volume'/>
;;   </signal>
;;   <signal name="VolumeAdded">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='(ssssssbbssa{ss}sa{sv})' name='volume'/>
;;   </signal>
;;   <signal name="VolumeRemoved">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='(ssssssbbssa{ss}sa{sv})' name='volume'/>
;;   </signal>
;;   <signal name="MountChanged">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='(ssssssbsassa{sv})' name='mount'/>
;;   </signal>
;;   <signal name="MountAdded">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='(ssssssbsassa{sv})' name='mount'/>
;;   </signal>
;;   <signal name="MountPreUnmount">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='(ssssssbsassa{sv})' name='mount'/>
;;   </signal>
;;   <signal name="MountRemoved">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='(ssssssbsassa{sv})' name='mount'/>
;;   </signal>
;;   <signal name="MountOpAskPassword">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='s' name='message_to_show'/>
;;     <arg type='s' name='default_user'/>
;;     <arg type='s' name='default_domain'/>
;;     <arg type='u' name='flags'/>
;;   </signal>
;;   <signal name="MountOpAskQuestion">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='s' name='message_to_show'/>
;;     <arg type='as' name='choices'/>
;;   </signal>
;;   <signal name="MountOpShowProcesses">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='s' name='message_to_show'/>
;;     <arg type='ai' name='pid'/>
;;     <arg type='as' name='choices'/>
;;   </signal>
;;   <signal name="MountOpShowUnmountProgress">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;     <arg type='s' name='message_to_show'/>
;;     <arg type='x' name='time_left'/>
;;     <arg type='x' name='bytes_left'/>
;;   </signal>
;;   <signal name="MountOpAborted">
;;     <arg type='s' name='dbus_name'/>
;;     <arg type='s' name='id'/>
;;   </signal>
;; </interface>

;; STRUCT		volume
;;   STRING		  id
;;   STRING		  name
;;   STRING		  gicon_data
;;   STRING		  symbolic_gicon_data
;;   STRING		  uuid
;;   STRING		  activation_uri
;;   BOOLEAN		  can-mount
;;   BOOLEAN		  should-automount
;;   STRING		  drive-id
;;   STRING		  mount-id
;;   ARRAY		  identifiers
;;     DICT
;;       STRING		    key (unix-device, class, uuid, ...)
;;       STRING		    value
;;   STRING		  sort_key
;;   ARRAY		  expansion
;;     DICT
;;       STRING		    key (always-call-mount, is-removable, ...)
;;       VARIANT	    value (boolean?)

;; The basic structure for media devices.
(cl-defstruct (tramp-media-device (:type list) :named) method host port)

;; "gvfs-<command>" utilities have been deprecated in GVFS 1.31.1.  We
;; must use "gio <command>" tool instead.
(defconst tramp-gvfs-gio-mapping
  '(("gvfs-copy" . "copy")
    ("gvfs-info" . "info")
    ("gvfs-ls" . "list")
    ("gvfs-mkdir" . "mkdir")
    ("gvfs-monitor-file" . "monitor")
    ("gvfs-mount" . "mount")
    ("gvfs-move" . "move")
    ("gvfs-rm" . "remove")
    ("gvfs-set-attribute" . "set"))
  "List of cons cells, mapping \"gvfs-<command>\" to \"gio <command>\".")

;; <https://www.pygtk.org/docs/pygobject/gio-constants.html>
(eval-and-compile
  (defconst tramp-gvfs-file-attributes
    '("name"
      "type"
      "standard::display-name"
      "standard::symlink-target"
      "standard::is-volatile"
      "unix::nlink"
      "unix::uid"
      "owner::user"
      "unix::gid"
      "owner::group"
      "time::access"
      "time::modified"
      "time::changed"
      "standard::size"
      "unix::mode"
      "access::can-read"
      "access::can-write"
      "access::can-execute"
      "unix::inode"
      "unix::device")
    "GVFS file attributes."))

(defconst tramp-gvfs-file-attributes-with-gvfs-ls-regexp
  (rx
   blank (group (regexp (regexp-opt tramp-gvfs-file-attributes)))
   "=" (group (+? nonl)))
  "Regexp to parse GVFS file attributes with `gvfs-ls'.")

(defconst tramp-gvfs-file-attributes-with-gvfs-info-regexp
  (rx
   bol (* blank) (group (regexp (regexp-opt tramp-gvfs-file-attributes)))
   ":" (+ blank) (group (* nonl)) eol)
  "Regexp to parse GVFS file attributes with `gvfs-info'.")

(defconst tramp-gvfs-file-system-attributes
  '("filesystem::free"
    "filesystem::size"
    "filesystem::used")
  "GVFS file system attributes.")

(defconst tramp-gvfs-file-system-attributes-regexp
  (rx
   bol (* blank)
   (group (regexp (regexp-opt tramp-gvfs-file-system-attributes)))
   ":" (+ blank) (group (* nonl)) eol)
  "Regexp to parse GVFS file system attributes with `gvfs-info'.")

(defconst tramp-gvfs-nextcloud-default-prefix "/remote.php/webdav"
  "Default prefix for owncloud / nextcloud methods.")

(defconst tramp-gvfs-nextcloud-default-prefix-regexp
  (rx (literal tramp-gvfs-nextcloud-default-prefix) eol)
  "Regexp of default prefix for owncloud / nextcloud methods.")


;; New handlers should be added here.
;;;###tramp-autoload
(defconst tramp-gvfs-file-name-handler-alist
  '((abbreviate-file-name . tramp-handle-abbreviate-file-name)
    (access-file . tramp-handle-access-file)
    (add-name-to-file . tramp-handle-add-name-to-file)
    ;; `byte-compiler-base-file-name' performed by default handler.
    (copy-directory . tramp-handle-copy-directory)
    (copy-file . tramp-gvfs-handle-copy-file)
    (delete-directory . tramp-gvfs-handle-delete-directory)
    (delete-file . tramp-gvfs-handle-delete-file)
    ;; `diff-latest-backup-file' performed by default handler.
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes
     . tramp-handle-directory-files-and-attributes)
    (dired-compress-file . ignore)
    (dired-uncache . tramp-handle-dired-uncache)
    (exec-path . ignore)
    (expand-file-name . tramp-gvfs-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . ignore)
    (file-attributes . tramp-gvfs-handle-file-attributes)
    (file-directory-p . tramp-handle-file-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . tramp-gvfs-handle-file-executable-p)
    (file-exists-p . tramp-handle-file-exists-p)
    (file-group-gid . tramp-handle-file-group-gid)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-handle-file-local-copy)
    (file-locked-p . tramp-handle-file-locked-p)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-gvfs-handle-file-name-all-completions)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    ;; `file-name-sans-versions' performed by default handler.
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
    (file-notify-add-watch . tramp-gvfs-handle-file-notify-add-watch)
    (file-notify-rm-watch . tramp-handle-file-notify-rm-watch)
    (file-notify-valid-p . tramp-handle-file-notify-valid-p)
    (file-ownership-preserved-p . ignore)
    (file-readable-p . tramp-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . tramp-gvfs-handle-file-system-info)
    (file-truename . tramp-handle-file-truename)
    (file-user-uid . tramp-handle-file-user-uid)
    (file-writable-p . tramp-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-handle-insert-directory)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (list-system-processes . ignore)
    (load . tramp-handle-load)
    (lock-file . tramp-handle-lock-file)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . tramp-gvfs-handle-make-directory)
    (make-directory-internal . ignore)
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . ignore)
    (make-symbolic-link . tramp-handle-make-symbolic-link)
    (memory-info . ignore)
    (process-attributes . ignore)
    (process-file . ignore)
    (rename-file . tramp-gvfs-handle-rename-file)
    (set-file-acl . ignore)
    (set-file-modes . tramp-gvfs-handle-set-file-modes)
    (set-file-selinux-context . ignore)
    (set-file-times . tramp-gvfs-handle-set-file-times)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (shell-command . ignore)
    (start-file-process . ignore)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (tramp-get-home-directory . tramp-gvfs-handle-get-home-directory)
    (tramp-get-remote-gid . tramp-gvfs-handle-get-remote-gid)
    (tramp-get-remote-groups . ignore)
    (tramp-get-remote-uid . tramp-gvfs-handle-get-remote-uid)
    (tramp-set-file-uid-gid . tramp-gvfs-handle-set-file-uid-gid)
    (unhandled-file-name-directory . ignore)
    (unlock-file . tramp-handle-unlock-file)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . tramp-handle-write-region))
  "Alist of handler functions for Tramp GVFS method.
Operations not mentioned here will be handled by the default Emacs primitives.")

;; It must be a `defsubst' in order to push the whole code into
;; tramp-loaddefs.el.  Otherwise, there would be recursive autoloading.
;;;###tramp-autoload
(defsubst tramp-gvfs-file-name-p (vec-or-filename)
  "Check if it's a VEC-OR-FILENAME handled by the GVFS daemon."
  (and-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename))
	     (method (tramp-file-name-method vec))
	     ((member method tramp-gvfs-methods)))))

;;;###tramp-autoload
(defun tramp-gvfs-file-name-handler (operation &rest args)
  "Invoke the GVFS related OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION."
  ;; `file-remote-p' must not return an error.  (Bug#68976)
  (unless (or tramp-gvfs-enabled (eq operation 'file-remote-p))
    (tramp-user-error nil "Package `tramp-gvfs' not supported"))
  (if-let* ((filename (apply #'tramp-file-name-for-operation operation args))
            (tramp-gvfs-dbus-event-vector
             (and (tramp-tramp-file-p filename)
                  (tramp-dissect-file-name filename)))
            (fn (assoc operation tramp-gvfs-file-name-handler-alist)))
      (prog1 (save-match-data (apply (cdr fn) args))
	(setq tramp-debug-message-fnh-function (cdr fn)))
    (prog1 (tramp-run-real-handler operation args)
      (setq tramp-debug-message-fnh-function operation))))

;;;###tramp-autoload
(when (featurep 'dbusbind)
  (tramp--with-startup
   (tramp-register-foreign-file-name-handler
    #'tramp-gvfs-file-name-p #'tramp-gvfs-file-name-handler)))

;; Event type `dbus-event' is added to `while-no-input-ignore-events'
;; in Emacs 29.1.  If it is missing, some packages like Helm report
;; problems.  So we add it here.
(when (and (featurep 'dbusbind)
	   (not (memq 'dbus-event while-no-input-ignore-events)))
  (setq while-no-input-ignore-events
	(cons 'dbus-event while-no-input-ignore-events)))


;; D-Bus helper function.

(defun tramp-gvfs-dbus-string-to-byte-array (string)
  "Like `dbus-string-to-byte-array' but add trailing \\0 if needed."
  (dbus-string-to-byte-array
   (if (string-match-p (rx bol "(aya{sv})") tramp-gvfs-mountlocation-signature)
       (concat string (string 0)) string)))

(defun tramp-gvfs-dbus-byte-array-to-string (byte-array)
  "Like `dbus-byte-array-to-string' but remove trailing \\0 if exists.
Return nil for null BYTE-ARRAY."
  ;; The byte array could be a variant.  Take care.
  (when-let* ((byte-array
	       (if (and (consp byte-array) (atom (car byte-array)))
		   byte-array (car byte-array))))
    (dbus-byte-array-to-string
     (if (and (consp byte-array) (zerop (car (last byte-array))))
	 (butlast byte-array) byte-array))))

(defun tramp-gvfs-stringify-dbus-message (message)
  "Convert a D-Bus MESSAGE into readable UTF8 strings, used for traces."
  (cond
   ((and (consp message) (characterp (car message)))
    (format "%S" (tramp-gvfs-dbus-byte-array-to-string message)))
   ((and (consp message) (atom (cdr message)))
    (cons (tramp-gvfs-stringify-dbus-message (car message))
	  (tramp-gvfs-stringify-dbus-message (cdr message))))
   ((consp message)
    (mapcar #'tramp-gvfs-stringify-dbus-message message))
   ((stringp message)
    (format "%S" message))
   (t message)))

(defun tramp-dbus-function (vec func args)
  "Apply a D-Bus function FUNC from dbus.el.
The call will be traced by Tramp with trace level 6."
  (declare (tramp-suppress-trace t))
  (let (result)
    (tramp-message vec 6 "%s" (cons func args))
    (setq result (apply func args))
    (tramp-message vec 6 "%s" (tramp-gvfs-stringify-dbus-message result))
    result))

(defmacro with-tramp-dbus-call-method
  (vec synchronous bus service path interface method &rest args)
  "Apply a D-Bus call on bus BUS.

If SYNCHRONOUS is non-nil, the call is synchronously.  Otherwise,
it is an asynchronous call, with `ignore' as callback function.

The other arguments have the same meaning as with `dbus-call-method'
or `dbus-call-method-asynchronously'."
  (declare (indent 2) (debug t))
  `(let ((func (if ,synchronous
		   #'dbus-call-method #'dbus-call-method-asynchronously))
	 (args (append (list ,bus ,service ,path ,interface ,method)
		       (if ,synchronous (list ,@args) (list 'ignore ,@args)))))
     ;; We use `dbus-ignore-errors', because this macro is also called
     ;; when loading.
     (dbus-ignore-errors (tramp-dbus-function ,vec func args))))

(defmacro with-tramp-dbus-get-all-properties
  (vec bus service path interface)
  "Return all properties of INTERFACE.
The call will be traced by Tramp with trace level 6."
  (declare (indent 1) (debug t))
  ;; Check, that interface exists at object path.  Retrieve properties.
  `(when (member
	  ,interface
	  (tramp-dbus-function
	   ,vec #'dbus-introspect-get-interface-names
	   (list ,bus ,service ,path)))
     (tramp-dbus-function
      ,vec #'dbus-get-all-properties (list ,bus ,service ,path ,interface))))

(defvar tramp-gvfs-dbus-event-vector nil
  "Current Tramp file name to be used, as vector.
It is needed when D-Bus signals or errors arrive, because there
is no information where to trace the message.
The global value will always be nil; it is bound where needed.")

(defun tramp-gvfs-dbus-event-error (event err)
  "Called when a D-Bus error message arrives, see `dbus-event-error-functions'."
  (when tramp-gvfs-dbus-event-vector
    (tramp-message tramp-gvfs-dbus-event-vector 6 "%S" event)
    (tramp-error tramp-gvfs-dbus-event-vector 'file-error (cadr err))))

(add-hook 'dbus-event-error-functions #'tramp-gvfs-dbus-event-error)
(add-hook 'tramp-gvfs-unload-hook
	  (lambda ()
	    (remove-hook 'dbus-event-error-functions
			 #'tramp-gvfs-dbus-event-error)))


;; File name primitives.

(defun tramp-gvfs-info (filename &optional arg)
  "Check FILENAME via `gvfs-info'.
Set file property \"file-exists-p\" with the result."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (tramp-set-file-property
     v localname "file-exists-p"
     (tramp-gvfs-send-command
      v "gvfs-info" arg (tramp-gvfs-url-file-name filename)))))

(defun tramp-gvfs-do-copy-or-rename-file
  (op filename newname &optional ok-if-already-exists keep-date
   preserve-uid-gid preserve-extended-attributes)
  "Copy or rename a remote file.
OP must be `copy' or `rename' and indicates the operation to perform.
FILENAME specifies the file to copy or rename, NEWNAME is the name of
the new file (for copy) or the new name of the file (for rename).
OK-IF-ALREADY-EXISTS means don't barf if NEWNAME exists already.
KEEP-DATE means to make sure that NEWNAME has the same timestamp
as FILENAME.  PRESERVE-UID-GID, when non-nil, instructs to keep
the uid and gid if both files are on the same host.
PRESERVE-EXTENDED-ATTRIBUTES is ignored.

This function is invoked by `tramp-gvfs-handle-copy-file' and
`tramp-gvfs-handle-rename-file'.  It is an error if OP is neither
of `copy' and `rename'.  FILENAME and NEWNAME must be absolute
file names."
  ;; FILENAME and NEWNAME are already expanded.
  (unless (memq op '(copy rename))
    (error "Unknown operation `%s', must be `copy' or `rename'" op))

  ;; We cannot use `file-truename', this would fail for symlinks with
  ;; non-existing target.
  (setq filename (expand-file-name filename))
  (if (file-directory-p filename)
      (progn
	(copy-directory filename newname keep-date t)
	(when (eq op 'rename) (delete-directory filename 'recursive)))

    (let ((t1 (tramp-tramp-file-p filename))
	  (t2 (tramp-tramp-file-p newname))
	  (equal-remote (tramp-equal-remote filename newname))
	  (volatile
	   (and (eq op 'rename) (tramp-gvfs-file-name-p filename)
		(equal
		 (cdr
		  (assoc
		   "standard::is-volatile"
		   (tramp-gvfs-get-file-attributes filename)))
		 "TRUE")))
	  ;; "gvfs-rename" is not trustworthy.
	  (gvfs-operation (if (eq op 'copy) "gvfs-copy" "gvfs-move"))
	  (msg-operation (if (eq op 'copy) "Copying" "Renaming")))

      (with-parsed-tramp-file-name (if t1 filename newname) nil
	(tramp-barf-if-file-missing v filename
	  (when (and (not ok-if-already-exists) (file-exists-p newname))
	    (tramp-error v 'file-already-exists newname))
	  (when (and (file-directory-p newname)
		     (not (directory-name-p newname)))
	    (tramp-error v 'file-error "File is a directory %s" newname))
	  (when (file-regular-p newname)
	    (delete-file newname))

	  (cond
	   ;; We cannot rename volatile files, as used by Google-drive.
	   ((and (not equal-remote) volatile)
	    (prog1 (copy-file
		    filename newname ok-if-already-exists keep-date
		    preserve-uid-gid preserve-extended-attributes)
	      (delete-file filename)))

	   ;; We cannot copy or rename directly.
	   ((or (and equal-remote
		     (tramp-get-connection-property v "direct-copy-failed"))
		(and t1 (not (tramp-gvfs-file-name-p filename)))
		(and t2 (not (tramp-gvfs-file-name-p newname))))
	    (let ((tmpfile (tramp-compat-make-temp-file filename)))
	      (if (eq op 'copy)
		  (copy-file
		   filename tmpfile t keep-date preserve-uid-gid
		   preserve-extended-attributes)
		(rename-file filename tmpfile t))
	      (rename-file tmpfile newname ok-if-already-exists)))

	   ;; Direct action.
	   (t (with-tramp-progress-reporter
		  v 0 (format "%s %s to %s" msg-operation filename newname)
		(unless
		    (and (apply
			  #'tramp-gvfs-send-command v gvfs-operation
			  (append
			   (and (eq op 'copy) (or keep-date preserve-uid-gid)
				'("--preserve"))
			   (list
			    (tramp-gvfs-url-file-name filename)
			    (tramp-gvfs-url-file-name newname))))
			 ;; Some backends do not return a proper error
			 ;; code in case of direct copy/move.  Apply
			 ;; sanity checks.
			 (or (not equal-remote)
			     (and
			      (tramp-gvfs-info newname)
			      (or (eq op 'copy)
				  (not (tramp-gvfs-info filename))))))

		  (if (or (not equal-remote)
			  (and equal-remote
			       (tramp-get-connection-property
				v "direct-copy-failed")))
		      ;; Propagate the error.
		      (with-current-buffer (tramp-get-connection-buffer v)
			(goto-char (point-min))
			(tramp-error-with-buffer
			 nil v 'file-error
			 "%s failed, see buffer `%s' for details"
			 msg-operation (buffer-name)))

		    ;; Some WebDAV server, like the one from QNAP, do
		    ;; not support direct copy/move.  Try a fallback.
		    (tramp-set-connection-property v "direct-copy-failed" t)
		    (tramp-gvfs-do-copy-or-rename-file
		     op filename newname ok-if-already-exists keep-date
		     preserve-uid-gid preserve-extended-attributes))))

	      (when (and t1 (eq op 'rename))
		(with-parsed-tramp-file-name filename nil
		  (tramp-flush-file-properties v localname)))

	      (when t2
		(with-parsed-tramp-file-name newname nil
		  (tramp-flush-file-properties v localname))))))))))

(defun tramp-gvfs-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date
   preserve-uid-gid preserve-extended-attributes)
  "Like `copy-file' for Tramp files."
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))
  ;; At least one file a Tramp file?
  (if (or (tramp-tramp-file-p filename)
	  (tramp-tramp-file-p newname))
      (tramp-gvfs-do-copy-or-rename-file
       'copy filename newname ok-if-already-exists keep-date
       preserve-uid-gid preserve-extended-attributes)
    (tramp-run-real-handler
     #'copy-file
     (list filename newname ok-if-already-exists keep-date
	   preserve-uid-gid preserve-extended-attributes))))

(defun tramp-gvfs-handle-delete-directory (directory &optional recursive trash)
  "Like `delete-directory' for Tramp files."
  (tramp-skeleton-delete-directory directory recursive trash
    (if (and recursive (not (file-symlink-p directory)))
	(mapc (lambda (file)
		(if (eq t (file-attribute-type (file-attributes file)))
		    (delete-directory file recursive)
		  (delete-file file)))
	      (directory-files
	       directory 'full directory-files-no-dot-files-regexp))
      (unless (directory-empty-p directory)
	(tramp-error
	 v 'file-error "Couldn't delete non-empty %s" directory)))

    (unless (and (tramp-gvfs-send-command
		  v "gvfs-rm" (tramp-gvfs-url-file-name directory))
		 (not (tramp-gvfs-info directory)))
      ;; Propagate the error.
      (with-current-buffer (tramp-get-connection-buffer v)
	(goto-char (point-min))
	(tramp-error-with-buffer
	 nil v 'file-error "Couldn't delete %s" directory)))))

(defun tramp-gvfs-handle-delete-file (filename &optional trash)
  "Like `delete-file' for Tramp files."
  (tramp-skeleton-delete-file filename trash
    (unless (and (tramp-gvfs-send-command
		  v "gvfs-rm" (tramp-gvfs-url-file-name filename))
		 (not (tramp-gvfs-info filename)))
      ;; Propagate the error.
      (with-current-buffer (tramp-get-connection-buffer v)
	(goto-char (point-min))
	(tramp-error-with-buffer
	 nil v 'file-error "Couldn't delete %s" filename)))))

(defun tramp-gvfs-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for Tramp files."
  ;; If DIR is not given, use DEFAULT-DIRECTORY or "/".
  (setq dir (or dir default-directory "/"))
  ;; Handle empty NAME.
  (when (string-empty-p name)
    (setq name "."))
  ;; Unless NAME is absolute, concat DIR and NAME.
  (unless (file-name-absolute-p name)
    (setq name (file-name-concat dir name)))
  ;; If NAME is not a Tramp file, run the real handler.
  (if (not (tramp-tramp-file-p name))
      (tramp-run-real-handler #'expand-file-name (list name))
    ;; Dissect NAME.
    (with-parsed-tramp-file-name name nil
      ;; Tilde expansion shall be possible also for quoted localname.
      (when (string-prefix-p "~" (file-name-unquote localname))
	(setq localname (file-name-unquote localname)))
      ;; If there is a default location, expand tilde.
      (when (string-match
	     (rx bos "~" (group (* (not "/"))) (group (* nonl)) eos) localname)
	(let ((uname (match-string 1 localname))
	      (fname (match-string 2 localname))
	      hname)
	  (when (tramp-string-empty-or-nil-p uname)
	    (setq uname user))
	  (when (setq hname (tramp-get-home-directory v uname))
	    (setq localname (concat hname fname)))))
      ;; Tilde expansion is not possible.
      (when (and (not tramp-tolerate-tilde)
		 (string-prefix-p "~" localname))
	(tramp-error v 'file-error "Cannot expand tilde in file `%s'" name))
      (unless (tramp-run-real-handler #'file-name-absolute-p (list localname))
	(setq localname (concat "/" localname)))
      ;; We do not pass "/..".
      (if (string-match-p (rx bos (| "afp" (: "dav" (? "s")) "smb") eos) method)
	  (when (string-match
		 (rx bos "/" (+ (not "/")) (group "/.." (? "/"))) localname)
	    (setq localname (replace-match "/" t t localname 1)))
	(when (string-match (rx bol "/.." (? "/")) localname)
	  (setq localname (replace-match "/" t t localname))))
      ;; There might be a double slash.  Remove this.
      (while (string-match "//" localname)
	(setq localname (replace-match "/" t t localname)))
      ;; Do not keep "/..".
      (when (string-match-p (rx bos "/" (** 1 2 ".") eos) localname)
	(setq localname "/"))
      ;; Do normal `expand-file-name' (this does "/./" and "/../"),
      ;; unless there are tilde characters in file name.
      (tramp-make-tramp-file-name
       v (if (string-prefix-p "~" localname)
	     localname
	   (tramp-run-real-handler #'expand-file-name (list localname)))))))

(defun tramp-gvfs-get-directory-attributes (directory)
  "Return GVFS attributes association list of all files in DIRECTORY."
  ;; Don't modify `last-coding-system-used' by accident.
  (let ((last-coding-system-used last-coding-system-used)
	result)
    (with-parsed-tramp-file-name (expand-file-name directory) nil
      (with-tramp-file-property v localname "directory-attributes"
	(tramp-message v 5 "directory gvfs attributes: %s" localname)
	;; Send command.
	(tramp-gvfs-send-command
	 v "gvfs-ls" "-h"
	 (unless (string-equal (file-remote-p directory 'method) "gdrive") "-n")
	 "-a" (string-join tramp-gvfs-file-attributes ",")
	 (tramp-gvfs-url-file-name directory))
	;; Parse output.
	(with-current-buffer (tramp-get-connection-buffer v)
	  (goto-char (point-min))
	  (while (looking-at
		  (rx
		   bol (group (+ nonl)) blank
		   (group (+ digit)) blank
		   "(" (group (+? nonl)) ")"
		   (regexp tramp-gvfs-file-attributes-with-gvfs-ls-regexp)))
	    (let ((item (list (cons "type" (match-string 3))
			      (cons "standard::size" (match-string 2))
			      (cons "name" (match-string 1)))))
	      (goto-char (1+ (match-end 3)))
	      (while (looking-at
		      (rx
		       (regexp tramp-gvfs-file-attributes-with-gvfs-ls-regexp)
		       (group
			(| (regexp
			    tramp-gvfs-file-attributes-with-gvfs-ls-regexp)
			   eol))))
		(push (cons (match-string 1) (match-string 2)) item)
		(goto-char (match-end 2)))
	      ;; Add display name as head.
	      (push
	       (cons (cdr (or (assoc "standard::display-name" item)
			      (assoc "name" item)))
		     (nreverse item))
	       result))
	    (forward-line)))
	result))))

(defun tramp-gvfs-get-root-attributes (filename &optional file-system)
  "Return GVFS attributes association list of FILENAME.
If FILE-SYSTEM is non-nil, return file system attributes."
  ;; Don't modify `last-coding-system-used' by accident.
  (let ((last-coding-system-used last-coding-system-used)
	result)
    (with-parsed-tramp-file-name (expand-file-name filename) nil
      (with-tramp-file-property
	  v localname
	  (if file-system "file-system-attributes" "file-attributes")
	(tramp-message
	 v 5 "file%s gvfs attributes: %s"
	 (if file-system " system" "") localname)
	;; Send command.
	(if file-system
	    (tramp-gvfs-info filename "--filesystem")
	  (tramp-gvfs-info filename))
	;; Parse output.
	(with-current-buffer (tramp-get-connection-buffer v)
	  (goto-char (point-min))
	  (while (search-forward-regexp
		  (if file-system
		      tramp-gvfs-file-system-attributes-regexp
		    tramp-gvfs-file-attributes-with-gvfs-info-regexp)
		  nil t)
	    (push (cons (match-string 1) (match-string 2)) result))
	  result)))))

(defun tramp-gvfs-get-file-attributes (filename)
  "Return GVFS attributes association list of FILENAME."
  (setq filename (directory-file-name (expand-file-name filename)))
  (with-parsed-tramp-file-name filename nil
    (setq localname (file-name-unquote localname))
    (if (or (and (string-match-p
		  (rx bol (| "afp" (: "dav" (? "s")) "smb") eol) method)
		 (string-match-p (rx bol (? "/") (+ (not "/")) eol) localname))
	    (string-equal localname "/"))
	(tramp-gvfs-get-root-attributes filename)
      (assoc
       (file-name-nondirectory filename)
       (tramp-gvfs-get-directory-attributes (file-name-directory filename))))))

(defun tramp-gvfs-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  (unless id-format (setq id-format 'integer))
  (let ((attributes (tramp-gvfs-get-file-attributes filename))
	dirp res-symlink-target res-numlinks res-uid res-gid res-access
	res-mod res-change res-size res-filemodes res-inode res-device)
    (when attributes
      ;; ... directory or symlink
      (setq dirp (if (equal "directory" (cdr (assoc "type" attributes))) t))
      (setq res-symlink-target
	    ;; Google-drive creates file blobs and links to them.  We
	    ;; don't want to see them.
	    (and
	     (not
	      (equal (cdr (assoc "standard::is-volatile" attributes)) "TRUE"))
	     (cdr (assoc "standard::symlink-target" attributes))))
      (when (stringp res-symlink-target)
	(setq res-symlink-target
	      ;; Parse unibyte codes "\xNN".  We assume they are
	      ;; non-ASCII codepoints in the range #x80 through #xff.
	      ;; Convert them to multibyte.
	      (decode-coding-string
	       (replace-regexp-in-string
		(rx "\\x" (group (= 2 xdigit)))
		(lambda (x)
		  (unibyte-string (string-to-number (match-string 1 x) 16)))
		res-symlink-target)
	       'utf-8)))
      ;; ... number links
      (setq res-numlinks
	    (string-to-number
	     (or (cdr (assoc "unix::nlink" attributes)) "0")))
      ;; ... uid and gid
      (setq res-uid
	    (if (eq id-format 'integer)
		(string-to-number
		 (or (cdr (assoc "unix::uid" attributes))
		     (number-to-string tramp-unknown-id-integer)))
	      (or (cdr (assoc "owner::user" attributes))
		  (cdr (assoc "unix::uid" attributes))
		  tramp-unknown-id-string)))
      (setq res-gid
	    (if (eq id-format 'integer)
		(string-to-number
		 (or (cdr (assoc "unix::gid" attributes))
		     (number-to-string tramp-unknown-id-integer)))
	      (or (cdr (assoc "owner::group" attributes))
		  (cdr (assoc "unix::gid" attributes))
		  tramp-unknown-id-string)))
      ;; ... last access, modification and change time
      (setq res-access
	    (seconds-to-time
	     (string-to-number
	      (or (cdr (assoc "time::access" attributes)) "0"))))
      (setq res-mod
	    (seconds-to-time
	     (string-to-number
	      (or (cdr (assoc "time::modified" attributes)) "0"))))
      (setq res-change
	    (seconds-to-time
	     (string-to-number
	      (or (cdr (assoc "time::changed" attributes)) "0"))))
      ;; ... size
      (setq res-size
	    (string-to-number
	     (or (cdr (assoc "standard::size" attributes)) "0")))
      ;; ... file mode flags
      (setq res-filemodes
	    (if-let* ((n (cdr (assoc "unix::mode" attributes))))
		(tramp-file-mode-from-int (string-to-number n))
	      (format
	       "%s%s%s%s------"
	       (if dirp "d" (if res-symlink-target "l" "-"))
	       (if (equal (cdr (assoc "access::can-read" attributes))
			  "FALSE")
		   "-" "r")
	       (if (equal (cdr (assoc "access::can-write" attributes))
			  "FALSE")
		   "-" "w")
	       (if (equal (cdr (assoc "access::can-execute" attributes))
			  "FALSE")
		   "-" "x"))))
      ;; ... inode and device
      (setq res-inode
	    (if-let* ((n (cdr (assoc "unix::inode" attributes))))
		(string-to-number n)
	      (tramp-get-inode (tramp-dissect-file-name filename))))
      (setq res-device
	    (if-let* ((n (cdr (assoc "unix::device" attributes))))
		(string-to-number n)
	      (tramp-get-device (tramp-dissect-file-name filename))))

      ;; Return data gathered.
      (list
       ;; 0. t for directory, string (name linked to) for
       ;; symbolic link, or nil.
       (or dirp res-symlink-target)
       ;; 1. Number of links to file.
       res-numlinks
       ;; 2. File uid.
       res-uid
       ;; 3. File gid.
       res-gid
       ;; 4. Last access time, as a list of integers.
       ;; 5. Last modification time, likewise.
       ;; 6. Last status change time, likewise.
       res-access res-mod res-change
       ;; 7. Size in bytes (-1, if number is out of range).
       res-size
       ;; 8. File modes.
       res-filemodes
       ;; 9. t if file's gid would change if file were deleted
       ;; and recreated.
       nil
       ;; 10. Inode number.
       res-inode
       ;; 11. Device number.
       res-device
       ))))

(defun tramp-gvfs-handle-file-executable-p (filename)
  "Like `file-executable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-executable-p"
      (or (tramp-check-cached-permissions v ?x)
	  (tramp-check-cached-permissions v ?s)))))

(defun tramp-gvfs-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (tramp-skeleton-file-name-all-completions filename directory
    (unless (string-search "/" filename)
      (all-completions
       filename
       (with-parsed-tramp-file-name (expand-file-name directory) nil
	 (with-tramp-file-property v localname "file-name-all-completions"
           (let (result)
             ;; Get a list of directories and files.
	     (dolist (item
		      (tramp-gvfs-get-directory-attributes directory)
		      result)
	       (if (string-equal (cdr (assoc "type" item)) "directory")
		   (push (file-name-as-directory (car item)) result)
		 (push (car item) result))))))))))

(defun tramp-gvfs-handle-file-notify-add-watch (file-name flags _callback)
  "Like `file-notify-add-watch' for Tramp files."
  (setq file-name (expand-file-name file-name))
  (with-parsed-tramp-file-name file-name nil
    ;; TODO: We cannot watch directories, because `gio monitor' is not
    ;; supported for gvfs-mounted directories.  However,
    ;; `file-notify-add-watch' uses directories.
    (when (or (not (tramp-gvfs-gio-tool-p v)) (file-directory-p file-name))
      (tramp-error
       v 'file-notify-error "Monitoring not supported for `%s'" file-name))
    (let* ((default-directory (file-name-directory file-name))
	   (events
	    (cond
	     ((and (memq 'change flags) (memq 'attribute-change flags))
	      '(created changed changes-done-hint moved deleted
			attribute-changed unmounted))
	     ((memq 'change flags)
	      '(created changed changes-done-hint moved deleted unmounted))
	     ((memq 'attribute-change flags) '(attribute-changed unmounted))))
	   (p (apply
	       #'start-process
	       "gvfs-monitor" (generate-new-buffer " *gvfs-monitor*")
	       `("gio" "monitor" ,(tramp-gvfs-url-file-name file-name)))))
      (if (not (processp p))
	  (tramp-error
	   v 'file-notify-error "Monitoring not supported for `%s'" file-name)
	(process-put p 'tramp-events events)
	(process-put p 'tramp-watch-name localname)
	(set-process-filter p #'tramp-gvfs-monitor-process-filter)
	(set-process-sentinel p #'tramp-file-notify-process-sentinel)
	(tramp-post-process-creation p v)
	;; There might be an error if the monitor is not supported.
	;; Give the filter a chance to read the output.
	(while (tramp-accept-process-output p))
	(unless (process-live-p p)
	  (tramp-error
	   p 'file-notify-error "Monitoring not supported for `%s'" file-name))
	;; Set "gio-file-monitor" property.  We believe, that "gio
	;; monitor" uses polling when applied for mounted files.
	(tramp-set-connection-property p "gio-file-monitor" 'GPollFileMonitor)
	p))))

(defun tramp-gvfs-monitor-process-filter (proc string)
  "Read output from \"gvfs-monitor-file\" and add corresponding \
`file-notify' events."
  (let* ((events (process-get proc 'tramp-events))
	 (rest-string (process-get proc 'tramp-rest-string))
	 (dd (tramp-get-default-directory (process-buffer proc)))
	 (ddu (rx (literal (tramp-gvfs-url-file-name dd)))))
    (when rest-string
      (tramp-message proc 10 "Previous string:\n%s" rest-string))
    (tramp-message proc 6 "%S\n%s" proc string)
    (setq string (concat rest-string string)
          ;; Fix action names.
          string (string-replace "attributes changed" "attribute-changed" string)
          string (string-replace "changes done" "changes-done-hint" string)
          string (string-replace "renamed to" "moved" string))
    ;; https://bugs.launchpad.net/bugs/1742946
    (when
	(string-match-p
	 (rx (| "Monitoring not supported" "No locations given")) string)
      (delete-process proc))

    (while (string-match
	    (rx
	     bol (+ nonl) ":"
	     blank (group (+ nonl)) ":"
	     blank (group (regexp (regexp-opt tramp-gio-events)))
	     (? (group blank (group (+ nonl)))) eol)
	    string)

      (let ((file (match-string 1 string))
	    (file1 (match-string 4 string))
	    (action (intern-soft (match-string 2 string))))
	(setq string (replace-match "" nil nil string))
	;; File names are returned as URL paths.  We must convert them.
	(when (string-match ddu file)
	  (setq file (replace-match dd nil nil file)))
	(while (string-match-p (rx "%" (= 2 xdigit)) file)
	  (setq file (url-unhex-string file)))
	(when (string-match ddu (or file1 ""))
	  (setq file1 (replace-match dd nil nil file1)))
	(while (string-match-p (rx "%" (= 2 xdigit)) (or file1 ""))
	  (setq file1 (url-unhex-string file1)))
	;; Remove watch when file or directory to be watched is deleted.
	(when (and (member action '(moved deleted))
		   (string-equal file (process-get proc 'tramp-watch-name)))
	  (delete-process proc))
        ;; Add an Emacs event now.
	;; `insert-special-event' exists since Emacs 31.
	(when (member action events)
	  (tramp-compat-funcall
              (if (fboundp 'insert-special-event)
                  'insert-special-event
	        (lookup-key special-event-map [file-notify]))
	    `(file-notify
              ,(list proc action file file1) file-notify-callback)))))

    ;; Save rest of the string.
    (when (string-empty-p string) (setq string nil))
    (when string (tramp-message proc 10 "Rest string:\n%s" string))
    (process-put proc 'tramp-rest-string string)))

(defun tramp-gvfs-handle-file-system-info (filename)
  "Like `file-system-info' for Tramp files."
  (setq filename (directory-file-name (expand-file-name filename)))
  (with-parsed-tramp-file-name filename nil
    ;; We don't use cached values.
    (tramp-flush-file-property v localname "file-system-attributes")
    (let* ((attr (tramp-gvfs-get-root-attributes filename 'file-system))
	   (size (cdr (assoc "filesystem::size" attr)))
	   (used (cdr (assoc "filesystem::used" attr)))
	   (free (cdr (assoc "filesystem::free" attr))))
      (when (or size free)
	(list (and size (string-to-number size))
	      (and free (string-to-number free))
	      ;; "mtp" connections do not return "filesystem::used".
	      (or (and size used
		       (- (string-to-number size) (string-to-number used)))
		  (and free (string-to-number free))))))))

(defun tramp-gvfs-handle-make-directory (dir &optional parents)
  "Like `make-directory' for Tramp files."
  (tramp-skeleton-make-directory dir parents
    (save-match-data
      (if (and (tramp-gvfs-send-command
		v "gvfs-mkdir" (tramp-gvfs-url-file-name dir))
	       (tramp-gvfs-info dir))
	  (set-file-modes dir (default-file-modes))
	(tramp-error v 'file-error "Couldn't make directory %s" dir)))))

(defun tramp-gvfs-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for Tramp files."
  ;; Check if both files are local -- invoke normal rename-file.
  ;; Otherwise, use Tramp from local system.
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))
  ;; At least one file a Tramp file?
  (if (or (tramp-tramp-file-p filename)
          (tramp-tramp-file-p newname))
      (tramp-gvfs-do-copy-or-rename-file
       'rename filename newname ok-if-already-exists
       'keep-date 'preserve-uid-gid)
    (tramp-run-real-handler
     #'rename-file (list filename newname ok-if-already-exists))))

(defun tramp-gvfs-set-attribute (vec &rest args)
  "Call \"gio set ...\" if possible."
  (let ((key (concat "gvfs-set-attribute-" (nth 3 args))))
    (when (tramp-get-connection-property vec key t)
      (or (apply #'tramp-gvfs-send-command vec "gvfs-set-attribute" args)
	  (with-current-buffer (tramp-get-connection-buffer vec)
	    (goto-char (point-min))
	    (when (looking-at-p "gio: Operation not supported")
	      (tramp-set-connection-property vec key nil)))))))

(defun tramp-gvfs-handle-set-file-modes (filename mode &optional flag)
  "Like `set-file-modes' for Tramp files."
  (tramp-skeleton-set-file-modes-times-uid-gid filename
    (tramp-gvfs-set-attribute
     v (if (eq flag 'nofollow) "-nt" "-t") "uint32"
     (tramp-gvfs-url-file-name filename) "unix::mode" (number-to-string mode))))

(defun tramp-gvfs-handle-set-file-times (filename &optional time flag)
  "Like `set-file-times' for Tramp files."
  (tramp-skeleton-set-file-modes-times-uid-gid filename
    (tramp-gvfs-set-attribute
     v (if (eq flag 'nofollow) "-nt" "-t") "uint64"
     (tramp-gvfs-url-file-name filename) "time::modified"
     (format-time-string "%s" (tramp-defined-time time)))))

(defun tramp-gvfs-handle-get-home-directory (vec &optional _user)
  "The remote home directory for connection VEC as local file name.
If USER is a string, return its home directory instead of the
user identified by VEC.  If there is no user specified in either
VEC or USER, or if there is no home directory, return nil."
  (let ((localname (tramp-get-connection-property vec "default-location"))
	result)
    (cond
     ((tramp-string-empty-or-nil-p localname)
      (tramp-get-connection-property (tramp-get-process vec) "share"))
     ;; Google-drive.
     ((not (string-prefix-p "/" localname))
      (dolist (item
	       (tramp-gvfs-get-directory-attributes
		(tramp-make-tramp-file-name vec "/"))
	       result)
	(when (string-equal (cdr (assoc "name" item)) localname)
	  (setq result (concat "/" (car item))))))
     (t localname))))

(defun tramp-gvfs-handle-get-remote-uid (vec id-format)
  "The uid of the remote connection VEC, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  ;; The result is cached in `tramp-get-remote-uid'.
  (if (equal id-format 'string)
      (tramp-file-name-user vec)
    (and-let* ((localname
		(tramp-get-connection-property (tramp-get-process vec) "share"))
	       ((file-attribute-user-id
		 (file-attributes
		  (tramp-make-tramp-file-name vec localname) id-format)))))))

(defun tramp-gvfs-handle-get-remote-gid (vec id-format)
  "The gid of the remote connection VEC, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  ;; The result is cached in `tramp-get-remote-gid'.
  (and-let* ((localname
	      (tramp-get-connection-property (tramp-get-process vec) "share"))
	     ((file-attribute-group-id
	       (file-attributes
		(tramp-make-tramp-file-name vec localname) id-format))))))

(defun tramp-gvfs-handle-set-file-uid-gid (filename &optional uid gid)
  "Like `tramp-set-file-uid-gid' for Tramp files."
  (tramp-skeleton-set-file-modes-times-uid-gid filename
    (when (natnump uid)
      (tramp-gvfs-set-attribute
       v "-t" "uint32"
       (tramp-gvfs-url-file-name filename) "unix::uid" (number-to-string uid)))
    (when (natnump gid)
      (tramp-gvfs-set-attribute
       v "-t" "uint32"
       (tramp-gvfs-url-file-name filename)
       "unix::gid" (number-to-string gid)))))


;; File name conversions.

(defun tramp-gvfs-activation-uri (filename)
  "Return activation URI to be used in gio commands."
  (if (tramp-tramp-file-p filename)
      (with-parsed-tramp-file-name filename nil
	;; Ensure that media devices are cached.
	(when (string-equal method "mtp")
	  (tramp-get-media-device v))
	(with-tramp-connection-property v "activation-uri"
	  (setq localname "/")
	  (when (string-equal "gdrive" method)
	    (setq method "google-drive"))
	  (when (string-equal "nextcloud" method)
	    (setq method "davs"
		  localname
		  (concat (tramp-gvfs-get-remote-prefix v) localname)))
	  (when-let*
	      (((string-equal "mtp" method))
	       (media (tramp-get-connection-property v "media-device")))
	    (setq method (tramp-media-device-method media)
		  host (tramp-media-device-host media)
		  port (tramp-media-device-port media)))
	  (when (and user domain)
	    (setq user (concat domain ";" user)))
	  (url-recreate-url
	   (url-parse-make-urlobj
	    method (and user (url-hexify-string user))
	    nil (and host (url-hexify-string host))
	    (if (stringp port) (string-to-number port) port)
	    localname nil nil t))))
    ;; Local URI.
    (url-recreate-url
     (url-parse-make-urlobj "file" nil nil nil nil nil nil nil t))))

(defun tramp-gvfs-url-file-name (filename)
  "Return FILENAME in URL syntax."
  (setq filename (file-name-unquote filename))
  (let* (;; "/" must NOT be hexified.
	 (url-unreserved-chars (cons ?/ url-unreserved-chars))
	 (result
	  (concat (substring (tramp-gvfs-activation-uri filename) 0 -1)
		  (url-hexify-string (tramp-file-local-name filename)))))
    (when (tramp-tramp-file-p filename)
      (tramp-message
       (tramp-dissect-file-name filename) 10
       "remote file `%s' is URL `%s'" filename result))
    result))

(defun tramp-gvfs-object-path (filename)
  "Create a D-Bus object path from FILENAME."
  (expand-file-name (dbus-escape-as-identifier filename) tramp-gvfs-path-tramp))

(defun tramp-gvfs-file-name (object-path)
  "Retrieve file name from D-Bus OBJECT-PATH."
  (dbus-unescape-from-identifier
   (replace-regexp-in-string
    (rx bol (* nonl) "/" (group (+ (not "/"))) eol) "\\1" object-path)))

(defun tramp-gvfs-url-host (url)
  "Return the host name part of URL, a string.
We cannot use `url-host', because `url-generic-parse-url' returns
a downcased host name only."
  (and (stringp url)
       (string-match (rx bol (+ alnum) "://" (group (+ (not (any "/:"))))) url)
       (match-string 1 url)))

;; This is used in GNU ELPA package tramp-locproc.el.
(defun tramp-gvfs-local-file-name (filename)
  "Return local mount name of FILENAME."
  (setq filename (file-name-unquote (expand-file-name filename)))
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "local-file-name"
      ;; As long as we call `tramp-gvfs-maybe-open-connection' here,
      ;; we cache the result.
      (tramp-gvfs-maybe-open-connection v)
      (let ((quoted (file-name-quoted-p localname))
	    (localname (file-name-unquote localname)))
	(funcall
	 (if quoted #'file-name-quote #'identity)
	 (expand-file-name
	  (if (file-name-absolute-p localname)
	      (substring localname 1) localname)
	  (tramp-get-file-property v "/" "fuse-mountpoint")))))))


;; D-Bus GVFS functions.

(defun tramp-gvfs-handler-askpassword (message user domain flags)
  "Implementation for the \"org.gtk.vfs.MountOperation.askPassword\" method."
  (let* ((filename
	  (tramp-gvfs-file-name (dbus-event-path-name last-input-event)))
	 (pw-prompt
	  (format
	   "%s for %s "
	   (if (string-match
		(rx (group (any "Pp") (| "assword" "assphrase"))) message)
	       (capitalize (match-string 1 message))
	     "Password")
	   filename))
	 password)

    (condition-case nil
	(with-parsed-tramp-file-name filename l
	  (when (and (tramp-string-empty-or-nil-p user)
		     (not
		      (zerop (logand flags tramp-gvfs-password-need-username))))
	    (setq user (read-string "User name: ")))
	  (when (and (tramp-string-empty-or-nil-p domain)
		     (not
		      (zerop (logand flags tramp-gvfs-password-need-domain))))
	    (setq domain (read-string "Domain name: ")))

	  (tramp-message l 6 "%S %S %S %d" message user domain flags)
	  (unless (tramp-get-connection-property l " first-password-request")
	    (tramp-clear-passwd l))

	  (setq password (tramp-read-passwd
			  (tramp-get-connection-process l) pw-prompt))

	  ;; Return result.
	  (if (stringp password)
	      (list
	       t ;; password handled.
	       nil ;; no abort of D-Bus.
	       password
	       (tramp-file-name-user l)
	       domain
	       nil ;; not anonymous.
	       0) ;; no password save.
	    ;; No password provided.
	    (list nil t "" (tramp-file-name-user l) domain nil 0)))

      ;; When QUIT is raised, we shall return this information to D-Bus.
      (quit (list nil t "" "" "" nil 0)))))

(defun tramp-gvfs-handler-askquestion (message choices)
  "Implementation for the \"org.gtk.vfs.MountOperation.askQuestion\" method."
  (save-window-excursion
    (let ((enable-recursive-minibuffers t)
	  (use-dialog-box (and use-dialog-box (null noninteractive)))
	  result)

      (with-parsed-tramp-file-name
	  (tramp-gvfs-file-name (dbus-event-path-name last-input-event)) nil
	(tramp-message v 6 "%S %S" message choices)

	(setq result
	      (condition-case nil
		  (list
		   t ;; handled.
		   nil ;; no abort of D-Bus.
		   ;; Preserve message for `progress-reporter'.
		   (with-temp-message ""
		     (if noninteractive
			 ;; Keep regression tests running.
			 (progn
			   (message "%s" message)
			   0)
		       (with-tramp-connection-property
			   (tramp-get-process v) message
			 ;; In theory, there can be several choices.
			 ;; Until now, there is only the question
			 ;; whether to accept an unknown host
			 ;; signature or certificate.
			 (with-temp-buffer
			   (insert message)
			   (goto-char (point-max))
			   (pop-to-buffer (current-buffer))
			   (if (yes-or-no-p
				(buffer-substring
				 (line-beginning-position) (point)))
			       0 1))))))

		;; When QUIT is raised, we shall return this
		;; information to D-Bus.
		(quit (list nil t 1))))

	(tramp-message v 6 "%s" result)

	;; When the choice is "no", we set a dummy fuse-mountpoint in
	;; order to leave the timeout.
	(unless (zerop (caddr result))
	  (tramp-set-file-property v "/" "fuse-mountpoint" "/"))

	result))))

(defun tramp-gvfs-handler-mounted-unmounted (mount-info)
  "Signal handler for the gvfs \"mounted\" and \"unmounted\" signals.
Their full names are \"org.gtk.vfs.MountTracker.mounted\" and
\"org.gtk.vfs.MountTracker.unmounted\"."
  (ignore-errors
    (let ((signal-name (dbus-event-member-name last-input-event))
	  (elt mount-info))
      ;; Jump over the first elements of the mount info.  Since there
      ;; were changes in the entries, we cannot access dedicated
      ;; elements.
      (while (stringp (car elt)) (setq elt (cdr elt)))
      (let* ((fuse-mountpoint (tramp-gvfs-dbus-byte-array-to-string (cadr elt)))
	     (mount-spec (caddr elt))
	     (prefix (tramp-gvfs-dbus-byte-array-to-string (car mount-spec)))
	     (default-location (tramp-gvfs-dbus-byte-array-to-string
				(cadddr elt)))
	     (method (tramp-gvfs-dbus-byte-array-to-string
		      (cadr (assoc "type" (cadr mount-spec)))))
	     (user (tramp-gvfs-dbus-byte-array-to-string
		    (cadr (assoc "user" (cadr mount-spec)))))
	     (domain (tramp-gvfs-dbus-byte-array-to-string
		      (cadr (assoc "domain" (cadr mount-spec)))))
	     (host (tramp-gvfs-dbus-byte-array-to-string
		    (cadr (or (assoc "host" (cadr mount-spec))
			      (assoc "server" (cadr mount-spec))))))
	     (port (tramp-gvfs-dbus-byte-array-to-string
		    (cadr (assoc "port" (cadr mount-spec)))))
	     (ssl (tramp-gvfs-dbus-byte-array-to-string
		   (cadr (assoc "ssl" (cadr mount-spec)))))
	     (uri (tramp-gvfs-dbus-byte-array-to-string
		   (cadr (assoc "uri" (cadr mount-spec))))))
	(when (string-match (rx bol (group (| "afp" "smb"))) method)
	  (setq method (match-string 1 method)))
	(when (and (string-equal "dav" method) (string-equal "true" ssl))
	  (setq method "davs"))
	(when (and (string-equal "davs" method)
		   (string-match-p
		    tramp-gvfs-nextcloud-default-prefix-regexp prefix))
	  (setq method "nextcloud"))
	(when (string-equal "google-drive" method)
	  (setq method "gdrive"))
	(when (and (string-equal "http" method) (stringp uri))
	  (setq host (tramp-gvfs-url-host uri)
		uri (url-generic-parse-url uri)
		method (url-type uri)
		user (url-user uri)
		port (url-portspec uri)))
	(when (member method tramp-media-methods)
	  ;; Ensure that media devices are cached.
	  (tramp-get-media-devices nil)
	  (when-let* ((v (tramp-get-connection-property
			  (make-tramp-media-device
			   :method method :host host :port port)
			  "vector" nil)))
	    (setq method (tramp-file-name-method v)
		  host (tramp-file-name-host v)
		  port (tramp-file-name-port v))))
	(when (member method tramp-gvfs-methods)
	  (let ((v (make-tramp-file-name
		    :method method :user user :domain domain
		    :host host :port port)))
	    (tramp-message
	     v 6 "%s %s"
	     signal-name (tramp-gvfs-stringify-dbus-message mount-info))
	    (tramp-flush-file-property v "/" "list-mounts")
	    (if (tramp-compat-string-equal-ignore-case signal-name "unmounted")
		(tramp-flush-file-properties v "/")
	      ;; Set mountpoint and location.
	      (tramp-set-file-property v "/" "fuse-mountpoint" fuse-mountpoint)
	      (tramp-set-connection-property
	       v "default-location" default-location))))))))

(when tramp-gvfs-enabled
  (dbus-register-signal
   :session nil tramp-gvfs-path-mounttracker
   tramp-gvfs-interface-mounttracker "mounted"
   #'tramp-gvfs-handler-mounted-unmounted)
  (dbus-register-signal
   :session nil tramp-gvfs-path-mounttracker
   tramp-gvfs-interface-mounttracker "Mounted"
   #'tramp-gvfs-handler-mounted-unmounted)

  (dbus-register-signal
   :session nil tramp-gvfs-path-mounttracker
   tramp-gvfs-interface-mounttracker "unmounted"
   #'tramp-gvfs-handler-mounted-unmounted)
  (dbus-register-signal
   :session nil tramp-gvfs-path-mounttracker
   tramp-gvfs-interface-mounttracker "Unmounted"
   #'tramp-gvfs-handler-mounted-unmounted))

(defun tramp-gvfs-connection-mounted-p (vec)
  "Check, whether the location is already mounted."
  (or
   (tramp-get-file-property vec "/" "fuse-mountpoint")
   (catch 'mounted
     (dolist
	 (elt
	  (with-tramp-file-property vec "/" "list-mounts"
	    (with-tramp-dbus-call-method vec t
	      :session tramp-gvfs-service-daemon tramp-gvfs-path-mounttracker
	      tramp-gvfs-interface-mounttracker tramp-gvfs-listmounts)))
       ;; Jump over the first elements of the mount info.  Since there
       ;; were changes in the entries, we cannot access dedicated
       ;; elements.
       (while (stringp (car elt)) (setq elt (cdr elt)))
       (let* ((fuse-mountpoint (tramp-gvfs-dbus-byte-array-to-string
				(cadr elt)))
	      (mount-spec (caddr elt))
	      (prefix (tramp-gvfs-dbus-byte-array-to-string (car mount-spec)))
	      (default-location (tramp-gvfs-dbus-byte-array-to-string
				 (cadddr elt)))
	      (method (tramp-gvfs-dbus-byte-array-to-string
		       (cadr (assoc "type" (cadr mount-spec)))))
	      (user (tramp-gvfs-dbus-byte-array-to-string
		     (cadr (assoc "user" (cadr mount-spec)))))
	      (domain (tramp-gvfs-dbus-byte-array-to-string
		       (cadr (assoc "domain" (cadr mount-spec)))))
	      (host (tramp-gvfs-dbus-byte-array-to-string
		     (cadr (or (assoc "host" (cadr mount-spec))
			       (assoc "server" (cadr mount-spec))))))
	      (port (tramp-gvfs-dbus-byte-array-to-string
		     (cadr (assoc "port" (cadr mount-spec)))))
	      (ssl (tramp-gvfs-dbus-byte-array-to-string
		    (cadr (assoc "ssl" (cadr mount-spec)))))
	      (uri (tramp-gvfs-dbus-byte-array-to-string
		    (cadr (assoc "uri" (cadr mount-spec)))))
	      (share (tramp-gvfs-dbus-byte-array-to-string
		      (or
		       (cadr (assoc "share" (cadr mount-spec)))
		       (cadr (assoc "volume" (cadr mount-spec)))))))
	 (when (string-match (rx bol (group (| "afp" "smb"))) method)
	   (setq method (match-string 1 method)))
	 (when (and (string-equal "dav" method) (string-equal "true" ssl))
	   (setq method "davs"))
	 (when (and (string-equal "davs" method)
		    (string-match-p
		     tramp-gvfs-nextcloud-default-prefix-regexp prefix))
	   (setq method "nextcloud"))
	 (when (string-equal "google-drive" method)
	   (setq method "gdrive"))
	 (when (and (string-equal "http" method) (stringp uri))
	   (setq host (tramp-gvfs-url-host uri)
		 uri (url-generic-parse-url uri)
		 method (url-type uri)
		 user (url-user uri)
		 port (url-portspec uri)))
	 (when (member method tramp-media-methods)
	   ;; Ensure that media devices are cached.
	   (tramp-get-media-devices vec)
	   (when-let* ((v (tramp-get-connection-property
			   (make-tramp-media-device
			    :method method :host host :port port)
			   "vector")))
	     (setq method (tramp-file-name-method v)
		   host (tramp-file-name-host v)
		   port (tramp-file-name-port v))))
	 (when (and
		(string-equal method (tramp-file-name-method vec))
		(string-equal user (tramp-file-name-user vec))
		(string-equal domain (tramp-file-name-domain vec))
		(string-equal host (tramp-file-name-host vec))
		(string-equal port (tramp-file-name-port vec))
		(string-match-p
		 (rx bol "/" (literal (or share "")))
		 (tramp-file-name-unquote-localname vec)))
	   ;; Set mountpoint and location.
	   (tramp-set-file-property vec "/" "fuse-mountpoint" fuse-mountpoint)
	   (tramp-set-connection-property
	    vec "default-location" default-location)
	   (when share
	     (tramp-set-connection-property
	      (tramp-get-process vec) "share" (concat "/" share)))
	   (throw 'mounted t)))))))

(defun tramp-gvfs-unmount (vec)
  "Unmount the object identified by VEC."
  (setf (tramp-file-name-localname vec) "/"
	(tramp-file-name-hop vec) nil)
  (when (tramp-gvfs-connection-mounted-p vec)
    (tramp-gvfs-send-command
     vec "gvfs-mount" "-u"
     (tramp-gvfs-url-file-name (tramp-make-tramp-file-name vec))))
  (while (tramp-gvfs-connection-mounted-p vec)
    (read-event nil nil 0.1))
  (tramp-cleanup-connection vec 'keep-debug 'keep-password))

(defun tramp-gvfs-mount-spec-entry (key value)
  "Construct a mount-spec entry to be used in a mount_spec.
It was \"a(say)\", but has changed to \"a{sv})\"."
  (if (string-match-p (rx bol "(aya{sv})") tramp-gvfs-mountlocation-signature)
      (list :dict-entry key
	    (list :variant (tramp-gvfs-dbus-string-to-byte-array value)))
    (list :struct key (tramp-gvfs-dbus-string-to-byte-array value))))

(defun tramp-gvfs-mount-spec (vec)
  "Return a mount-spec for \"org.gtk.vfs.MountTracker.mountLocation\"."
  (let* ((media (tramp-get-media-device vec))
	 (method (if media
		     (tramp-media-device-method media)
		   (tramp-file-name-method vec)))
	 (user (tramp-file-name-user vec))
	 (domain (tramp-file-name-domain vec))
	 (host (if media
		   (tramp-media-device-host media) (tramp-file-name-host vec)))
	 (port (if media
		   (tramp-media-device-port media) (tramp-file-name-port vec)))
	 (localname (tramp-file-name-unquote-localname vec))
	 (share (when (string-match
		       (rx bol (? "/") (group (+ (not "/")))) localname)
		  (match-string 1 localname)))
	 (ssl (if (string-match-p (rx bol (| "davs" "nextcloud")) method)
		  "true" "false"))
	 (mount-spec
          `(:array
            ,@(cond
               ((string-equal "smb" method)
                (list (tramp-gvfs-mount-spec-entry "type" "smb-share")
                      (tramp-gvfs-mount-spec-entry "server" host)
                      (tramp-gvfs-mount-spec-entry "share" share)))
               ((string-match-p (rx bol (| "davs" "nextcloud")) method)
                (list (tramp-gvfs-mount-spec-entry "type" "dav")
                      (tramp-gvfs-mount-spec-entry "host" host)
                      (tramp-gvfs-mount-spec-entry "ssl" ssl)))
               ((string-equal "afp" method)
                (list (tramp-gvfs-mount-spec-entry "type" "afp-volume")
                      (tramp-gvfs-mount-spec-entry "host" host)
                      (tramp-gvfs-mount-spec-entry "volume" share)))
               ((string-equal "gdrive" method)
                (list (tramp-gvfs-mount-spec-entry "type" "google-drive")
                      (tramp-gvfs-mount-spec-entry "host" host)))
               ((string-equal "nextcloud" method)
                (list (tramp-gvfs-mount-spec-entry "type" "owncloud")
                      (tramp-gvfs-mount-spec-entry "host" host)))
               ((string-match-p (rx bol "http") method)
                (list (tramp-gvfs-mount-spec-entry "type" "http")
                      (tramp-gvfs-mount-spec-entry
		       "uri"
		       (url-recreate-url
			(url-parse-make-urlobj
			 method user nil host port "/" nil nil t)))))
	       (t
                (list (tramp-gvfs-mount-spec-entry "type" method)
                      (tramp-gvfs-mount-spec-entry "host" host))))
            ,@(when user
                (list (tramp-gvfs-mount-spec-entry "user" user)))
            ,@(when domain
                (list (tramp-gvfs-mount-spec-entry "domain" domain)))
            ,@(when port
                (list (tramp-gvfs-mount-spec-entry "port" port)))))
	 (mount-pref
          (if (and (string-match-p (rx bol "dav") method)
                   (string-match (rx bol (? "/") (+ (not "/"))) localname))
              (match-string 0 localname)
	    (tramp-gvfs-get-remote-prefix vec))))

    ;; Return.
    `(:struct ,(tramp-gvfs-dbus-string-to-byte-array mount-pref) ,mount-spec)))

(defun tramp-gvfs-handler-volumeadded-volumeremoved (_dbus-name _id volume)
  "Signal handler for the gvfs \"VolumeAdded\" and \"VolumeRemoved\" signals.
Their full names are
\"org.gtk.Private.RemoteVolumeMonitor.VolumeAdded\" and
\"org.gtk.Private.RemoteVolumeMonitor.VolumeRemoved\"."
  (ignore-errors
    (let* ((signal-name (dbus-event-member-name last-input-event))
	   (uri (url-generic-parse-url (nth 5 volume)))
	   (method (url-type uri))
	   (vec (make-tramp-file-name
		 :method "mtp"
		 ;; A host name cannot contain spaces.
		 :host (string-replace " " "_" (nth 1 volume))))
	   (media (make-tramp-media-device
		   :method method
		   :host (tramp-gvfs-url-host (nth 5 volume))
		   :port (and (url-portspec uri)))))
      (when (member method tramp-media-methods)
	(tramp-message
	 vec 6 "%s %s" signal-name (tramp-gvfs-stringify-dbus-message volume))
	(tramp-flush-connection-properties vec)
	(tramp-flush-connection-properties media)
	(tramp-get-media-devices nil)))))

(when tramp-gvfs-enabled
  (dbus-register-signal
   :session nil tramp-gvfs-path-remotevolumemonitor
   tramp-gvfs-interface-remotevolumemonitor "VolumeAdded"
   #'tramp-gvfs-handler-volumeadded-volumeremoved)
  (dbus-register-signal
   :session nil tramp-gvfs-path-remotevolumemonitor
   tramp-gvfs-interface-remotevolumemonitor "VolumeRemoved"
   #'tramp-gvfs-handler-volumeadded-volumeremoved))


;; Connection functions.

(defun tramp-gvfs-get-remote-prefix (vec)
  "The prefix of the remote connection VEC.
This is relevant for GNOME Online Accounts."
  (with-tramp-connection-property vec "prefix"
    ;; Ensure that GNOME Online Accounts are cached.
    (when (member (tramp-file-name-method vec) tramp-goa-methods)
      (tramp-get-goa-accounts vec))
    (tramp-get-connection-property (tramp-get-goa-account vec) "prefix" "/")))

(defun tramp-gvfs-maybe-open-connection (vec)
  "Maybe open a connection VEC.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  ;; During completion, don't reopen a new connection.
  (unless (tramp-connectable-p vec)
    (throw 'non-essential 'non-essential))

  (with-tramp-debug-message vec "Opening connection"
    ;; Sanity check.
    (let ((method (tramp-file-name-method vec)))
      (unless (member
	       (or (assoc-default
		    method '(("smb" . "smb-share")
			     ("davs" . "dav")
			     ("nextcloud" . "dav")
			     ("afp" . "afp-volume")
			     ("gdrive" . "google-drive")))
		   method)
	       tramp-gvfs-mounttypes)
	(tramp-error
	 vec 'file-error "Method `%s' not supported by GVFS" method)))

    ;; For password handling, we need a process bound to the
    ;; connection buffer.  Therefore, we create a dummy process.
    ;; Maybe there is a better solution?
    (unless (get-buffer-process (tramp-get-connection-buffer vec))
      (let ((p (make-network-process
		:name (tramp-get-connection-name vec)
		:buffer (tramp-get-connection-buffer vec)
		:server t :host 'local :service t :noquery t)))
	(tramp-post-process-creation p vec)

	;; Set connection-local variables.
	(tramp-set-connection-local-variables vec)))

    (unless (tramp-gvfs-connection-mounted-p vec)
      (let ((method (tramp-file-name-method vec))
	    (user-domain (tramp-file-name-user-domain vec))
	    (host-port (tramp-file-name-host-port vec))
	    (localname (tramp-file-name-unquote-localname vec))
	    (object-path
	     (tramp-gvfs-object-path (tramp-make-tramp-file-name vec 'noloc))))

	(when (and (string-equal method "afp")
		   (string-equal localname "/"))
	  (tramp-user-error vec "Filename must contain an AFP volume"))

	(when (and (string-match-p (rx "dav" (? "s")) method)
		   (string-equal localname "/"))
	  (tramp-user-error vec "Filename must contain a WebDAV share"))

	(when (and (string-equal method "smb")
		   (string-equal localname "/"))
	  (tramp-user-error vec "Filename must contain a Windows share"))

	(when (member method tramp-goa-methods)
	  ;; Ensure that GNOME Online Accounts are cached.
	  (tramp-get-goa-accounts vec)
	  (when (tramp-get-connection-property
		 (tramp-get-goa-account vec) "FilesDisabled" t)
	    (tramp-user-error
	     vec "There is no Online Account `%s'"
	     (tramp-make-tramp-file-name vec 'noloc))))

	(with-tramp-progress-reporter
	    vec 3 (format "Opening connection for %s%s using %s"
			  (if (tramp-string-empty-or-nil-p user-domain)
			      "" (concat user-domain "@"))
			  host-port method)

	  ;; Enable `auth-source'.
	  (tramp-set-connection-property
	   vec " first-password-request" tramp-cache-read-persistent-data)

	  ;; There will be a callback of "askPassword" when a password is needed.
	  (dbus-register-method
	   :session dbus-service-emacs object-path
	   tramp-gvfs-interface-mountoperation "askPassword"
	   #'tramp-gvfs-handler-askpassword)
	  (dbus-register-method
	   :session dbus-service-emacs object-path
	   tramp-gvfs-interface-mountoperation "AskPassword"
	   #'tramp-gvfs-handler-askpassword)

	  ;; There could be a callback of "askQuestion" when adding
	  ;; fingerprints or checking certificates.
	  (dbus-register-method
	   :session dbus-service-emacs object-path
	   tramp-gvfs-interface-mountoperation "askQuestion"
	   #'tramp-gvfs-handler-askquestion)
	  (dbus-register-method
	   :session dbus-service-emacs object-path
	   tramp-gvfs-interface-mountoperation "AskQuestion"
	   #'tramp-gvfs-handler-askquestion)

	  ;; The call must be asynchronously, because of the
	  ;; "askPassword" or "askQuestion" callbacks.
	  (if (string-match-p (rx "(so)" eol) tramp-gvfs-mountlocation-signature)
	      (with-tramp-dbus-call-method vec nil
		:session tramp-gvfs-service-daemon tramp-gvfs-path-mounttracker
		tramp-gvfs-interface-mounttracker tramp-gvfs-mountlocation
		(tramp-gvfs-mount-spec vec)
		`(:struct :string ,(dbus-get-unique-name :session)
			  :object-path ,object-path))
	    (with-tramp-dbus-call-method vec nil
	      :session tramp-gvfs-service-daemon tramp-gvfs-path-mounttracker
	      tramp-gvfs-interface-mounttracker tramp-gvfs-mountlocation
	      (tramp-gvfs-mount-spec vec)
	      :string (dbus-get-unique-name :session) :object-path object-path))

	  ;; We must wait, until the mount is applied.  This will be
	  ;; indicated by the "mounted" signal, i.e. the
	  ;; "fuse-mountpoint" file property.
	  (with-timeout
	      ((tramp-get-method-parameter
		vec 'tramp-connection-timeout tramp-connection-timeout)
	       (if (tramp-string-empty-or-nil-p user-domain)
		   (tramp-error
		    vec 'file-error
		    "Timeout reached mounting %s using %s" host-port method)
		 (tramp-error
		  vec 'file-error
		  "Timeout reached mounting %s@%s using %s"
		  user-domain host-port method)))
	    (while (not (tramp-get-file-property vec "/" "fuse-mountpoint"))
	      (read-event nil nil 0.1)))

	  ;; If `tramp-gvfs-handler-askquestion' has returned "No", it
	  ;; is marked with the fuse-mountpoint "/".  We shall react.
	  (when (string-equal
		 (tramp-get-file-property vec "/" "fuse-mountpoint" "") "/")
	    (tramp-error vec 'file-error "FUSE mount denied"))

	  ;; Save the password.
	  (ignore-errors
	    (and (functionp tramp-password-save-function)
		 (funcall tramp-password-save-function)))

	  ;; Mark it as connected.
	  (tramp-set-connection-property
	   (tramp-get-connection-process vec) "connected" t))))))

(defun tramp-gvfs-gio-tool-p (vec)
  "Check, whether the gio tool is available."
  (with-tramp-connection-property vec "gio-tool"
    (zerop (tramp-call-process vec "gio" nil nil nil "version"))))

(defun tramp-gvfs-send-command (vec command &rest args)
  "Send the COMMAND with its ARGS to connection VEC.
COMMAND is a command from the gvfs-* utilities.  It is replaced
by the corresponding gio tool call if available.  `call-process'
is applied, and it returns t if the return code is zero."
  (let ((locale (tramp-get-local-locale vec)))
    (when (tramp-gvfs-gio-tool-p vec)
      ;; Use gio tool.
      (setq args (cons (cdr (assoc command tramp-gvfs-gio-mapping))
		       (delq nil args))
	    command "gio"))

    (with-current-buffer (tramp-get-connection-buffer vec)
      (tramp-gvfs-maybe-open-connection vec)
      (erase-buffer)
      (or (zerop
	   (apply
	    #'tramp-call-process vec "env" nil t nil
	    (append `(,(format "LANG=%s" locale)
		      ,(format "LANGUAGE=%s" locale)
		      ,(format "LC_ALL=%s" locale)
		      ,command)
		    args)))
	  ;; Remove information about mounted connection.
	  (and (tramp-flush-file-properties vec "/") nil)))))


;; GNOME Online Accounts functions.

(defun tramp-get-goa-account (vec)
  "Transform VEC into a `tramp-goa-account' structure."
  (when (tramp-file-name-p vec)
    (make-tramp-goa-account
     :method (tramp-file-name-method vec)
     :user (tramp-file-name-user vec)
     :host (tramp-file-name-host vec)
     :port (tramp-file-name-port vec))))

(defun tramp-get-goa-accounts (vec)
  "Retrieve GNOME Online Accounts, and cache them.
The hash key is a `tramp-goa-account' structure.  The value is an
alist of the properties of `tramp-goa-interface-account' and
`tramp-goa-interface-files' of the corresponding GNOME Online
Account.  Additionally, a property \"prefix\" is added.
VEC is used only for traces."
  (with-tramp-connection-property nil "goa-accounts"
    (dolist
	(object-path
	 (mapcar
	  #'car
	  (tramp-dbus-function
	   vec #'dbus-get-all-managed-objects
	   `(:session ,tramp-goa-service ,tramp-goa-path))))
      (let* ((account-properties
	      (with-tramp-dbus-get-all-properties vec
		:session tramp-goa-service object-path
		tramp-goa-interface-account))
	     (files-properties
	      (with-tramp-dbus-get-all-properties vec
		:session tramp-goa-service object-path
		tramp-goa-interface-files))
	     (identity
	      (or (cdr (assoc "PresentationIdentity" account-properties)) ""))
	     key)
	;; Only accounts which matter.
	(when (and
	       (not (cdr (assoc "FilesDisabled" account-properties)))
	       (member
		(cdr (assoc "ProviderType" account-properties))
		'("google" "owncloud"))
	       (string-match tramp-goa-identity-regexp identity))
	  (setq key (make-tramp-goa-account
		     :method (cdr (assoc "ProviderType" account-properties))
		     :user (match-string 1 identity)
		     :host (match-string 2 identity)
		     :port (match-string 3 identity)))
	  (when (string-equal (tramp-goa-account-method key) "google")
	    (setf (tramp-goa-account-method key) "gdrive"))
	  (when (string-equal (tramp-goa-account-method key) "owncloud")
	    (setf (tramp-goa-account-method key) "nextcloud"))
	  ;; Cache all properties.
	  (dolist (prop (nconc account-properties files-properties))
	    (tramp-set-connection-property key (car prop) (cdr prop)))
	  ;; Cache "prefix".
	  (tramp-message
	   vec 10 "%s prefix %s" key
	   (tramp-set-connection-property
	    key "prefix"
	    (directory-file-name
	     (url-filename
	      (url-generic-parse-url
	       (tramp-get-connection-property key "Uri" "file:///")))))))))
    ;; Mark, that goa accounts have been cached.
    "cached"))

(defun tramp-parse-goa-accounts (service)
  "Return a list of (user host) tuples allowed to access.
It checks for registered GNOME Online Accounts."
  ;; SERVICE might be encoded as a DNS-SD service.
  (and (string-match tramp-dns-sd-service-regexp service)
       (setq service (match-string 1 service)))
  (mapcar
   (lambda (key)
     (and (tramp-goa-account-p key)
	  (string-equal service (tramp-goa-account-method key))
	  (list (tramp-goa-account-user key)
		(tramp-goa-account-host key))))
   (hash-table-keys tramp-cache-data)))


;; Media devices functions.

(defun tramp-get-media-device (vec)
  "Transform VEC into a `tramp-media-device' structure.
Check, that respective cache values do exist."
  (if-let* ((media (tramp-get-connection-property vec "media-device"))
	    (prop (tramp-get-connection-property media "vector")))
      media
    (tramp-get-media-devices vec)
    (tramp-get-connection-property vec "media-device")))

(defun tramp-get-media-devices (vec)
  "Retrieve media devices, and cache them.
The hash key is a `tramp-media-device' structure.
VEC is used only for traces."
  (let (devices)
    (dolist (method tramp-media-methods)
      (dolist (volume (cadr (with-tramp-dbus-call-method vec t
			      :session (tramp-gvfs-service-volumemonitor method)
			      tramp-gvfs-path-remotevolumemonitor
			      tramp-gvfs-interface-remotevolumemonitor "List")))
	(let* ((uri (url-generic-parse-url (nth 5 volume)))
	       (vec (make-tramp-file-name
		     :method "mtp"
		     ;; A host name cannot contain spaces.
		     :host (string-replace " " "_" (nth 1 volume))))
	       (media (make-tramp-media-device
		       :method method
		       :host (tramp-gvfs-url-host (nth 5 volume))
		       :port (and (url-portspec uri)
				  (number-to-string (url-portspec uri))))))
	  (push (tramp-file-name-host vec) devices)
	  (tramp-set-connection-property vec "activation-uri" (nth 5 volume))
	  (tramp-set-connection-property vec "media-device" media)
	  (tramp-set-connection-property media "vector" vec))))

    ;; Adapt default host name, supporting /mtp:: when possible.
    (setq tramp-default-host-alist
	  (append
	   `(("mtp" nil ,(if (length= devices 1) (car devices) "")))
	   (delete
	    (assoc "mtp" tramp-default-host-alist)
	    tramp-default-host-alist)))))

(defun tramp-parse-media-names (service)
  "Return a list of (user host) tuples allowed to access.
It checks for mounted media devices."
  ;; SERVICE might be encoded as a DNS-SD service.
  (and (string-match tramp-dns-sd-service-regexp service)
       (setq service (match-string 1 service)))
  (mapcar
   (lambda (key)
     (and (tramp-media-device-p key)
	  (string-equal service (tramp-media-device-method key))
	  (tramp-get-connection-property key "vector")
	  (list nil (tramp-file-name-host
		     (tramp-get-connection-property key "vector")))))
   (hash-table-keys tramp-cache-data)))


;; D-Bus zeroconf functions.

(defun tramp-zeroconf-parse-device-names (service)
  "Return a list of (user host) tuples allowed to access."
  (mapcar
   (lambda (x)
     (let ((host (zeroconf-service-host x))
	   (port (zeroconf-service-port x))
	   (text (zeroconf-service-txt x))
	   user)
       (when port
	 (setq host (format "%s%s%d" host tramp-prefix-port-format port)))
       ;; A user is marked in a TXT field like "u=guest".
       (while text
	 (when (string-match "u=\\(.+\\)$" (car text))
	   (setq user (match-string 1 (car text))))
	 (setq text (cdr text)))
       (list user host)))
   (zeroconf-list-services service)))

(defun tramp-gvfs-parse-device-names (service)
  "Return a list of (user host) tuples allowed to access.
This uses \"avahi-browse\" in case D-Bus is not enabled in Avahi."
  (let ((result
	 (ignore-errors
	   (split-string
	    (shell-command-to-string (format "avahi-browse -trkp %s" service))
	    (rx (+ (any "\r\n"))) 'omit (rx bol "+;" (* nonl) eol)))))
    (delete-dups
     (mapcar
      (lambda (x)
	(ignore-errors
	  (let* ((list (split-string x ";"))
		 (host (nth 6 list))
		 (text (split-string (nth 9 list) "\" \"" 'omit "\""))
		 user)
	    ;; A user is marked in a TXT field like "u=guest".
	    (while text
	      (when (string-match (rx "u=" (group (+ nonl)) eol) (car text))
		(setq user (match-string 1 (car text))))
	      (setq text (cdr text)))
	    (list user host))))
      result))))

(when tramp-gvfs-enabled
  ;; Suppress D-Bus error messages and Tramp traces.
  (let ((tramp-verbose 0)
	tramp-gvfs-dbus-event-vector fun)
    (when (and (autoload 'zeroconf-init "zeroconf")
               (ignore-error dbus-error
	         (tramp-compat-funcall 'dbus-get-unique-name :system)))
      ;; Add completion functions for services announced by DNS-SD.
      ;; See <http://www.dns-sd.org/ServiceTypes.html> for valid service types.
      (zeroconf-init tramp-gvfs-zeroconf-domain)
      (when (setq fun (or (and (zeroconf-list-service-types)
			       #'tramp-zeroconf-parse-device-names)
			  (and (executable-find "avahi-browse")
			       #'tramp-gvfs-parse-device-names)))
	(when (member "afp" tramp-gvfs-methods)
	  (tramp-set-completion-function
	   "afp" `((,fun "_afpovertcp._tcp"))))
	(when (member "dav" tramp-gvfs-methods)
	  (tramp-set-completion-function
	   "dav" `((,fun "_webdav._tcp")
		   (,fun "_webdavs._tcp"))))
	(when (member "davs" tramp-gvfs-methods)
	  (tramp-set-completion-function
	   "davs" `((,fun "_webdav._tcp")
		    (,fun "_webdavs._tcp"))))
	(when (member "ftp" tramp-gvfs-methods)
	  (tramp-set-completion-function
	   "ftp" `((,fun "_ftp._tcp"))))
	(when (member "http" tramp-gvfs-methods)
	  (tramp-set-completion-function
	   "http" `((,fun "_http._tcp")
		    (,fun "_https._tcp"))))
	(when (member "https" tramp-gvfs-methods)
	  (tramp-set-completion-function
	   "https" `((,fun "_http._tcp")
		     (,fun "_https._tcp"))))
	(when (member "sftp" tramp-gvfs-methods)
	  (tramp-set-completion-function
	   "sftp" `((,fun "_sftp-ssh._tcp")
		    (,fun "_ssh._tcp")
		    (,fun "_workstation._tcp"))))
	(when (member "smb" tramp-gvfs-methods)
	  (tramp-set-completion-function
	   "smb" `((,fun "_smb._tcp"))))))

    ;; Add completion functions for GNOME Online Accounts.
    (tramp-get-goa-accounts nil)
    (dolist (method tramp-goa-methods)
      (when (member method tramp-gvfs-methods)
	(tramp-set-completion-function
	 method `((tramp-parse-goa-accounts ,(format "_%s._tcp" method))))))

    ;; Add completion functions for media devices.
    (tramp-get-media-devices nil)
    (tramp-set-completion-function
     "mtp"
     (mapcar
      (lambda (method) `(tramp-parse-media-names ,(format "_%s._tcp" method)))
      tramp-media-methods))))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-gvfs 'force)))

(provide 'tramp-gvfs)

;;; TODO:

;; * (Customizable) unmount when exiting Emacs.  See tramp-archive.el.
;;
;; * Host name completion for existing mount points (afp-server,
;;   smb-server) or via smb-network or network.
;;
;; * What's up with the other types in `tramp-gvfs-mounttypes'?
;;
;; * Check, how two shares of the same SMB server can be mounted in
;;   parallel.

;;; tramp-gvfs.el ends here
