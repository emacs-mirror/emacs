;;; tramp-gvfs.el --- Tramp access functions for GVFS daemon  -*- lexical-binding:t -*-

;; Copyright (C) 2009-2018 Free Software Foundation, Inc.

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

;; The custom option `tramp-gvfs-methods' contains the list of
;; supported connection methods.  Per default, these are "afp", "dav",
;; "davs", "gdrive", "obex", "owncloud", "sftp" and "synce".  Note
;; that with "obex" it might be necessary to pair with the other
;; bluetooth device, if it hasn't been done already.  There might be
;; also some few seconds delay in discovering available bluetooth
;; devices.

;; "gdrive" and "owncloud" connection methods require a respective
;; account in GNOME Online Accounts, with enabled "Files" service.

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
;;   'car
;;   (dbus-call-method
;;    :session tramp-gvfs-service-daemon tramp-gvfs-path-mounttracker
;;    tramp-gvfs-interface-mounttracker "ListMountableInfo")))

;; Note that all other connection methods are not tested, beside the
;; ones offered for customization in `tramp-gvfs-methods'.  If you
;; request an additional connection method to be supported, please
;; drop me a note.

;; For hostname completion, information is retrieved either from the
;; bluez daemon (for the "obex" method), the hal daemon (for the
;; "synce" method), or from the zeroconf daemon (for the "afp", "dav",
;; "davs", and "sftp" methods).  The zeroconf daemon is pre-configured
;; to discover services in the "local" domain.  If another domain
;; shall be used for discovering services, the custom option
;; `tramp-gvfs-zeroconf-domain' can be set accordingly.

;; Restrictions:

;; * The current GVFS implementation does not allow writing on the
;;   remote bluetooth device via OBEX.
;;
;; * Two shares of the same SMB server cannot be mounted in parallel.

;;; Code:

;; D-Bus support in the Emacs core can be disabled with configuration
;; option "--without-dbus".  Declare used subroutines and variables.
(declare-function dbus-get-unique-name "dbusbind.c")

(require 'tramp)

(require 'dbus)
(require 'url-parse)
(require 'url-util)
(require 'zeroconf)

;; Pacify byte-compiler.
(eval-when-compile
  (require 'custom))

;; We don't call `dbus-ping', because this would load dbus.el.
(defconst tramp-gvfs-enabled
  (ignore-errors
    (and (featurep 'dbusbind)
	 (tramp-compat-funcall 'dbus-get-unique-name :system)
	 (tramp-compat-funcall 'dbus-get-unique-name :session)
	 (or (tramp-compat-process-running-p "gvfs-fuse-daemon")
	     (tramp-compat-process-running-p "gvfsd-fuse"))))
  "Non-nil when GVFS is available.")

;;;###tramp-autoload
(defcustom tramp-gvfs-methods
  '("afp" "dav" "davs" "gdrive" "obex" "owncloud" "sftp" "synce")
  "List of methods for remote files, accessed with GVFS."
  :group 'tramp
  :version "26.1"
  :type '(repeat (choice (const "afp")
			 (const "dav")
			 (const "davs")
			 (const "ftp")
			 (const "gdrive")
			 (const "http")
			 (const "https")
			 (const "obex")
			 (const "owncloud")
			 (const "sftp")
			 (const "smb")
			 (const "synce")))
  :require 'tramp)

(defconst tramp-goa-methods '("gdrive" "owncloud")
  "List of methods which require registration at GNOME Online Accounts.")

;; Remove GNOME Online Accounts methods if not supported.
(unless (and tramp-gvfs-enabled
	     (member tramp-goa-service (dbus-list-known-names :session)))
  (dolist (method tramp-goa-methods)
    (setq tramp-gvfs-methods (delete method tramp-gvfs-methods))))

;; Add defaults for `tramp-default-user-alist' and `tramp-default-host-alist'.
;;;###tramp-autoload
(when (string-match "\\(.+\\)@\\(\\(?:gmail\\|googlemail\\)\\.com\\)"
		    user-mail-address)
  (add-to-list 'tramp-default-user-alist
	       `("\\`gdrive\\'" nil ,(match-string 1 user-mail-address)))
  (add-to-list 'tramp-default-host-alist
	       '("\\`gdrive\\'" nil ,(match-string 2 user-mail-address))))
;;;###tramp-autoload
(add-to-list 'tramp-default-user-alist '("\\`synce\\'" nil nil))

;;;###tramp-autoload
(defcustom tramp-gvfs-zeroconf-domain "local"
  "Zeroconf domain to be used for discovering services, like host names."
  :group 'tramp
  :version "23.2"
  :type 'string
  :require 'tramp)

;; Add the methods to `tramp-methods', in order to allow minibuffer
;; completion.
;;;###tramp-autoload
(when (featurep 'dbusbind)
  (dolist (elt tramp-gvfs-methods)
    (unless (assoc elt tramp-methods)
      (add-to-list 'tramp-methods (cons elt nil)))))

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
  (concat "^" "\\(" tramp-user-regexp "\\)?"
	  "@" "\\(" tramp-host-regexp "\\)?"
	  "\\(?:" ":""\\(" tramp-port-regexp "\\)" "\\)?")
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

;; The basic structure for GNOME Online Accounts.  We use a list :type,
;; in order to be compatible with Emacs 24 and 25.
(cl-defstruct (tramp-goa-name (:type list) :named) method user host port)

(defconst tramp-bluez-service "org.bluez"
  "The well known name of the BLUEZ service.")

(defconst tramp-bluez-interface-manager "org.bluez.Manager"
  "The manager interface of the BLUEZ daemon.")

;; <interface name='org.bluez.Manager'>
;;   <method name='DefaultAdapter'>
;;     <arg type='o' direction='out'/>
;;   </method>
;;   <method name='FindAdapter'>
;;     <arg type='s' direction='in'/>
;;     <arg type='o' direction='out'/>
;;   </method>
;;   <method name='ListAdapters'>
;;     <arg type='ao' direction='out'/>
;;   </method>
;;   <signal name='AdapterAdded'>
;;     <arg type='o'/>
;;   </signal>
;;   <signal name='AdapterRemoved'>
;;     <arg type='o'/>
;;   </signal>
;;   <signal name='DefaultAdapterChanged'>
;;     <arg type='o'/>
;;   </signal>
;; </interface>

(defconst tramp-bluez-interface-adapter "org.bluez.Adapter"
  "The adapter interface of the BLUEZ daemon.")

;; <interface name='org.bluez.Adapter'>
;;   <method name='GetProperties'>
;;     <arg type='a{sv}' direction='out'/>
;;   </method>
;;   <method name='SetProperty'>
;;     <arg type='s' direction='in'/>
;;     <arg type='v' direction='in'/>
;;   </method>
;;   <method name='RequestMode'>
;;     <arg type='s' direction='in'/>
;;   </method>
;;   <method name='ReleaseMode'/>
;;   <method name='RequestSession'/>
;;   <method name='ReleaseSession'/>
;;   <method name='StartDiscovery'/>
;;   <method name='StopDiscovery'/>
;;   <method name='ListDevices'>
;;     <arg type='ao' direction='out'/>
;;   </method>
;;   <method name='CreateDevice'>
;;     <arg type='s' direction='in'/>
;;     <arg type='o' direction='out'/>
;;   </method>
;;   <method name='CreatePairedDevice'>
;;     <arg type='s' direction='in'/>
;;     <arg type='o' direction='in'/>
;;     <arg type='s' direction='in'/>
;;     <arg type='o' direction='out'/>
;;   </method>
;;   <method name='CancelDeviceCreation'>
;;     <arg type='s' direction='in'/>
;;   </method>
;;   <method name='RemoveDevice'>
;;     <arg type='o' direction='in'/>
;;   </method>
;;   <method name='FindDevice'>
;;     <arg type='s' direction='in'/>
;;     <arg type='o' direction='out'/>
;;   </method>
;;   <method name='RegisterAgent'>
;;     <arg type='o' direction='in'/>
;;     <arg type='s' direction='in'/>
;;   </method>
;;   <method name='UnregisterAgent'>
;;     <arg type='o' direction='in'/>
;;   </method>
;;   <signal name='DeviceCreated'>
;;     <arg type='o'/>
;;   </signal>
;;   <signal name='DeviceRemoved'>
;;     <arg type='o'/>
;;   </signal>
;;   <signal name='DeviceFound'>
;;     <arg type='s'/>
;;     <arg type='a{sv}'/>
;;   </signal>
;;   <signal name='PropertyChanged'>
;;     <arg type='s'/>
;;     <arg type='v'/>
;;   </signal>
;;   <signal name='DeviceDisappeared'>
;;     <arg type='s'/>
;;   </signal>
;; </interface>

;;;###tramp-autoload
(defcustom tramp-bluez-discover-devices-timeout 60
  "Defines seconds since last bluetooth device discovery before rescanning.
A value of 0 would require an immediate discovery during hostname
completion, nil means to use always cached values for discovered
devices."
  :group 'tramp
  :version "23.2"
  :type '(choice (const nil) integer)
  :require 'tramp)

(defvar tramp-bluez-discovery nil
  "Indicator for a running bluetooth device discovery.
It keeps the timestamp of last discovery.")

(defvar tramp-bluez-devices nil
  "Alist of detected bluetooth devices.
Every entry is a list (NAME ADDRESS).")

(defconst tramp-hal-service "org.freedesktop.Hal"
  "The well known name of the HAL service.")

(defconst tramp-hal-path-manager "/org/freedesktop/Hal/Manager"
  "The object path of the HAL daemon manager.")

(defconst tramp-hal-interface-manager "org.freedesktop.Hal.Manager"
  "The manager interface of the HAL daemon.")

(defconst tramp-hal-interface-device "org.freedesktop.Hal.Device"
  "The device interface of the HAL daemon.")

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
    ("gvfs-trash" . "trash"))
  "List of cons cells, mapping \"gvfs-<command>\" to \"gio <command>\".")

;; <http://www.pygtk.org/docs/pygobject/gio-constants.html>
(defconst tramp-gvfs-file-attributes
  '("name"
    "type"
    "standard::display-name"
    "standard::symlink-target"
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
  "GVFS file attributes.")

(defconst tramp-gvfs-file-attributes-with-gvfs-ls-regexp
  (concat "[[:blank:]]" (regexp-opt tramp-gvfs-file-attributes t) "=\\(.+?\\)")
  "Regexp to parse GVFS file attributes with `gvfs-ls'.")

(defconst tramp-gvfs-file-attributes-with-gvfs-info-regexp
  (concat "^[[:blank:]]*"
	  (regexp-opt tramp-gvfs-file-attributes t)
	  ":[[:blank:]]+\\(.*\\)$")
  "Regexp to parse GVFS file attributes with `gvfs-info'.")

(defconst tramp-gvfs-file-system-attributes
  '("filesystem::free"
    "filesystem::size"
    "filesystem::used")
  "GVFS file system attributes.")

(defconst tramp-gvfs-file-system-attributes-regexp
  (concat "^[[:blank:]]*"
	  (regexp-opt tramp-gvfs-file-system-attributes t)
	  ":[[:blank:]]+\\(.*\\)$")
  "Regexp to parse GVFS file system attributes with `gvfs-info'.")

(defconst tramp-gvfs-owncloud-default-prefix "/remote.php/webdav"
  "Default prefix for owncloud / nextcloud methods.")

(defconst tramp-gvfs-owncloud-default-prefix-regexp
  (concat (regexp-quote tramp-gvfs-owncloud-default-prefix) "$")
  "Regexp of default prefix for owncloud / nextcloud methods.")


;; New handlers should be added here.
;;;###tramp-autoload
(defconst tramp-gvfs-file-name-handler-alist
  '((access-file . ignore)
    (add-name-to-file . tramp-handle-add-name-to-file)
    ;; `byte-compiler-base-file-name' performed by default handler.
    ;; `copy-directory' performed by default handler.
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
    (expand-file-name . tramp-gvfs-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . ignore)
    (file-attributes . tramp-gvfs-handle-file-attributes)
    (file-directory-p . tramp-handle-file-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . tramp-gvfs-handle-file-executable-p)
    (file-exists-p . tramp-handle-file-exists-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-gvfs-handle-file-local-copy)
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
    (file-readable-p . tramp-gvfs-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . tramp-gvfs-handle-file-system-info)
    (file-truename . tramp-handle-file-truename)
    (file-writable-p . tramp-gvfs-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `find-file-noselect' performed by default handler.
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-handle-insert-directory)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (load . tramp-handle-load)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . tramp-gvfs-handle-make-directory)
    (make-directory-internal . ignore)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-symbolic-link . tramp-handle-make-symbolic-link)
    (process-file . ignore)
    (rename-file . tramp-gvfs-handle-rename-file)
    (set-file-acl . ignore)
    (set-file-modes . ignore)
    (set-file-selinux-context . ignore)
    (set-file-times . ignore)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (shell-command . ignore)
    (start-file-process . ignore)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (unhandled-file-name-directory . ignore)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . tramp-gvfs-handle-write-region))
  "Alist of handler functions for Tramp GVFS method.
Operations not mentioned here will be handled by the default Emacs primitives.")

;; It must be a `defsubst' in order to push the whole code into
;; tramp-loaddefs.el.  Otherwise, there would be recursive autoloading.
;;;###tramp-autoload
(defsubst tramp-gvfs-file-name-p (filename)
  "Check if it's a filename handled by the GVFS daemon."
  (and (tramp-tramp-file-p filename)
       (let ((method
	      (tramp-file-name-method (tramp-dissect-file-name filename))))
	 (and (stringp method) (member method tramp-gvfs-methods)))))

;;;###tramp-autoload
(defun tramp-gvfs-file-name-handler (operation &rest args)
  "Invoke the GVFS related OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (unless tramp-gvfs-enabled
    (tramp-compat-user-error nil "Package `tramp-gvfs' not supported"))
  (let ((fn (assoc operation tramp-gvfs-file-name-handler-alist)))
    (if fn
	(save-match-data (apply (cdr fn) args))
      (tramp-run-real-handler operation args))))

;;;###tramp-autoload
(when (featurep 'dbusbind)
  (tramp-register-foreign-file-name-handler
   'tramp-gvfs-file-name-p 'tramp-gvfs-file-name-handler))


;; D-Bus helper function.

(defun tramp-gvfs-dbus-string-to-byte-array (string)
  "Like `dbus-string-to-byte-array' but add trailing \\0 if needed."
  (dbus-string-to-byte-array
   (if (string-match "^(aya{sv})" tramp-gvfs-mountlocation-signature)
       (concat string (string 0)) string)))

(defun tramp-gvfs-dbus-byte-array-to-string (byte-array)
  "Like `dbus-byte-array-to-string' but remove trailing \\0 if exists.
Return nil for null BYTE-ARRAY."
  ;; The byte array could be a variant.  Take care.
  (let ((byte-array
	 (if (and (consp byte-array) (atom (car byte-array)))
	     byte-array (car byte-array))))
    (and byte-array
	 (dbus-byte-array-to-string
	  (if (and (consp byte-array) (zerop (car (last byte-array))))
	      (butlast byte-array) byte-array)))))

(defun tramp-gvfs-stringify-dbus-message (message)
  "Convert a D-Bus message into readable UTF8 strings, used for traces."
  (cond
   ((and (consp message) (characterp (car message)))
    (format "%S" (tramp-gvfs-dbus-byte-array-to-string message)))
   ((and (consp message) (not (consp (cdr message))))
    (cons (tramp-gvfs-stringify-dbus-message (car message))
	  (tramp-gvfs-stringify-dbus-message (cdr message))))
   ((consp message)
    (mapcar 'tramp-gvfs-stringify-dbus-message message))
   ((stringp message)
    (format "%S" message))
   (t message)))

(defun tramp-dbus-function (vec func args)
  "Apply a D-Bus function FUNC from dbus.el.
The call will be traced by Tramp with trace level 6."
  (let (result)
    (tramp-message vec 6 "%s" (cons func args))
    (setq result (apply func args))
    (tramp-message vec 6 "%s" result(tramp-gvfs-stringify-dbus-message result))
    result))

(defmacro with-tramp-dbus-call-method
  (vec synchronous bus service path interface method &rest args)
  "Apply a D-Bus call on bus BUS.

If SYNCHRONOUS is non-nil, the call is synchronously.  Otherwise,
it is an asynchronous call, with `ignore' as callback function.

The other arguments have the same meaning as with `dbus-call-method'
or `dbus-call-method-asynchronously'."
  `(let ((func (if ,synchronous
		   'dbus-call-method 'dbus-call-method-asynchronously))
	 (args (append (list ,bus ,service ,path ,interface ,method)
		       (if ,synchronous (list ,@args) (list 'ignore ,@args)))))
     (tramp-dbus-function ,vec func args)))

(put 'with-tramp-dbus-call-method 'lisp-indent-function 2)
(put 'with-tramp-dbus-call-method 'edebug-form-spec '(form symbolp body))
(font-lock-add-keywords 'emacs-lisp-mode '("\\<with-tramp-dbus-call-method\\>"))

(defmacro with-tramp-dbus-get-all-properties
  (vec bus service path interface)
  "Return all properties of INTERFACE.
The call will be traced by Tramp with trace level 6."
     ;; Check, that interface exists at object path.  Retrieve properties.
  `(when (member
	  ,interface
	  (tramp-dbus-function
	   ,vec 'dbus-introspect-get-interface-names
	   (list ,bus ,service ,path)))
     (tramp-dbus-function
      ,vec 'dbus-get-all-properties (list ,bus ,service ,path ,interface))))

(put 'with-tramp-dbus-get-all-properties 'lisp-indent-function 1)
(put 'with-tramp-dbus-get-all-properties 'edebug-form-spec '(form symbolp body))
(font-lock-add-keywords 'emacs-lisp-mode '("\\<with-tramp-dbus-get-all-properties\\>"))

(defvar tramp-gvfs-dbus-event-vector nil
  "Current Tramp file name to be used, as vector.
It is needed when D-Bus signals or errors arrive, because there
is no information where to trace the message.")

(defun tramp-gvfs-dbus-event-error (event err)
  "Called when a D-Bus error message arrives, see `dbus-event-error-functions'."
  (when tramp-gvfs-dbus-event-vector
    (tramp-message tramp-gvfs-dbus-event-vector 6 "%S" event)
    (tramp-error tramp-gvfs-dbus-event-vector 'file-error "%s" (cadr err))))

;; `dbus-event-error-hooks' has been renamed to
;; `dbus-event-error-functions' in Emacs 24.3.
(add-hook
 (if (boundp 'dbus-event-error-functions)
     'dbus-event-error-functions 'dbus-event-error-hooks)
 'tramp-gvfs-dbus-event-error)


;; File name primitives.

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
  (unless (memq op '(copy rename))
    (error "Unknown operation `%s', must be `copy' or `rename'" op))

  (setq filename (file-truename filename))
  (if (file-directory-p filename)
      (progn
	(copy-directory filename newname keep-date t)
	(when (eq op 'rename) (delete-directory filename 'recursive)))

    (let ((t1 (tramp-tramp-file-p filename))
	  (t2 (tramp-tramp-file-p newname))
	  (equal-remote (tramp-equal-remote filename newname))
	  (gvfs-operation (if (eq op 'copy) "gvfs-copy" "gvfs-move"))
	  (msg-operation (if (eq op 'copy) "Copying" "Renaming")))

      (with-parsed-tramp-file-name (if t1 filename newname) nil
	(when (and (not ok-if-already-exists) (file-exists-p newname))
	  (tramp-error v 'file-already-exists newname))

	(if (or (and equal-remote
		     (tramp-get-connection-property v "direct-copy-failed" nil))
		(and t1 (not (tramp-gvfs-file-name-p filename)))
		(and t2 (not (tramp-gvfs-file-name-p newname))))

	    ;; We cannot copy or rename directly.
	    (let ((tmpfile (tramp-compat-make-temp-file filename)))
	      (if (eq op 'copy)
		  (copy-file
		   filename tmpfile t keep-date preserve-uid-gid
		   preserve-extended-attributes)
		(rename-file filename tmpfile t))
	      (rename-file tmpfile newname ok-if-already-exists))

	  ;; Direct action.
	  (with-tramp-progress-reporter
	      v 0 (format "%s %s to %s" msg-operation filename newname)
	    (unless
		(apply
		 'tramp-gvfs-send-command v gvfs-operation
		 (append
		  (and (eq op 'copy) (or keep-date preserve-uid-gid)
		       '("--preserve"))
		  (list
		   (tramp-gvfs-url-file-name filename)
		   (tramp-gvfs-url-file-name newname))))

	      (if (or (not equal-remote)
		      (and equal-remote
			   (tramp-get-connection-property
			    v "direct-copy-failed" nil)))
		  ;; Propagate the error.
		  (with-current-buffer (tramp-get-connection-buffer v)
		    (goto-char (point-min))
		    (tramp-error-with-buffer
		     nil v 'file-error
		     "%s failed, see buffer `%s' for details."
		     msg-operation (buffer-name)))

		;; Some WebDAV server, like the one from QNAP, do not
		;; support direct copy/move.  Try a fallback.
		(tramp-set-connection-property v "direct-copy-failed" t)
		(tramp-gvfs-do-copy-or-rename-file
		 op filename newname ok-if-already-exists keep-date
		 preserve-uid-gid preserve-extended-attributes))))

	  (when (and t1 (eq op 'rename))
	    (with-parsed-tramp-file-name filename nil
	      (tramp-flush-file-properties v (file-name-directory localname))
	      (tramp-flush-file-properties v localname)))

	  (when t2
	    (with-parsed-tramp-file-name newname nil
	      (tramp-flush-file-properties v (file-name-directory localname))
	      (tramp-flush-file-properties v localname))))))))

(defun tramp-gvfs-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date
   preserve-uid-gid preserve-extended-attributes)
  "Like `copy-file' for Tramp files."
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  ;; At least one file a Tramp file?
  (if (or (tramp-tramp-file-p filename)
	  (tramp-tramp-file-p newname))
      (tramp-gvfs-do-copy-or-rename-file
       'copy filename newname ok-if-already-exists keep-date
       preserve-uid-gid preserve-extended-attributes)
    (tramp-run-real-handler
     'copy-file
     (list filename newname ok-if-already-exists keep-date
	   preserve-uid-gid preserve-extended-attributes))))

(defun tramp-gvfs-handle-delete-directory (directory &optional recursive trash)
  "Like `delete-directory' for Tramp files."
  (with-parsed-tramp-file-name directory nil
    (if (and recursive (not (file-symlink-p directory)))
	(mapc (lambda (file)
		(if (eq t (tramp-compat-file-attribute-type
			   (file-attributes file)))
		    (delete-directory file recursive trash)
		  (delete-file file trash)))
	      (directory-files
	       directory 'full directory-files-no-dot-files-regexp))
      (when (directory-files directory nil directory-files-no-dot-files-regexp)
	(tramp-error
	 v 'file-error "Couldn't delete non-empty %s" directory)))

    (tramp-flush-file-properties v (file-name-directory localname))
    (tramp-flush-directory-properties v localname)
    (unless
	(tramp-gvfs-send-command
	 v (if (and trash delete-by-moving-to-trash) "gvfs-trash" "gvfs-rm")
	 (tramp-gvfs-url-file-name directory))
      ;; Propagate the error.
      (with-current-buffer (tramp-get-connection-buffer v)
	(goto-char (point-min))
	(tramp-error-with-buffer
	 nil v 'file-error "Couldn't delete %s" directory)))))

(defun tramp-gvfs-handle-delete-file (filename &optional trash)
  "Like `delete-file' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (tramp-flush-file-properties v (file-name-directory localname))
    (tramp-flush-file-properties v localname)
    (unless
	(tramp-gvfs-send-command
	 v (if (and trash delete-by-moving-to-trash) "gvfs-trash" "gvfs-rm")
	 (tramp-gvfs-url-file-name filename))
      ;; Propagate the error.
      (with-current-buffer (tramp-get-connection-buffer v)
	(goto-char (point-min))
	(tramp-error-with-buffer
	 nil v 'file-error "Couldn't delete %s" filename)))))

(defun tramp-gvfs-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for Tramp files."
  ;; If DIR is not given, use DEFAULT-DIRECTORY or "/".
  (setq dir (or dir default-directory "/"))
  ;; Unless NAME is absolute, concat DIR and NAME.
  (unless (file-name-absolute-p name)
    (setq name (concat (file-name-as-directory dir) name)))
  ;; If NAME is not a Tramp file, run the real handler.
  (if (not (tramp-tramp-file-p name))
      (tramp-run-real-handler 'expand-file-name (list name nil))
    ;; Dissect NAME.
    (with-parsed-tramp-file-name name nil
      ;; If there is a default location, expand tilde.
      (when (string-match "\\`\\(~\\)\\(/\\|\\'\\)" localname)
	(save-match-data
	  (tramp-gvfs-maybe-open-connection
	   (make-tramp-file-name
	    :method method :user user :domain domain
	    :host host :port port :localname "/" :hop hop)))
	(setq localname
	      (replace-match
	       (tramp-get-connection-property v "default-location" "~")
	       nil t localname 1)))
      ;; Tilde expansion is not possible.
      (when (string-match "\\`\\(~[^/]*\\)\\(.*\\)\\'" localname)
	(tramp-error
	 v 'file-error
	 "Cannot expand tilde in file `%s'" name))
      (unless (tramp-run-real-handler 'file-name-absolute-p (list localname))
	(setq localname (concat "/" localname)))
      ;; We do not pass "/..".
      (if (string-match "^\\(afp\\|davs?\\|smb\\)$" method)
	  (when (string-match "^/[^/]+\\(/\\.\\./?\\)" localname)
	    (setq localname (replace-match "/" t t localname 1)))
	(when (string-match "^/\\.\\./?" localname)
	  (setq localname (replace-match "/" t t localname))))
      ;; There might be a double slash.  Remove this.
      (while (string-match "//" localname)
	(setq localname (replace-match "/" t t localname)))
      ;; No tilde characters in file name, do normal
      ;; `expand-file-name' (this does "/./" and "/../").
      (tramp-make-tramp-file-name
       method user domain host port
       (tramp-run-real-handler 'expand-file-name (list localname))))))

(defun tramp-gvfs-get-directory-attributes (directory)
  "Return GVFS attributes association list of all files in DIRECTORY."
  (ignore-errors
    ;; Don't modify `last-coding-system-used' by accident.
    (let ((last-coding-system-used last-coding-system-used)
	  result)
      (with-parsed-tramp-file-name directory nil
	(with-tramp-file-property v localname "directory-attributes"
	  (tramp-message v 5 "directory gvfs attributes: %s" localname)
	  ;; Send command.
	  (tramp-gvfs-send-command
	   v "gvfs-ls" "-h" "-n" "-a"
	   (mapconcat 'identity tramp-gvfs-file-attributes ",")
	   (tramp-gvfs-url-file-name directory))
	  ;; Parse output.
	  (with-current-buffer (tramp-get-connection-buffer v)
	    (goto-char (point-min))
	    (while (looking-at
		    (concat "^\\(.+\\)[[:blank:]]"
			    "\\([[:digit:]]+\\)[[:blank:]]"
			    "(\\(.+?\\))"
			    tramp-gvfs-file-attributes-with-gvfs-ls-regexp))
	      (let ((item (list (cons "type" (match-string 3))
				(cons "standard::size" (match-string 2))
				(cons "name" (match-string 1)))))
		(goto-char (1+ (match-end 3)))
		(while (looking-at
			(concat
			 tramp-gvfs-file-attributes-with-gvfs-ls-regexp
			 "\\(" tramp-gvfs-file-attributes-with-gvfs-ls-regexp
			 "\\|" "$" "\\)"))
		  (push (cons (match-string 1) (match-string 2)) item)
		  (goto-char (match-end 2)))
		;; Add display name as head.
		(push
		 (cons (cdr (or (assoc "standard::display-name" item)
				(assoc "name" item)))
		       (nreverse item))
		 result))
	      (forward-line)))
	  result)))))

(defun tramp-gvfs-get-root-attributes (filename &optional file-system)
  "Return GVFS attributes association list of FILENAME.
If FILE-SYSTEM is non-nil, return file system attributes."
  (ignore-errors
    ;; Don't modify `last-coding-system-used' by accident.
    (let ((last-coding-system-used last-coding-system-used)
	  result)
      (with-parsed-tramp-file-name filename nil
	(with-tramp-file-property
	    v localname
	    (if file-system "file-system-attributes" "file-attributes")
	  (tramp-message
	   v 5 "file%s gvfs attributes: %s"
	   (if file-system " system" "") localname)
	  ;; Send command.
	  (if file-system
	      (tramp-gvfs-send-command
	       v "gvfs-info" "--filesystem" (tramp-gvfs-url-file-name filename))
	    (tramp-gvfs-send-command
	     v "gvfs-info" (tramp-gvfs-url-file-name filename)))
	  ;; Parse output.
	  (with-current-buffer (tramp-get-connection-buffer v)
	    (goto-char (point-min))
	    (while (re-search-forward
		    (if file-system
			tramp-gvfs-file-system-attributes-regexp
		      tramp-gvfs-file-attributes-with-gvfs-info-regexp)
		    nil t)
	      (push (cons (match-string 1) (match-string 2)) result))
	    result))))))

(defun tramp-gvfs-get-file-attributes (filename)
  "Return GVFS attributes association list of FILENAME."
  (setq filename (directory-file-name (expand-file-name filename)))
  (with-parsed-tramp-file-name filename nil
    (setq localname (tramp-compat-file-name-unquote localname))
    (if (or (and (string-match "^\\(afp\\|davs?\\|smb\\)$" method)
		 (string-match "^/?\\([^/]+\\)$" localname))
	    (string-equal localname "/"))
	(tramp-gvfs-get-root-attributes filename)
      (assoc
       (file-name-nondirectory filename)
       (tramp-gvfs-get-directory-attributes (file-name-directory filename))))))

(defun tramp-gvfs-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  (unless id-format (setq id-format 'integer))
  (ignore-errors
    (let ((attributes (tramp-gvfs-get-file-attributes filename))
	  dirp res-symlink-target res-numlinks res-uid res-gid res-access
	  res-mod res-change res-size res-filemodes res-inode res-device)
      (when attributes
	;; ... directory or symlink
	(setq dirp (if (equal "directory" (cdr (assoc "type" attributes))) t))
	(setq res-symlink-target
	      (cdr (assoc "standard::symlink-target" attributes)))
	;; ... number links
	(setq res-numlinks
	      (string-to-number
	       (or (cdr (assoc "unix::nlink" attributes)) "0")))
	;; ... uid and gid
	(setq res-uid
	      (if (eq id-format 'integer)
		  (string-to-number
		   (or (cdr (assoc "unix::uid" attributes))
		       (format "%s" tramp-unknown-id-integer)))
		(or (cdr (assoc "owner::user" attributes))
		    (cdr (assoc "unix::uid" attributes))
		    tramp-unknown-id-string)))
	(setq res-gid
	      (if (eq id-format 'integer)
		  (string-to-number
		   (or (cdr (assoc "unix::gid" attributes))
		       (format "%s" tramp-unknown-id-integer)))
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
	      (let ((n (cdr (assoc "unix::mode" attributes))))
		(if n
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
		       "-" "x")))))
	;; ... inode and device
	(setq res-inode
	      (let ((n (cdr (assoc "unix::inode" attributes))))
		(if n
		    (string-to-number n)
		  (tramp-get-inode (tramp-dissect-file-name filename)))))
	(setq res-device
	      (let ((n (cdr (assoc "unix::device" attributes))))
		(if n
		    (string-to-number n)
		  (tramp-get-device (tramp-dissect-file-name filename)))))

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
	 )))))

(defun tramp-gvfs-handle-file-executable-p (filename)
  "Like `file-executable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-executable-p"
      (tramp-check-cached-permissions v ?x))))

(defun tramp-gvfs-handle-file-local-copy (filename)
  "Like `file-local-copy' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (unless (file-exists-p filename)
      (tramp-error
       v tramp-file-missing
       "Cannot make local copy of non-existing file `%s'" filename))
    (let ((tmpfile (tramp-compat-make-temp-file filename)))
      (copy-file filename tmpfile 'ok-if-already-exists 'keep-time)
      tmpfile)))

(defun tramp-gvfs-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (unless (save-match-data (string-match "/" filename))
    (all-completions
     filename
     (with-parsed-tramp-file-name (expand-file-name directory) nil
       (with-tramp-file-property v localname "file-name-all-completions"
         (let ((result '("./" "../")))
           ;; Get a list of directories and files.
	   (dolist (item (tramp-gvfs-get-directory-attributes directory) result)
	     (if (string-equal (cdr (assoc "type" item)) "directory")
		 (push (file-name-as-directory (car item)) result)
	       (push (car item) result)))))))))

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
			attribute-changed))
	     ((memq 'change flags)
	      '(created changed changes-done-hint moved deleted))
	     ((memq 'attribute-change flags) '(attribute-changed))))
	   (p (apply
	       'start-process
	       "gvfs-monitor" (generate-new-buffer " *gvfs-monitor*")
	       `("gio" "monitor" ,(tramp-gvfs-url-file-name file-name)))))
      (if (not (processp p))
	  (tramp-error
	   v 'file-notify-error "Monitoring not supported for `%s'" file-name)
	(tramp-message
	 v 6 "Run `%s', %S" (mapconcat 'identity (process-command p) " ") p)
	(process-put p 'vector v)
	(process-put p 'events events)
	(process-put p 'watch-name localname)
	(process-put p 'adjust-window-size-function 'ignore)
	(set-process-query-on-exit-flag p nil)
	(set-process-filter p 'tramp-gvfs-monitor-process-filter)
	;; There might be an error if the monitor is not supported.
	;; Give the filter a chance to read the output.
	(tramp-accept-process-output p 1)
	(unless (process-live-p p)
	  (tramp-error
	   p 'file-notify-error "Monitoring not supported for `%s'" file-name))
	p))))

(defun tramp-gvfs-monitor-process-filter (proc string)
  "Read output from \"gvfs-monitor-file\" and add corresponding \
file-notify events."
  (let* ((events (process-get proc 'events))
	 (rest-string (process-get proc 'rest-string))
	 (dd (with-current-buffer (process-buffer proc) default-directory))
	 (ddu (regexp-quote (tramp-gvfs-url-file-name dd))))
    (when rest-string
      (tramp-message proc 10 "Previous string:\n%s" rest-string))
    (tramp-message proc 6 "%S\n%s" proc string)
    (setq string (concat rest-string string)
          ;; Fix action names.
          string (replace-regexp-in-string
	          "attributes changed" "attribute-changed" string)
          string (replace-regexp-in-string
	          "changes done" "changes-done-hint" string)
          string (replace-regexp-in-string
	          "renamed to" "moved" string))
    ;; https://bugs.launchpad.net/bugs/1742946
    (when (string-match "Monitoring not supported\\|No locations given" string)
      (delete-process proc))

    (while (string-match
	    (concat "^.+:"
		    "[[:space:]]\\(.+\\):"
		    "[[:space:]]" (regexp-opt tramp-gio-events t)
		    "\\([[:space:]]\\(.+\\)\\)?$")
	    string)

      (let ((file (match-string 1 string))
	    (file1 (match-string 4 string))
	    (action (intern-soft (match-string 2 string))))
	(setq string (replace-match "" nil nil string))
	;; File names are returned as URL paths.  We must convert them.
	(when (string-match ddu file)
	  (setq file (replace-match dd nil nil file)))
	(while (string-match "%\\([0-9A-F]\\{2\\}\\)" file)
	  (setq file (url-unhex-string file)))
	(when (string-match ddu (or file1 ""))
	  (setq file1 (replace-match dd nil nil file1)))
	(while (string-match "%\\([0-9A-F]\\{2\\}\\)" (or file1 ""))
	  (setq file1 (url-unhex-string file1)))
	;; Remove watch when file or directory to be watched is deleted.
	(when (and (member action '(moved deleted))
		   (string-equal file (process-get proc 'watch-name)))
	  (delete-process proc))
	;; Usually, we would add an Emacs event now.  Unfortunately,
	;; `unread-command-events' does not accept several events at
	;; once.  Therefore, we apply the callback directly.
	(when (member action events)
	  (tramp-compat-funcall
           'file-notify-callback (list proc action file file1)))))

    ;; Save rest of the string.
    (when (zerop (length string)) (setq string nil))
    (when string (tramp-message proc 10 "Rest string:\n%s" string))
    (process-put proc 'rest-string string)))

(defun tramp-gvfs-handle-file-readable-p (filename)
  "Like `file-readable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-readable-p"
      (tramp-check-cached-permissions v ?r))))

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
      (when (and (stringp size) (stringp used) (stringp free))
	(list (string-to-number (concat size "e0"))
	      (- (string-to-number (concat size "e0"))
		 (string-to-number (concat used "e0")))
	      (string-to-number (concat free "e0")))))))

(defun tramp-gvfs-handle-file-writable-p (filename)
  "Like `file-writable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-writable-p"
      (if (file-exists-p filename)
	  (tramp-check-cached-permissions v ?w)
	;; If file doesn't exist, check if directory is writable.
	(and (file-directory-p (file-name-directory filename))
	     (file-writable-p (file-name-directory filename)))))))

(defun tramp-gvfs-handle-make-directory (dir &optional parents)
  "Like `make-directory' for Tramp files."
  (setq dir (directory-file-name (expand-file-name dir)))
  (with-parsed-tramp-file-name dir nil
    (tramp-flush-file-properties v (file-name-directory localname))
    (tramp-flush-directory-properties v localname)
    (save-match-data
      (let ((ldir (file-name-directory dir)))
	;; Make missing directory parts.  "gvfs-mkdir -p ..." does not
	;; work robust.
	(when (and parents (not (file-directory-p ldir)))
	  (make-directory ldir parents))
	;; Just do it.
	(unless (or (tramp-gvfs-send-command
		     v "gvfs-mkdir" (tramp-gvfs-url-file-name dir))
		    (and parents (file-directory-p dir)))
	  (tramp-error v 'file-error "Couldn't make directory %s" dir))))))

(defun tramp-gvfs-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for Tramp files."
  ;; Check if both files are local -- invoke normal rename-file.
  ;; Otherwise, use Tramp from local system.
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  ;; At least one file a Tramp file?
  (if (or (tramp-tramp-file-p filename)
          (tramp-tramp-file-p newname))
      (tramp-gvfs-do-copy-or-rename-file
       'rename filename newname ok-if-already-exists
       'keep-date 'preserve-uid-gid)
    (tramp-run-real-handler
     'rename-file (list filename newname ok-if-already-exists))))

(defun tramp-gvfs-handle-write-region
  (start end filename &optional append visit lockname mustbenew)
  "Like `write-region' for Tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (when (and mustbenew (file-exists-p filename)
	       (or (eq mustbenew 'excl)
		   (not
		    (y-or-n-p
		     (format "File %s exists; overwrite anyway? " filename)))))
      (tramp-error v 'file-already-exists filename))

    (let ((tmpfile (tramp-compat-make-temp-file filename)))
      (when (and append (file-exists-p filename))
	(copy-file filename tmpfile 'ok))
      ;; We say `no-message' here because we don't want the visited file
      ;; modtime data to be clobbered from the temp file.  We call
      ;; `set-visited-file-modtime' ourselves later on.
      (tramp-run-real-handler
       'write-region (list start end tmpfile append 'no-message lockname))
      (condition-case nil
	  (rename-file tmpfile filename 'ok-if-already-exists)
	(error
	 (delete-file tmpfile)
	 (tramp-error
	  v 'file-error "Couldn't write region to `%s'" filename))))

    (tramp-flush-file-properties v (file-name-directory localname))
    (tramp-flush-file-properties v localname)

    ;; Set file modification time.
    (when (or (eq visit t) (stringp visit))
      (set-visited-file-modtime
       (tramp-compat-file-attribute-modification-time
	(file-attributes filename))))

    ;; The end.
    (when (and (null noninteractive)
	       (or (eq visit t) (null visit) (stringp visit)))
      (tramp-message v 0 "Wrote %s" filename))
    (run-hooks 'tramp-handle-write-region-hook)))


;; File name conversions.

(defun tramp-gvfs-url-file-name (filename)
  "Return FILENAME in URL syntax."
  ;; "/" must NOT be hexified.
  (setq filename (tramp-compat-file-name-unquote filename))
  (let ((url-unreserved-chars (cons ?/ url-unreserved-chars))
	result)
    (setq
     result
     (url-recreate-url
      (if (tramp-tramp-file-p filename)
	  (with-parsed-tramp-file-name filename nil
	    (when (string-equal "gdrive" method)
	      (setq method "google-drive"))
	    (when (string-equal "owncloud" method)
	      (setq method "davs"
		    localname
		    (concat (tramp-gvfs-get-remote-prefix v) localname)))
	    (when (and user domain)
	      (setq user (concat domain ";" user)))
	    (url-parse-make-urlobj
	     method (and user (url-hexify-string user))
	     nil (and host (url-hexify-string host))
	     (if (stringp port) (string-to-number port) port)
	     (and localname (url-hexify-string localname)) nil nil t))
	(url-parse-make-urlobj
	 "file" nil nil nil nil
	 (url-hexify-string (file-truename filename)) nil nil t))))
    (when (tramp-tramp-file-p filename)
      (with-parsed-tramp-file-name filename nil
	(tramp-message v 10 "remote file `%s' is URL `%s'" filename result)))
    result))

(defun tramp-gvfs-object-path (filename)
  "Create a D-Bus object path from FILENAME."
  (expand-file-name (dbus-escape-as-identifier filename) tramp-gvfs-path-tramp))

(defun tramp-gvfs-file-name (object-path)
  "Retrieve file name from D-Bus OBJECT-PATH."
  (dbus-unescape-from-identifier
   (replace-regexp-in-string "^.*/\\([^/]+\\)$" "\\1" object-path)))


;; D-Bus GVFS functions.

(defun tramp-gvfs-handler-askpassword (message user domain flags)
  "Implementation for the \"org.gtk.vfs.MountOperation.askPassword\" method."
  (let* ((filename
	  (tramp-gvfs-file-name (dbus-event-path-name last-input-event)))
	 (pw-prompt
	  (format
	   "%s for %s "
	   (if (string-match "\\([pP]assword\\|[pP]assphrase\\)" message)
	       (capitalize (match-string 1 message))
	     "Password")
	   filename))
	 password)

    (condition-case nil
	(with-parsed-tramp-file-name filename l
	  (when (and (zerop (length user))
		     (not
		      (zerop (logand flags tramp-gvfs-password-need-username))))
	    (setq user (read-string "User name: ")))
	  (when (and (zerop (length domain))
		     (not
		      (zerop (logand flags tramp-gvfs-password-need-domain))))
	    (setq domain (read-string "Domain name: ")))

	  (tramp-message l 6 "%S %S %S %d" message user domain flags)
	  (unless (tramp-get-connection-property l "first-password-request" nil)
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
		   (with-tramp-connection-property
		       (tramp-get-connection-process v) message
		     ;; In theory, there can be several choices.
		     ;; Until now, there is only the question whether
		     ;; to accept an unknown host signature or certificate.
		     (with-temp-buffer
		       ;; Preserve message for `progress-reporter'.
		       (with-temp-message ""
			 (insert message)
			 (goto-char (point-max))
			 (if noninteractive
			     (message "%s" message)
			   (pop-to-buffer (current-buffer)))
			 (if (yes-or-no-p
			      (concat
			       (buffer-substring
				(line-beginning-position) (point))
			       " "))
			     0 1)))))

		;; When QUIT is raised, we shall return this
		;; information to D-Bus.
		(quit (list nil t 1))))

	(tramp-message v 6 "%s" result)

	;; When the choice is "no", we set a dummy fuse-mountpoint in
	;; order to leave the timeout.
	(unless (zerop (cl-caddr result))
	  (tramp-set-file-property v "/" "fuse-mountpoint" "/"))

	result))))

(defun tramp-gvfs-handler-mounted-unmounted (mount-info)
  "Signal handler for the \"org.gtk.vfs.MountTracker.mounted\" and
\"org.gtk.vfs.MountTracker.unmounted\" signals."
  (ignore-errors
    (let ((signal-name (dbus-event-member-name last-input-event))
	  (elt mount-info))
      ;; Jump over the first elements of the mount info. Since there
      ;; were changes in the entries, we cannot access dedicated
      ;; elements.
      (while (stringp (car elt)) (setq elt (cdr elt)))
      (let* ((fuse-mountpoint (tramp-gvfs-dbus-byte-array-to-string (cadr elt)))
	     (mount-spec (cl-caddr elt))
	     (prefix (tramp-gvfs-dbus-byte-array-to-string (car mount-spec)))
	     (default-location (tramp-gvfs-dbus-byte-array-to-string
				(cl-cadddr elt)))
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
	(when (string-match "^\\(afp\\|smb\\)" method)
	  (setq method (match-string 1 method)))
	(when (string-equal "obex" method)
	  (setq host (tramp-bluez-device host)))
	(when (and (string-equal "dav" method) (string-equal "true" ssl))
	  (setq method "davs"))
	(when (and (string-equal "davs" method)
		   (string-match
		    tramp-gvfs-owncloud-default-prefix-regexp prefix))
	  (setq method "owncloud"))
	(when (string-equal "google-drive" method)
	  (setq method "gdrive"))
	(when (and (string-equal "http" method) (stringp uri))
	  (setq uri (url-generic-parse-url uri)
		method (url-type uri)
		user (url-user uri)
		host (url-host uri)
		port (url-portspec uri)))
	(with-parsed-tramp-file-name
	    (tramp-make-tramp-file-name method user domain host port "") nil
	  (tramp-message
	   v 6 "%s %s"
	   signal-name (tramp-gvfs-stringify-dbus-message mount-info))
	  (tramp-flush-file-property v "/" "list-mounts")
	  (if (string-equal (downcase signal-name) "unmounted")
	      (tramp-flush-file-properties v "/")
	    ;; Set mountpoint and location.
	    (tramp-set-file-property v "/" "fuse-mountpoint" fuse-mountpoint)
	    (tramp-set-connection-property
	     v "default-location" default-location)))))))

(when tramp-gvfs-enabled
  (dbus-register-signal
   :session nil tramp-gvfs-path-mounttracker
   tramp-gvfs-interface-mounttracker "mounted"
   'tramp-gvfs-handler-mounted-unmounted)
  (dbus-register-signal
   :session nil tramp-gvfs-path-mounttracker
   tramp-gvfs-interface-mounttracker "Mounted"
   'tramp-gvfs-handler-mounted-unmounted)

  (dbus-register-signal
   :session nil tramp-gvfs-path-mounttracker
   tramp-gvfs-interface-mounttracker "unmounted"
   'tramp-gvfs-handler-mounted-unmounted)
  (dbus-register-signal
   :session nil tramp-gvfs-path-mounttracker
   tramp-gvfs-interface-mounttracker "Unmounted"
   'tramp-gvfs-handler-mounted-unmounted))

(defun tramp-gvfs-connection-mounted-p (vec)
  "Check, whether the location is already mounted."
  (or
   (tramp-get-file-property vec "/" "fuse-mountpoint" nil)
   (catch 'mounted
     (dolist
	 (elt
	  (with-tramp-file-property vec "/" "list-mounts"
	    (with-tramp-dbus-call-method vec t
	      :session tramp-gvfs-service-daemon tramp-gvfs-path-mounttracker
	      tramp-gvfs-interface-mounttracker tramp-gvfs-listmounts))
	  nil)
       ;; Jump over the first elements of the mount info. Since there
       ;; were changes in the entries, we cannot access dedicated
       ;; elements.
       (while (stringp (car elt)) (setq elt (cdr elt)))
       (let* ((fuse-mountpoint (tramp-gvfs-dbus-byte-array-to-string
				(cadr elt)))
	      (mount-spec (cl-caddr elt))
	      (prefix (tramp-gvfs-dbus-byte-array-to-string (car mount-spec)))
	      (default-location (tramp-gvfs-dbus-byte-array-to-string
				 (cl-cadddr elt)))
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
	 (when (string-match "^\\(afp\\|smb\\)" method)
	   (setq method (match-string 1 method)))
	 (when (string-equal "obex" method)
	   (setq host (tramp-bluez-device host)))
	 (when (and (string-equal "dav" method) (string-equal "true" ssl))
	   (setq method "davs"))
	 (when (and (string-equal "davs" method)
		    (string-match
		     tramp-gvfs-owncloud-default-prefix-regexp prefix))
	   (setq method "owncloud"))
	 (when (string-equal "google-drive" method)
	   (setq method "gdrive"))
	 (when (and (string-equal "synce" method) (zerop (length user)))
	   (setq user (or (tramp-file-name-user vec) "")))
	 (when (and (string-equal "http" method) (stringp uri))
	   (setq uri (url-generic-parse-url uri)
		 method (url-type uri)
		 user (url-user uri)
		 host (url-host uri)
		 port (url-portspec uri)))
	 (when (and
		(string-equal method (tramp-file-name-method vec))
		(string-equal user (tramp-file-name-user vec))
		(string-equal domain (tramp-file-name-domain vec))
		(string-equal host (tramp-file-name-host vec))
		(string-equal port (tramp-file-name-port vec))
		(string-match (concat "^/" (regexp-quote (or share "")))
			      (tramp-file-name-unquote-localname vec)))
	   ;; Set mountpoint and location.
	   (tramp-set-file-property vec "/" "fuse-mountpoint" fuse-mountpoint)
	   (tramp-set-connection-property
	    vec "default-location" default-location)
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
  (tramp-flush-connection-properties vec)
  (tramp-flush-connection-properties (tramp-get-connection-process vec)))

(defun tramp-gvfs-mount-spec-entry (key value)
  "Construct a mount-spec entry to be used in a mount_spec.
It was \"a(say)\", but has changed to \"a{sv})\"."
  (if (string-match "^(aya{sv})" tramp-gvfs-mountlocation-signature)
      (list :dict-entry key
	    (list :variant (tramp-gvfs-dbus-string-to-byte-array value)))
    (list :struct key (tramp-gvfs-dbus-string-to-byte-array value))))

(defun tramp-gvfs-mount-spec (vec)
  "Return a mount-spec for \"org.gtk.vfs.MountTracker.mountLocation\"."
  (let* ((method (tramp-file-name-method vec))
	 (user (tramp-file-name-user vec))
	 (domain (tramp-file-name-domain vec))
	 (host (tramp-file-name-host vec))
	 (port (tramp-file-name-port vec))
	 (localname (tramp-file-name-unquote-localname vec))
	 (share (when (string-match "^/?\\([^/]+\\)" localname)
		  (match-string 1 localname)))
	 (ssl (if (string-match "^davs\\|^owncloud" method) "true" "false"))
	 (mount-spec
          `(:array
            ,@(cond
               ((string-equal "smb" method)
                (list (tramp-gvfs-mount-spec-entry "type" "smb-share")
                      (tramp-gvfs-mount-spec-entry "server" host)
                      (tramp-gvfs-mount-spec-entry "share" share)))
               ((string-equal "obex" method)
                (list (tramp-gvfs-mount-spec-entry "type" method)
                      (tramp-gvfs-mount-spec-entry
                       "host" (concat "[" (tramp-bluez-address host) "]"))))
               ((string-match "^dav\\|^owncloud" method)
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
               ((string-match "^http" method)
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
          (if (and (string-match "^dav" method)
                   (string-match "^/?[^/]+" localname))
              (match-string 0 localname)
	    (tramp-gvfs-get-remote-prefix vec))))

    ;; Return.
    `(:struct ,(tramp-gvfs-dbus-string-to-byte-array mount-pref) ,mount-spec)))


;; Connection functions.

(defun tramp-gvfs-get-remote-uid (vec id-format)
  "The uid of the remote connection VEC, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  (with-tramp-connection-property vec (format "uid-%s" id-format)
    (let ((method (tramp-file-name-method vec))
	  (user (tramp-file-name-user vec))
	  (domain (tramp-file-name-domain vec))
	  (host (tramp-file-name-host vec))
	  (port (tramp-file-name-port vec))
	  (localname
	   (tramp-get-connection-property vec "default-location" nil)))
      (cond
       ((and user (equal id-format 'string)) user)
       (localname
	(tramp-compat-file-attribute-user-id
	 (file-attributes
	  (tramp-make-tramp-file-name method user domain host port localname)
	  id-format)))
       ((equal id-format 'integer) tramp-unknown-id-integer)
       ((equal id-format 'string) tramp-unknown-id-string)))))

(defun tramp-gvfs-get-remote-gid (vec id-format)
  "The gid of the remote connection VEC, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  (with-tramp-connection-property vec (format "gid-%s" id-format)
    (let ((method (tramp-file-name-method vec))
	  (user (tramp-file-name-user vec))
	  (domain (tramp-file-name-domain vec))
	  (host (tramp-file-name-host vec))
	  (port (tramp-file-name-port vec))
	  (localname
	   (tramp-get-connection-property vec "default-location" nil)))
      (cond
       (localname
	(tramp-compat-file-attribute-group-id
	 (file-attributes
	  (tramp-make-tramp-file-name method user domain host port localname)
	  id-format)))
       ((equal id-format 'integer) tramp-unknown-id-integer)
       ((equal id-format 'string) tramp-unknown-id-string)))))

(defvar tramp-gvfs-get-remote-uid-gid-in-progress nil
  "Indication, that remote uid and gid determination is in progress.")

(defun tramp-gvfs-get-remote-prefix (vec)
  "The prefix of the remote connection VEC.
This is relevant for GNOME Online Accounts."
  (with-tramp-connection-property vec "prefix"
    ;; Ensure that GNOME Online Accounts are cached.
    (when (member (tramp-file-name-method vec) tramp-goa-methods)
      (tramp-get-goa-accounts vec))
    (tramp-get-connection-property
     (make-tramp-goa-name
      :method (tramp-file-name-method vec)
      :user (tramp-file-name-user vec)
      :host (tramp-file-name-host vec)
      :port (tramp-file-name-port vec))
     "prefix" "/")))

(defun tramp-gvfs-maybe-open-connection (vec)
  "Maybe open a connection VEC.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  ;; We set the file name, in case there are incoming D-Bus signals or
  ;; D-Bus errors.
  (setq tramp-gvfs-dbus-event-vector vec)

  ;; For password handling, we need a process bound to the connection
  ;; buffer.  Therefore, we create a dummy process.  Maybe there is a
  ;; better solution?
  (unless (get-buffer-process (tramp-get-connection-buffer vec))
    (let ((p (make-network-process
	      :name (tramp-buffer-name vec)
	      :buffer (tramp-get-connection-buffer vec)
	      :server t :host 'local :service t :noquery t)))
      (process-put p 'vector vec)
      (set-process-query-on-exit-flag p nil)))

  (unless (tramp-gvfs-connection-mounted-p vec)
    (let* ((method (tramp-file-name-method vec))
	   (user (tramp-file-name-user vec))
	   (domain (tramp-file-name-domain vec))
	   (host (tramp-file-name-host vec))
	   (port (tramp-file-name-port vec))
	   (localname (tramp-file-name-unquote-localname vec))
	   (object-path
	    (tramp-gvfs-object-path
	     (tramp-make-tramp-file-name method user domain host port ""))))

      (when (and (string-equal method "afp")
		 (string-equal localname "/"))
	(tramp-error vec 'file-error "Filename must contain an AFP volume"))

      (when (and (string-match method "davs?")
		 (string-equal localname "/"))
	(tramp-error vec 'file-error "Filename must contain a WebDAV share"))

      (when (and (string-equal method "smb")
		 (string-equal localname "/"))
	(tramp-error vec 'file-error "Filename must contain a Windows share"))

      (with-tramp-progress-reporter
	  vec 3
	  (if (zerop (length user))
	      (format "Opening connection for %s using %s" host method)
	    (format "Opening connection for %s@%s using %s" user host method))

	;; Enable `auth-source'.
	(tramp-set-connection-property
	 vec "first-password-request" tramp-cache-read-persistent-data)

	;; There will be a callback of "askPassword" when a password is needed.
	(dbus-register-method
	 :session dbus-service-emacs object-path
	 tramp-gvfs-interface-mountoperation "askPassword"
	 'tramp-gvfs-handler-askpassword)
	(dbus-register-method
	 :session dbus-service-emacs object-path
	 tramp-gvfs-interface-mountoperation "AskPassword"
	 'tramp-gvfs-handler-askpassword)

	;; There could be a callback of "askQuestion" when adding
	;; fingerprints or checking certificates.
	(dbus-register-method
	 :session dbus-service-emacs object-path
	 tramp-gvfs-interface-mountoperation "askQuestion"
	 'tramp-gvfs-handler-askquestion)
	(dbus-register-method
	 :session dbus-service-emacs object-path
	 tramp-gvfs-interface-mountoperation "AskQuestion"
	 'tramp-gvfs-handler-askquestion)

	;; The call must be asynchronously, because of the "askPassword"
	;; or "askQuestion" callbacks.
	(if (string-match "(so)$" tramp-gvfs-mountlocation-signature)
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
	;; indicated by the "mounted" signal, i.e. the "fuse-mountpoint"
	;; file property.
	(with-timeout
	    ((or (tramp-get-method-parameter vec 'tramp-connection-timeout)
		 tramp-connection-timeout)
	     (if (zerop (length (tramp-file-name-user vec)))
		 (tramp-error
		  vec 'file-error
		  "Timeout reached mounting %s using %s" host method)
	       (tramp-error
		vec 'file-error
		"Timeout reached mounting %s@%s using %s" user host method)))
	  (while (not (tramp-get-file-property vec "/" "fuse-mountpoint" nil))
	    (read-event nil nil 0.1)))

	;; If `tramp-gvfs-handler-askquestion' has returned "No", it
	;; is marked with the fuse-mountpoint "/".  We shall react.
	(when (string-equal
	       (tramp-get-file-property vec "/" "fuse-mountpoint" "") "/")
	  (tramp-error vec 'file-error "FUSE mount denied"))

	;; Set connection-local variables.
	(tramp-set-connection-local-variables vec)

	;; Mark it as connected.
	(tramp-set-connection-property
	 (tramp-get-connection-process vec) "connected" t))))

  ;; In `tramp-check-cached-permissions', the connection properties
  ;; {uig,gid}-{integer,string} are used.  We set them to proper values.
  (unless tramp-gvfs-get-remote-uid-gid-in-progress
    (let ((tramp-gvfs-get-remote-uid-gid-in-progress t))
      (tramp-gvfs-get-remote-uid vec 'integer)
      (tramp-gvfs-get-remote-gid vec 'integer)
      (tramp-gvfs-get-remote-uid vec 'string)
      (tramp-gvfs-get-remote-gid vec 'string))))

(defun tramp-gvfs-gio-tool-p (vec)
  "Check, whether the gio tool is available."
  (with-tramp-connection-property vec "gio-tool"
    (zerop (tramp-call-process vec "gio" nil nil nil "version"))))

(defun tramp-gvfs-send-command (vec command &rest args)
  "Send the COMMAND with its ARGS to connection VEC.
COMMAND is a command from the gvfs-* utilities.  It is replaced
by the corresponding gio tool call if available.  `call-process'
is applied, and it returns t if the return code is zero."
  (let* ((locale (tramp-get-local-locale vec))
	 (process-environment
	  (append
	   `(,(format "LANG=%s" locale)
	     ,(format "LANGUAGE=%s" locale)
	     ,(format "LC_ALL=%s" locale))
	   process-environment)))
    (when (tramp-gvfs-gio-tool-p vec)
      ;; Use gio tool.
      (setq args (cons (cdr (assoc command tramp-gvfs-gio-mapping)) args)
	    command "gio"))

    (with-current-buffer (tramp-get-connection-buffer vec)
      (tramp-gvfs-maybe-open-connection vec)
      (erase-buffer)
      (or (zerop (apply 'tramp-call-process vec command nil t nil args))
	  ;; Remove information about mounted connection.
	  (and (tramp-flush-file-properties vec "/") nil)))))


;; D-Bus GNOME Online Accounts functions.

(defun tramp-get-goa-accounts (vec)
  "Retrieve GNOME Online Accounts, and cache them.
The hash key is a `tramp-goa-name' structure.  The value is an
alist of the properties of `tramp-goa-interface-account' and
`tramp-goa-interface-files' of the corresponding GNOME online
account.  Additionally, a property \"prefix\" is added.
VEC is used only for traces."
  (dolist
      (object-path
       (mapcar
	'car
	(tramp-dbus-function
	 vec 'dbus-get-all-managed-objects
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
	(setq key (make-tramp-goa-name
		   :method (cdr (assoc "ProviderType" account-properties))
		   :user (match-string 1 identity)
		   :host (match-string 2 identity)
		   :port (match-string 3 identity)))
	(when (string-equal (tramp-goa-name-method key) "google")
	  (setf (tramp-goa-name-method key) "gdrive"))
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
	     (tramp-get-connection-property key "Uri" "file:///"))))))))))


;; D-Bus BLUEZ functions.

(defun tramp-bluez-address (device)
  "Return bluetooth device address from a given bluetooth DEVICE name."
  (when (stringp device)
    (if (string-match tramp-ipv6-regexp device)
	(match-string 0 device)
      (cadr (assoc device (tramp-bluez-list-devices))))))

(defun tramp-bluez-device (address)
  "Return bluetooth device name from a given bluetooth device ADDRESS.
ADDRESS can have the form \"xx:xx:xx:xx:xx:xx\" or \"[xx:xx:xx:xx:xx:xx]\"."
  (when (stringp address)
    (while (string-match "[][]" address)
      (setq address (replace-match "" t t address)))
    (let (result)
      (dolist (item (tramp-bluez-list-devices) result)
	(when (string-match address (cadr item))
	  (setq result (car item)))))))

(defun tramp-bluez-list-devices ()
  "Return all discovered bluetooth devices as list.
Every entry is a list (NAME ADDRESS).

If `tramp-bluez-discover-devices-timeout' is an integer, and the last
discovery happened more time before indicated there, a rescan will be
started, which lasts some ten seconds.  Otherwise, cached results will
be used."
  ;; Reset the scanned devices list if time has passed.
  (and (integerp tramp-bluez-discover-devices-timeout)
       (integerp tramp-bluez-discovery)
       (> (tramp-time-diff (current-time) tramp-bluez-discovery)
	  tramp-bluez-discover-devices-timeout)
       (setq tramp-bluez-devices nil))

  ;; Rescan if needed.
  (unless tramp-bluez-devices
    (let ((object-path
	   (with-tramp-dbus-call-method tramp-gvfs-dbus-event-vector t
	     :system tramp-bluez-service "/"
	     tramp-bluez-interface-manager "DefaultAdapter")))
      (setq tramp-bluez-devices nil
	    tramp-bluez-discovery t)
      (with-tramp-dbus-call-method tramp-gvfs-dbus-event-vector nil
	:system tramp-bluez-service object-path
	tramp-bluez-interface-adapter "StartDiscovery")
      (while tramp-bluez-discovery
	(read-event nil nil 0.1))))
  (setq tramp-bluez-discovery (current-time))
  (tramp-message tramp-gvfs-dbus-event-vector 10 "%s" tramp-bluez-devices)
  tramp-bluez-devices)

(defun tramp-bluez-property-changed (property value)
  "Signal handler for the \"org.bluez.Adapter.PropertyChanged\" signal."
  (tramp-message tramp-gvfs-dbus-event-vector 6 "%s %s" property value)
  (cond
   ((string-equal property "Discovering")
    (unless (car value)
      ;; "Discovering" FALSE means discovery run has been completed.
      ;; We stop it, because we don't need another run.
      (setq tramp-bluez-discovery nil)
      (with-tramp-dbus-call-method tramp-gvfs-dbus-event-vector t
	:system tramp-bluez-service (dbus-event-path-name last-input-event)
	tramp-bluez-interface-adapter "StopDiscovery")))))

(when tramp-gvfs-enabled
  (dbus-register-signal
   :system nil nil tramp-bluez-interface-adapter "PropertyChanged"
   'tramp-bluez-property-changed))

(defun tramp-bluez-device-found (device args)
  "Signal handler for the \"org.bluez.Adapter.DeviceFound\" signal."
  (tramp-message tramp-gvfs-dbus-event-vector 6 "%s %s" device args)
  (let ((alias (car (cadr (assoc "Alias" args))))
	(address (car (cadr (assoc "Address" args)))))
    ;; Maybe we shall check the device class for being a proper
    ;; device, and call also SDP in order to find the obex service.
    (add-to-list 'tramp-bluez-devices (list alias address))))

(when tramp-gvfs-enabled
  (dbus-register-signal
   :system nil nil tramp-bluez-interface-adapter "DeviceFound"
   'tramp-bluez-device-found))

(defun tramp-bluez-parse-device-names (_ignore)
  "Return a list of (nil host) tuples allowed to access."
  (mapcar
   (lambda (x) (list nil (car x)))
   (tramp-bluez-list-devices)))

;; Add completion function for OBEX method.
(when (and tramp-gvfs-enabled
	   (member tramp-bluez-service (dbus-list-known-names :system)))
  (tramp-set-completion-function
   "obex" '((tramp-bluez-parse-device-names ""))))


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
	 (setq host (format "%s%s%d" host tramp-prefix-port-regexp port)))
       ;; A user is marked in a TXT field like "u=guest".
       (while text
	 (when (string-match "u=\\(.+\\)$" (car text))
	   (setq user (match-string 1 (car text))))
	 (setq text (cdr text)))
       (list user host)))
   (zeroconf-list-services service)))

;; We use the TRIM argument of `split-string', which exist since Emacs
;; 24.4.  I mask this for older Emacs versions, there is no harm.
(defun tramp-gvfs-parse-device-names (service)
  "Return a list of (user host) tuples allowed to access.
This uses \"avahi-browse\" in case D-Bus is not enabled in Avahi."
  (let ((result
	 (ignore-errors
	   (tramp-compat-funcall
	    'split-string
	    (shell-command-to-string (format "avahi-browse -trkp %s" service))
	    "[\n\r]+" 'omit "^\\+;.*$"))))
    (delete-dups
     (mapcar
      (lambda (x)
	(let* ((list (split-string x ";"))
	       (host (nth 6 list))
	       (text (tramp-compat-funcall
		      'split-string (nth 9 list) "\" \"" 'omit "\""))
	       user)
	  ;; A user is marked in a TXT field like "u=guest".
	  (while text
	    (when (string-match "u=\\(.+\\)$" (car text))
	      (setq user (match-string 1 (car text))))
	    (setq text (cdr text)))
	  (list user host)))
      result))))

;; Add completion functions for AFP, DAV, DAVS, SFTP and SMB methods.
(when tramp-gvfs-enabled
  ;; Suppress D-Bus error messages.
  (let (tramp-gvfs-dbus-event-vector)
    (zeroconf-init tramp-gvfs-zeroconf-domain)
    (if (zeroconf-list-service-types)
	(progn
	  (tramp-set-completion-function
	   "afp" '((tramp-zeroconf-parse-device-names "_afpovertcp._tcp")))
	  (tramp-set-completion-function
	   "dav" '((tramp-zeroconf-parse-device-names "_webdav._tcp")))
	  (tramp-set-completion-function
	   "davs" '((tramp-zeroconf-parse-device-names "_webdav._tcp")))
	  (tramp-set-completion-function
	   "sftp" '((tramp-zeroconf-parse-device-names "_ssh._tcp")
		    (tramp-zeroconf-parse-device-names "_workstation._tcp")))
	  (when (member "smb" tramp-gvfs-methods)
	    (tramp-set-completion-function
	     "smb" '((tramp-zeroconf-parse-device-names "_smb._tcp")))))

      (when (executable-find "avahi-browse")
	(tramp-set-completion-function
	 "afp" '((tramp-gvfs-parse-device-names "_afpovertcp._tcp")))
	(tramp-set-completion-function
	 "dav" '((tramp-gvfs-parse-device-names "_webdav._tcp")))
	(tramp-set-completion-function
	 "davs" '((tramp-gvfs-parse-device-names "_webdav._tcp")))
	(tramp-set-completion-function
	 "sftp" '((tramp-gvfs-parse-device-names "_ssh._tcp")
		  (tramp-gvfs-parse-device-names "_workstation._tcp")))
	(when (member "smb" tramp-gvfs-methods)
	  (tramp-set-completion-function
	   "smb" '((tramp-gvfs-parse-device-names "_smb._tcp"))))))))


;; D-Bus SYNCE functions.

(defun tramp-synce-list-devices ()
  "Return all discovered synce devices as list.
They are retrieved from the hal daemon."
  (let (tramp-synce-devices)
    (dolist (device
	     (with-tramp-dbus-call-method tramp-gvfs-dbus-event-vector t
	       :system tramp-hal-service tramp-hal-path-manager
	       tramp-hal-interface-manager "GetAllDevices"))
      (when (with-tramp-dbus-call-method tramp-gvfs-dbus-event-vector t
	      :system tramp-hal-service device tramp-hal-interface-device
	      "PropertyExists" "sync.plugin")
	(let ((prop
	       (with-tramp-dbus-call-method
		tramp-gvfs-dbus-event-vector t
		:system tramp-hal-service device tramp-hal-interface-device
		"GetPropertyString" "pda.pocketpc.name")))
	  (unless (member prop tramp-synce-devices)
	    (push prop tramp-synce-devices)))))
    (tramp-message tramp-gvfs-dbus-event-vector 10 "%s" tramp-synce-devices)
    tramp-synce-devices))

(defun tramp-synce-parse-device-names (_ignore)
  "Return a list of (nil host) tuples allowed to access."
  (mapcar
   (lambda (x) (list nil x))
   (tramp-synce-list-devices)))

;; Add completion function for SYNCE method.
(when tramp-gvfs-enabled
  (tramp-set-completion-function
   "synce" '((tramp-synce-parse-device-names ""))))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-gvfs 'force)))

(provide 'tramp-gvfs)

;;; TODO:

;; * (Customizable) unmount when exiting Emacs.  See tramp-archive.el.

;; * Host name completion for existing mount points (afp-server,
;;   smb-server, google-drive, owncloud) or via smb-network.
;;
;; * Check, how two shares of the same SMB server can be mounted in
;;   parallel.
;;
;; * Apply SDP on bluetooth devices, in order to filter out obex
;;   capability.
;;
;; * Implement obex for other serial communication but bluetooth.

;;; tramp-gvfs.el ends here
