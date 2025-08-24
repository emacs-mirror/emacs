!include MUI2.nsh
!include LogicLib.nsh
!include x64.nsh

Outfile "emacs-${OUT_VERSION}-installer.exe"


SetCompressor /solid lzma

Var StartMenuFolder
Var UninstallerPath

!define UNINST_KEY \
    "Software\Microsoft\Windows\CurrentVersion\Uninstall\emacs-${VERSION_BRANCH}"

!define MUI_WELCOMEPAGE_TITLE "Emacs"
!define MUI_WELCOMEPAGE_TITLE_3LINES
!define MUI_WELCOMEPAGE_TEXT "Welcome to Emacs -- the editor of a lifetime."

!define MUI_WELCOMEFINISHPAGE_BITMAP "emacs-${VERSION_BRANCH}\share\emacs\${EMACS_VERSION}\etc\images\splash.bmp"
!define MUI_ICON "emacs-${VERSION_BRANCH}\share\emacs\${EMACS_VERSION}\etc\images\icons\hicolor\scalable\apps\emacs.ico"
!define MUI_UNICON "emacs-${VERSION_BRANCH}\share\emacs\${EMACS_VERSION}\etc\images\icons\hicolor\scalable\apps\emacs.ico"

!insertmacro MUI_PAGE_WELCOME

# licensing/about click-though page
!define MUI_PAGE_HEADER_TEXT "Emacs is Free Software"
!define MUI_PAGE_HEADER_SUBTEXT "A component of the GNU operating system."
!define MUI_LICENSEPAGE_TEXT_TOP "This program is free software."
!define MUI_LICENSEPAGE_TEXT_BOTTOM "You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License (as above), or (at your option) any later version."
!define MUI_LICENSEPAGE_BUTTON "OK"
!insertmacro MUI_PAGE_LICENSE "emacs-${VERSION_BRANCH}\share\emacs\${EMACS_VERSION}\lisp\COPYING"

# user option page: installation path
!insertmacro MUI_PAGE_DIRECTORY

# user option page: start menu shortcut
!insertmacro MUI_PAGE_STARTMENU Application $StartMenuFolder

# user option confirm/begin install
!insertmacro MUI_PAGE_INSTFILES

# uninstaller confirmation/options (no options)
!insertmacro MUI_UNPAGE_CONFIRM

# uninstaller begin
!insertmacro MUI_UNPAGE_INSTFILES

!insertmacro MUI_LANGUAGE "English"
Name Emacs-${EMACS_VERSION}

function .onInit
    StrCpy $INSTDIR "$PROGRAMFILES64\Emacs"
functionend

# main section logic, run after confirming installation
Section

  # insisting on installing shortcuts for "all users"
  # might ensure uninstall can remove shortcuts we created
  # SetShellVarContext all

  # extract program files
  SetOutPath $INSTDIR
  File /r emacs-${VERSION_BRANCH}

  # define uninstaller name
  StrCpy $UninstallerPath "$INSTDIR\Uninstall-${VERSION_BRANCH}.exe"

  # create uninstaller
  WriteUninstaller "$UninstallerPath"

  # request to set x64 registry keys
  SetRegView 64

  # add registry key to enable uninstall from control panel
  WriteRegStr HKLM "${UNINST_KEY}" "DisplayName" "GNU Emacs ${VERSION_BRANCH}"
  WriteRegStr HKLM "${UNINST_KEY}" "DisplayIcon" "$\"$UninstallerPath$\""
  WriteRegStr HKLM "${UNINST_KEY}" "DisplayVersion" "${VERSION_BRANCH}"
  WriteRegStr HKLM "${UNINST_KEY}" "Publisher" "Free Software Foundation, Inc."
  WriteRegStr HKLM "${UNINST_KEY}" "UninstallString" "$\"$UninstallerPath$\""

  ;Create shortcuts
  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
  CreateDirectory "$SMPROGRAMS\$StartMenuFolder"
  CreateShortcut "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk" \
      "$UninstallerPath"
  CreateShortCut "$SMPROGRAMS\$StartMenuFolder\Emacs.lnk" \
      "$INSTDIR\emacs-${VERSION_BRANCH}\bin\runemacs.exe"
  !insertmacro MUI_STARTMENU_WRITE_END
SectionEnd


# create a section to define what the uninstaller does.
# the section will always be named "Uninstall"
Section "Uninstall"

  # remove All Users shortcuts only
  # SetShellVarContext all

  # retrieve/recalculate uninstaller location
  StrCpy $UninstallerPath "$INSTDIR\Uninstall-${VERSION_BRANCH}.exe"

  # remove registry key
  DeleteRegKey HKLM "${UNINST_KEY}"

  # delete uninstaller
  Delete "$INSTDIR\Uninstall-${VERSION_BRANCH}.exe"

  # retrieve/recalculate startmenu shortcuts location
  !insertmacro MUI_STARTMENU_GETFOLDER Application $StartMenuFolder
  StrCpy $StartMenuFolder "$SMPROGRAMS\$StartMenuFolder"

  # remove Start Menu Program shortcuts
  Delete "$StartMenuFolder\Emacs.lnk"
  Delete "$StartMenuFolder\Uninstall.lnk"

  # remove empty startmenu parents up to $SMPROGRAMS
  startMenuDeleteLoop:
    ClearErrors
    RMDir $StartMenuFolder
    GetFullPathName $StartMenuFolder "$StartMenuFolder\.."
    IfErrors startMenuDeleteLoopDone
    StrCmp $StartMenuFolder $SMPROGRAMS startMenuDeleteLoopDone startMenuDeleteLoop

  # we're basically using GOTO, above, so we should get here..
  startMenuDeleteLoopDone:

  # next we remove stuff from program-files/installation path
  # start with recursive delete of the Emacs we installed
  RMDir /r "$INSTDIR\emacs-${VERSION_BRANCH}"

  # now walk parents of installation directory, deleting if empty
  instDirDeleteLoop:
    ClearErrors
    RMDir $INSTDIR
    GetFullPathName $INSTDIR "$INSTDIR\.."
    IfErrors instDirDeleteLoopDone
    StrCmp $INSTDIR $PROGRAMFILES64 instDirDeleteLoopDone instDirDeleteLoop

  # final clean-up (after removing from startmenu and progfiles)
  instDirDeleteLoopDone:

SectionEnd
