C$Procedure      PRTPKG ( Declare Arguments for Error Message Routines )
 
      LOGICAL FUNCTION PRTPKG ( SHORT, LONG, EXPL, TRACE, DFAULT, TYPE )
 
C$ Abstract
C
C      Declare the arguments for the error message selection entry
C      points.  DO NOT CALL THIS ROUTINE.
C
C$ Required_Reading
C
C      ERROR
C
C$ Keywords
C
C      ERROR
C
C$ Declarations
 
       LOGICAL               SHORT
       LOGICAL               EXPL
       LOGICAL               LONG
       LOGICAL               TRACE
       LOGICAL               DFAULT
       CHARACTER*(*)         TYPE
 
       INTEGER               FILEN
       PARAMETER           ( FILEN = 128 )
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  ENTRY
C      --------  ---  --------------------------------------------------
C
C      SHORT      I   SETPRT
C      EXPL       I   SETPRT
C      LONG       I   SETPRT
C      TRACE      I   SETPRT
C      DFAULT     I   SETPRT
C      TYPE       I   MSGSEL
C      FILEN      P   MSGSEL
C
C$ Detailed_Input
C
C      See the ENTRY points for discussions of their arguments.
C
C$ Detailed_Output
C
C      See the ENTRY points for discussions of their arguments.
C
C$ Parameters
C
C      See the ENTRY points for discussions of their parameters.
C
C$ Exceptions
C
C      This routine signals an error IF IT IS CALLED.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      DO NOT CALL THIS ROUTINE.
C
C      The entry points declared in this routine are:
C
C      SETPRT
C      MSGSEL
C
C      There is no reason to call this subroutine.
C      The purpose of this subroutine is to make the
C      declarations required by the various entry points.
C      This routine has no run-time function.
C
C$ Examples
C
C      None.  DO NOT CALL THIS ROUTINE.
C
C$ Restrictions
C
C      DO NOT CALL THIS ROUTINE.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C
C$ Version
C
C-
C         Comment section for permuted index source lines was added
C         following the header.
C
C
C-&
 
C$ Index_Entries
C
C     None.
C
C-&
 
 
 
C$ Revisions
C
C
C-     Beta Version 1.0.1, 08-FEB-1989
C
C         PRTPKG, though it performs no run-time function, must
C         still return a value, in order to comply with the Fortran
C         standard.  So, now it does.
C
C-     Beta Version 1.0.1, 08-FEB-1989
C
C         Warnings added to discourage use of this routine.
C         Parameter declarations moved to "Declarations" section.
C         Two local declarations moved to the correct location.
C-&
 
 
 
C
C     SPICELIB functions
C
 
      LOGICAL               SETPRT
      LOGICAL               MSGSEL
 
C
C     Local variables:
C
      CHARACTER*(FILEN)     DEVICE
 
      CHARACTER*(10)        LTYPE
      CHARACTER*(10)        LOCTYP
 
C
C     Saved variables:
C
      LOGICAL               SVSHRT
      LOGICAL               SVEXPL
      LOGICAL               SVLONG
      LOGICAL               SVTRAC
      LOGICAL               SVDFLT
 
      SAVE                  SVSHRT
      SAVE                  SVEXPL
      SAVE                  SVLONG
      SAVE                  SVTRAC
      SAVE                  SVDFLT
 
C
C     Initial values:
C
      DATA     SVSHRT   / .TRUE. /
      DATA     SVEXPL   / .TRUE. /
      DATA     SVLONG   / .TRUE. /
      DATA     SVTRAC   / .TRUE. /
      DATA     SVDFLT   / .TRUE. /
 
C
C     Executable Code:
C
 
      CALL GETDEV ( DEVICE )
 
      CALL WRLINE ( DEVICE,
     .              'PRTPKG:  You have called an entry point which' //
     .              ' has no run-time function; this may indicate'  //
     .              ' a program bug.  Please check the PRTPKG'      //
     .              ' documentation.  ' )
 
      CALL WRLINE ( DEVICE, 'SPICE(BOGUSENTRY)' )
 
      PRTPKG = .FALSE.
 
      RETURN
 
 
 
 
 
 
C$Procedure      SETPRT ( Store Error Message Types to be Output )
 
C      ENTRY  BOGUS (X, Y, Z)

       ENTRY  SETPRT ( SHORT, EXPL, LONG, TRACE, DFAULT )
 
C$ Abstract
C
C      Store (a representation of) the selection of types of error
C      messages to be output.  DO NOT CALL THIS ROUTINE.
C
C$ Required_Reading
C
C      ERROR
C
C$ Keywords
C
C      ERROR
C
C$ Declarations
C
C      LOGICAL               SHORT
C      LOGICAL               EXPL
C      LOGICAL               LONG
C      LOGICAL               TRACE
C      LOGICAL               DFAULT
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C
C      SHORT      I   Select output of short error message?
C      EXPL       I   Select output of explanation of short message?
C      LONG       I   Select output of long error message?
C      TRACE      I   Select output of traceback?
C      DFAULT     I   Select output of default message?
C
C$ Detailed_Input
C
C      SHORT    indicates whether the short error message is selected
C               as one of the error messages to be output when an error
C               is detected.  A value of .TRUE. indicates that the
C               short error message IS selected.
C
C      EXPL     indicates whether the explanatory text for the short
C               error message is selected as one of the error messages
C               to be output when an error is detected.  A value of
C               .TRUE. indicates that the explanatory text for the
C               short error message IS selected.
C
C      LONG     indicates whether the long error message is selected
C               as one of the error messages to be output when an error
C               is detected.  A value of .TRUE. indicates that the
C               long error message IS selected.
C
C      TRACE    indicates whether the traceback is selected
C               as one of the error messages to be output when an error
C               is detected.  A value of .TRUE. indicates that the
C               traceback IS selected.
C
C      DFAULT   indicates whether the default message is selected
C               as one of the error messages to be output when an error
C               is detected.  A value of .TRUE. indicates that the
C               default message IS selected.
C
C
C$ Detailed_Output
C
C      None.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C      None.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      DO NOT CALL THIS ROUTINE.
C
C      The effect of this routine is an ENVIRONMENTAL one.  This
C      routine performs no output;  it stores the error message
C      selection provided as input.
C
C      Note that the actual output of error messages depends not
C      only on the selection made using this routine, but also
C      on the selection of the error output device (see ERRDEV)
C      and the choice of error response action (see ERRACT). If
C      the action is not 'IGNORE' (possible choices are
C      'IGNORE', 'ABORT', 'DEFAULT', 'REPORT', and 'RETURN'),
C      the selected error messages will be written to the chosen
C      output device when an error is detected.
C
C$ Examples
C
C      1.  In this example, the short and long messages are selected.
C
C      C
C      C     Select short and long error messages for output
C      C     (We don't examine the status returned because no
C      C     errors are detected by SETPRT):
C      C
C
C            STATUS = SETPRT ( .TRUE., .FALSE., .TRUE., .FALSE.,
C           .                  .FALSE.                          )
C
C
C
C$ Restrictions
C
C      DO NOT CALL THIS ROUTINE.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C
C$ Version
C
C-
C
C-&
 
C$ Index_Entries
C
C     None.
C
C-&
 
 
C$ Revisions
C
C-
C         Warnings added to discourage use of this routine in
C         non-error-handling code.  Parameters section added.
C
C-&
 
 
 
C
C     Executable Code:
C
 
 
      IF   (  SHORT  )    THEN
         SVSHRT = .TRUE.
      ELSE
         SVSHRT = .FALSE.
      END IF
 
 
 
      IF   (  EXPL   )    THEN
         SVEXPL = .TRUE.
      ELSE
         SVEXPL = .FALSE.
      END IF
 
 
 
      IF   (  LONG   )    THEN
         SVLONG = .TRUE.
      ELSE
         SVLONG = .FALSE.
      END IF
 
 
 
      IF   (  TRACE  )    THEN
         SVTRAC = .TRUE.
      ELSE
         SVTRAC = .FALSE.
      END IF
 
      IF   (  DFAULT  )    THEN
         SVDFLT = .TRUE.
      ELSE
         SVDFLT = .FALSE.
      END IF
 
 
C
C     We assign a value to SETPRT, but this value is
C     not meaningful...
C
      SETPRT = .TRUE.
 
 
      RETURN
 
 
 
 
C$Procedure      MSGSEL  ( Is This Message Type Selected for Output? )
 
       ENTRY  MSGSEL ( TYPE )
 
C$ Abstract
C
C      Indicate whether the specified message type has been selected
C      for output.
C
C$ Required_Reading
C
C      ERROR
C
C$ Keywords
C
C      ERROR
C
C$ Declarations
C
C      TYPE
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C
C      TYPE       I   Type of message whose selection status is queried.
C      FILEN      P   Maximum length of a file name.
C
C      The function takes the value .TRUE. if the message type indicated
C      by TYPE has been selected for output to the error output device.
C
C
C$ Detailed_Input
C
C      TYPE   Refers to a type of error message.  Possible values
C             are 'SHORT', 'EXPLAIN', 'LONG', 'DEFAULT',
C             and 'TRACEBACK'.
C
C$ Detailed_Output
C
C      The function takes the value .TRUE. if the message type indicated
C      by TYPE has been selected for output to the error output device.
C
C$ Parameters
C
C      FILEN  is the maximum length of a file name.
C
C$ Exceptions
C
C      Additionally, invalid values of TYPE are detected.
C
C      The short error message set in this case is:
C      'SPICE(INVALIDMSGTYPE)'
C
C      The handling of this error is a special case; to avoid recursion
C      problems, SIGERR is not called when the error is detected.
C      Instead, the short and long error messages are output directly.
C
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      This routine is part of the SPICELIB error handling mechanism.
C
C      Note that even though a given type of message may have been
C      selected for output, the output device and error response
C      action must also have been selected appropriately.
C      Use ERRDEV to choose the output device for error messages.
C      Use ERRACT to choose the error response action.  Any action
C      other than 'IGNORE' will result in error messages being
C      written to the error output device when errors are detected.
C      See ERRACT for details.
C
C$ Examples
C
C
C      1.  We want to know if the short message has been selected
C          for output:
C
C          C
C          C     Test whether the short message has been selected:
C          C
C
C                SELECT = MSGSEL ( 'SHORT' )
C
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C
C$ Version
C
C
C-&
 
C$ Index_Entries
C
C     None.
C
C-&
 
 
C$ Revisions
C
C
C         Parameters section added; parameter declaration added
C         to brief I/O section as well.
C
C-&
 
 
 
C
C     Executable Code:
C
 
      CALL LJUST ( TYPE,  LTYPE )
      CALL UCASE ( LTYPE, LTYPE )
 
 
      IF ( LTYPE  .EQ. 'SHORT' ) THEN
 
         MSGSEL = SVSHRT
 
      ELSE IF ( LTYPE  .EQ. 'EXPLAIN' ) THEN
 
         MSGSEL = SVEXPL
 
      ELSE IF ( LTYPE  .EQ. 'LONG' ) THEN
 
         MSGSEL = SVLONG
 
      ELSE IF ( LTYPE  .EQ. 'TRACEBACK' ) THEN
 
         MSGSEL = SVTRAC
 
      ELSE IF ( LTYPE  .EQ. 'DEFAULT' ) THEN
 
         MSGSEL = SVDFLT
 
      ELSE
 
C
C        Bad value of type!  We have a special case here; to
C        avoid recursion, we output the messages directly,
C        rather than call SIGERR.
C
 
         CALL GETDEV ( DEVICE )
 
         CALL WRLINE ( DEVICE,  'SPICE(INVALIDMSGTYPE)' )
 
         CALL WRLINE ( DEVICE, ' ' )
 
         LOCTYP = TYPE
 
C
C        Note:  What looks like a typo below isn't; there's
C        a line break after the substring 'specified' of
C        the "word" 'specifiedwas'.
C
 
         CALL WRLINE ( DEVICE,
 
     .   'MSGSEL:  An invalid error message type was supplied as'     //
     .   ' input; the type specifiedwas:  ' // LOCTYP
 
     .                )
 
 
      END IF
 
       subroutine 
     & intensity1(efv,fv,svin,svquad,sfpv,maxp,value,jndex,k,kj,jmod,isup)

       character*(*) function foo()

      END
