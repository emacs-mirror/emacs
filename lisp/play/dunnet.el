;;; dunnet.el --- Text adventure for Emacs

;; Author: Ron Schnell <ronnie@media.mit.edu>
;; Created: 25 Jul 1992
;; Version: 2.0
;; Keywords: games
;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This game can be run in batch mode.  To do this, use:
;;    emacs -batch -l dunnet.el

;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
;;;  The log file should be set for your system, and it must
;;;  be writeable by all.


      (setq log-file "/usr/local/dunscore")



(if nil
    (eval-and-compile (setq byte-compile-warnings nil)))

(require 'cl)

;;;; Mode definitions for interactive mode

(defun dungeon-mode ()
  "Major mode for running dungeon"
  (interactive)
  (text-mode)
  (use-local-map dungeon-mode-map)
  (setq major-mode 'dungeon-mode)
  (setq mode-name "Dungeon"))

(defun dungeon-parse (arg)
  "foo"
  (interactive "*p")
  (beginning-of-line)
  (setq beg (+ (point) 1))
  (end-of-line)
  (if (and (not (= beg (point))) (not (< (point) beg))
	   (string= ">" (buffer-substring (- beg 1) beg)))
      (progn
	(setq line (downcase (buffer-substring beg (point))))
	(princ line)
	(if (eq (parse ignore verblist line) -1)
	    (mprinc "I don't understand that.\n")))
    (goto-char (point-max))
    (mprinc "\n"))
    (dungeon-messages))
    
(defun dungeon-messages ()
  (if dead
      (text-mode)
    (if (eq dungeon-mode 'dungeon)
	(progn
	  (if (not (= room current-room))
	      (progn
		(describe-room current-room)
		(setq room current-room)))
	  (fix-screen)
	  (mprinc ">")))))


;;;###autoload
(defun dunnet ()
  "Switch to *dungeon* buffer and start game."
  (interactive)
  (switch-to-buffer "*dungeon*")
  (dungeon-mode)
  (setq dead nil)
  (setq room 0)
  (dungeon-messages))

;;;;
;;;; This section contains all of the verbs and commands.
;;;;

;;; Give long description of room if haven't been there yet.  Otherwise
;;; short.  Also give long if we were called with negative room number.

(defun describe-room (room)
  (if (and (not (member (abs room) light-rooms)) 
	   (not (member obj-lamp inventory)))
      (mprincl "It is pitch dark.  You are likely to be eaten by a grue.")
    (mprincl (cadr (nth (abs room) rooms)))
    (if (and (and (or (member room visited) 
		      (string= mode "superb")) (> room 0))
	     (not (string= mode "long")))
	nil
      (mprinc (car (nth (abs room) rooms)))
    (mprinc "\n"))
    (if (not (string= mode "long"))
	(if (not (member (abs room) visited))
	    (setq visited (append (list (abs room)) visited))))
    (dolist (xobjs (nth current-room room-objects))
      (if (= xobjs obj-special)
	  (special-object)
	(if (>= xobjs 0)
	    (mprincl (car (nth xobjs objects)))
	  (if (not (and (= xobjs obj-bus) inbus))
	      (progn
		(mprincl (car (nth (abs xobjs) perm-objects)))))))
      (if (and (= xobjs obj-jar) jar)
	  (progn
	    (mprincl "The jar contains:")
	    (dolist (x jar)
	      (mprinc "     ")
	      (mprincl (car (nth x objects)))))))
    (if (and (member obj-bus (nth current-room room-objects)) inbus)
	(mprincl "You are on the bus."))))

;;; There is a special object in the room.  This object's description,
;;; or lack thereof, depends on certain conditions.

(defun special-object ()
  (if (= current-room computer-room)
      (if computer
	  (mprincl 
"The panel lights are flashing in a seemingly organized pattern.")
	(mprincl "The panel lights are steady and motionless.")))

  (if (and (= current-room red-room) 
	   (not (member obj-towel (nth red-room room-objects))))
      (mprincl "There is a hole in the floor here."))

  (if (and (= current-room marine-life-area) black)
      (mprincl 
"The room is lit by a black light, causing the fish, and some of 
your objects, to give off an eerie glow."))
  (if (and (= current-room fourth-vermont-intersection) hole)
      (progn
	(mprincl"You fall into a hole in the ground.")
	(setq current-room vermont-station)
	(describe-room vermont-station)))

  (if (> current-room endgame-computer-room)
      (progn
	(if (not correct-answer)
	    (endgame-question)
	  (mprincl "Your question is:")
	  (mprincl endgame-question))))

  (if (= current-room sauna)
      (progn
	(mprincl (nth sauna-level '(
"It is normal room temperature in here."
"It is luke warm in here."
"It is comfortably hot in here."
"It is refreshingly hot in here."
"You are dead now.")))
	(if (and (= sauna-level 3) 
		 (or (member obj-rms inventory)
		     (member obj-rms (nth current-room room-objects))))
	    (progn
	      (mprincl 
"You notice the wax on your statuette beginning to melt, until it completely
melts off.  You are left with a beautiful diamond!")
	      (if (member obj-rms inventory)
		  (progn
		    (remove-obj-from-inven obj-rms)
		    (setq inventory (append inventory (list obj-diamond))))
		(remove-obj-from-room current-room obj-rms)
		(replace room-objects current-room
			 (append (nth current-room room-objects)
				 (list obj-diamond))))
	      (if (member obj-floppy inventory)
		  (progn
		    (mprincl
"You notice your floppy disk beginning to melt.  As you grab for it, the 
disk bursts into flames, and disintegrates.")
		    (remove-obj-from-inven obj-floppy)
		    (remove-obj-from-room current-room obj-floppy)))))))
)

(defun die (murderer)
  (mprinc "\n")
  (if murderer
      (mprincl "You are dead."))
  (do-logfile 'die murderer)
  (score nil)
  (setq dead t))

(defun quit (args)
  (die nil))

;;; Print every object in player's inventory.  Special case for the jar,
;;; as we must also print what is in it.

(defun inven (args)
  (mprinc "You currently have:")
  (mprinc "\n")
  (dolist (curobj inventory)
    (if curobj
	(progn
	  (mprincl (cadr (nth curobj objects)))
	  (if (and (= curobj obj-jar) jar)
	      (progn
		(mprincl "The jar contains:")
		(dolist (x jar)
		  (mprinc "     ")
		  (mprincl (cadr (nth x objects))))))))))

(defun shake (obj)
  (let (objnum)
    (when (setq objnum (objnum-from-args-std obj))
      (if (member objnum inventory)
	  (progn
;;;	If shaking anything will do anything, put here.
	    (mprinc "Shaking ")
	    (mprinc (downcase (cadr (nth objnum objects))))
	    (mprinc " seems to have no effect.")
	    (mprinc "\n")
	    )
	(if (and (not (member objnum (nth current-room room-silents)))
		 (not (member objnum (nth current-room room-objects))))
	    (mprincl "I don't see that here.")
;;;     Shaking trees can be deadly
	  (if (= objnum obj-tree)
	      (progn
		(mprinc
 "You begin to shake a tree, and notice a coconut begin to fall from the air.
As you try to get your hand up to block it, you feel the impact as it lands
on your head.")
		(die "a coconut"))
	    (if (= objnum obj-bear)
		(progn
		  (mprinc
"As you go up to the bear, it removes your head and places it on the ground.")
		  (die "a bear"))
	      (if (< objnum 0)
		  (mprincl "You cannot shake that.")
		(mprincl "You don't have that.")))))))))


(defun drop (obj)
  (if inbus
      (mprincl "You can't drop anything while on the bus.")
  (let (objnum ptr)
    (when (setq objnum (objnum-from-args-std obj))
      (if (not (setq ptr (member objnum inventory)))
	  (mprincl "You don't have that.")
	(progn
	  (remove-obj-from-inven objnum)
	  (replace room-objects current-room
		   (append (nth current-room room-objects)
			   (list objnum)))
	  (mprincl "Done.")
	  (if (member objnum (list obj-food obj-weight obj-jar))
	      (drop-check objnum))))))))

;;; Dropping certain things causes things to happen.

(defun drop-check (objnum)
  (if (and (= objnum obj-food) (= room bear-hangout)
	   (member obj-bear (nth bear-hangout room-objects)))
      (progn
	(mprincl
"The bear takes the food and runs away with it. He left something behind.")
	(remove-obj-from-room current-room obj-bear)
	(remove-obj-from-room current-room obj-food)
	(replace room-objects current-room
		 (append (nth current-room room-objects)
			 (list obj-key)))))

  (if (and (= objnum obj-jar) (member obj-nitric jar) 
	   (member obj-glycerine jar))
      (progn
	(mprincl "As the jar impacts the ground it explodes into many pieces.")
	(setq jar nil)
	(remove-obj-from-room current-room obj-jar)
	(if (= current-room fourth-vermont-intersection)
	    (progn
	      (setq hole t)
	      (setq current-room vermont-station)
	      (mprincl 
"The explosion causes a hole to open up in the ground, which you fall
through.")))))

  (if (and (= objnum obj-weight) (= current-room maze-button-room))
      (mprincl "A passageway opens.")))

;;; Give long description of current room, or an object.
      
(defun examine (obj)
  (let (objnum)
    (setq objnum (objnum-from-args obj))
    (if (eq objnum obj-special)
	(describe-room (* current-room -1))
      (if (and (eq objnum obj-computer)
	       (member obj-pc (nth current-room room-silents)))
	  (examine '("pc"))
	(if (eq objnum nil)
	    (mprincl "I don't know what that is.")
	  (if (and (not (member objnum (nth current-room room-objects)))
		   (not (member objnum (nth current-room room-silents)))
		   (not (member objnum inventory)))
	      (mprincl "I don't see that here.")
	    (if (>= objnum 0)
		(if (and (= objnum obj-bone) 
			 (= current-room marine-life-area) black)
		    (mprincl 
"In this light you can see some writing on the bone.  It says:
For an explosive time, go to Fourth St. and Vermont.")
		  (if (nth objnum physobj-desc)
		      (mprincl (nth objnum physobj-desc))
		    (mprincl "I see nothing special about that.")))
	      (if (nth (abs objnum) permobj-desc)
		  (progn
		    (mprincl (nth (abs objnum) permobj-desc)))
		(mprincl "I see nothing special about that.")))))))))

(defun take (obj)
  (if inbus
      (mprincl "You can't take anything while on the bus.")
  (setq obj (firstword obj))
  (if (not obj)
      (mprincl "You must supply an object.")
    (if (string= obj "all")
	(let (gotsome)
	  (setq gotsome nil)
	  (dolist (x (nth current-room room-objects))
	    (if (and (>= x 0) (not (= x obj-special)))
		(progn
		  (setq gotsome t)
		  (mprinc (cadr (nth x objects)))
		  (mprinc ": ")
		  (take-object x))))
	  (if (not gotsome)
	      (mprincl "Nothing to take.")))
      (let (objnum)
	(setq objnum (cdr (assq (intern obj) objnames)))
	(if (eq objnum nil)
	    (progn
	      (mprinc "I don't know what that is.")
	      (mprinc "\n"))
	  (take-object objnum)))))))

(defun take-object (objnum)
  (if (and (member objnum jar) (member obj-jar inventory))
      (let (newjar)
	(mprincl "You remove it from the jar.")
	(setq newjar nil)
	(dolist (x jar)
	  (if (not (= x objnum))
	      (setq newjar (append newjar (list x)))))
	(setq jar newjar)
	(setq inventory (append inventory (list objnum))))
    (if (not (member objnum (nth current-room room-objects)))
	(if (not (member objnum (nth current-room room-silents)))
	    (mprinc "I do not see that here.")
	  (try-take objnum))
      (if (>= objnum 0)
	  (progn
	    (if (and (car inventory) 
		     (> (+ (inven-weight) (nth objnum object-lbs)) 11))
		(mprinc "Your load would be too heavy.")
	      (setq inventory (append inventory (list objnum)))
	      (remove-obj-from-room current-room objnum)
	      (mprinc "Taken.  ")
	      (if (and (= objnum obj-towel) (= current-room red-room))
		  (mprinc "Taking the towel reveals a hole in the floor."))))
	(try-take objnum)))
    (mprinc "\n")))

(defun inven-weight ()
  (let (total)
    (setq total 0)
    (dolist (x jar)
      (setq total (+ total (nth x object-lbs))))
    (dolist (x inventory)
      (setq total (+ total (nth x object-lbs)))) total))

;;; We try to take an object that is untakable.  Print a message
;;; depending on what it is.

(defun try-take (obj)
  (mprinc "You cannot take that."))

(defun dig (args)
  (if inbus
      (mprincl "You can't dig while on the bus.")
  (if (not (member 0 inventory))
      (mprincl "You have nothing with which to dig.")
    (if (not (nth current-room diggables))
	(mprincl "Digging here reveals nothing.")
      (mprincl "I think you found something.")
      (replace room-objects current-room
	       (append (nth current-room room-objects)
		       (nth current-room diggables)))
      (replace diggables current-room nil)))))

(defun climb (obj)
  (let (objnum)
    (setq objnum (objnum-from-args obj))
    (if (and (not (= objnum obj-special))
	     (not (member objnum (nth current-room room-objects)))
	     (not (member objnum (nth current-room room-silents)))
	     (not (member objnum inventory)))
	(mprincl "I don't see that here.")
      (if (and (= objnum obj-special)
	       (not (member obj-tree (nth current-room room-silents))))
	  (mprincl "There is nothing here to climb.")
	(if (and (not (= objnum obj-tree)) (not (= objnum obj-special)))
	    (mprincl "You can't climb that.")
	  (mprincl
"You manage to get about two feet up the tree and fall back down.  You
notice that the tree is very unsteady."))))))

(defun eat (obj)
  (let (objnum)
    (when (setq objnum (objnum-from-args-std obj))
      (if (not (member objnum inventory))
	  (mprincl "You don't have that.")
	(if (not (= objnum obj-food))
	    (progn
	      (mprinc "You forcefully shove ")
	      (mprinc (downcase (cadr (nth objnum objects))))
	      (mprincl " down your throat, and start choking.")
	      (die "choking"))
	  (mprincl "That tasted horrible.")
	  (remove-obj-from-inven obj-food))))))

(defun dput (args)
  (if inbus
      (mprincl "You can't do that while on the bus")
    (let (newargs objnum objnum2 obj)
      (setq newargs (firstwordl args))
      (if (not newargs)
	  (mprincl "You must supply an object")
	(setq obj (intern (car newargs)))
	(setq objnum (cdr (assq obj objnames)))
	(if (not objnum)
	    (mprincl "I don't know what that object is.")
	  (if (not (member objnum inventory))
	      (mprincl "You don't have that.")
	    (setq newargs (firstwordl (cdr newargs)))
	    (setq newargs (firstwordl (cdr newargs)))
	    (if (not newargs)
		(mprincl "You must supply an indirect object.")
	      (setq objnum2 (cdr (assq (intern (car newargs)) objnames)))
	      (if (and (eq objnum2 obj-computer) (= current-room pc-area))
		  (setq objnum2 obj-pc))
	      (if (not objnum2)
		  (mprincl "I don't know what that indirect object is.")
		(if (and (not (member objnum2 (nth current-room room-objects)))
			 (not (member objnum2 (nth current-room room-silents)))
			 (not (member objnum2 inventory)))
		    (mprincl "That indirect object is not here.")
		  (put-objs objnum objnum2))))))))))

(defun put-objs (obj1 obj2)
  (if (and (= obj2 obj-drop) (not nomail))
      (setq obj2 obj-chute))

  (if (= obj2 obj-disposal) (setq obj2 obj-chute))

  (if (and (= obj1 obj-cpu) (= obj2 obj-computer))
      (progn
	(remove-obj-from-inven obj-cpu)
	(setq computer t)
	(mprincl
"As you put the CPU board in the computer, it immediately springs to life.
The lights start flashing, and the fans seem to startup."))
    (if (and (= obj1 obj-weight) (= obj2 obj-button))
	(drop '("weight"))
      (if (= obj2 obj-jar)                 ;; Put something in jar
	  (if (not (member obj1 (list obj-paper obj-diamond obj-emerald
				      obj-license obj-coins obj-egg
				      obj-nitric obj-glycerine)))
	      (mprincl "That will not fit in the jar.")
	    (remove-obj-from-inven obj1)
	    (setq jar (append jar (list obj1)))
	    (mprincl "Done."))
	(if (= obj2 obj-chute)                 ;; Put something in chute
	    (progn
	      (remove-obj-from-inven obj1)
	      (mprincl 
"You hear it slide down the chute and off into the distance.")
	      (put-objs-in-treas (list obj1)))
	  (if (= obj2 obj-box)              ;; Put key in key box
	      (if (= obj1 obj-key)
		  (progn
		    (mprincl
"As you drop the key, the box begins to shake.  Finally it explodes
with a bang.  The key seems to have vanished!")
		    (remove-obj-from-inven obj1)
		    (replace room-objects computer-room (append
							(nth computer-room
							     room-objects)
							(list obj1)))
		    (remove-obj-from-room current-room obj-box)
		    (setq key-level (1+ key-level)))
		(mprincl "You can't put that in the key box!"))

	    (if (and (= obj1 obj-floppy) (= obj2 obj-pc))
		(progn
		  (setq floppy t)
		  (remove-obj-from-inven obj1)
		  (mprincl "Done."))

	      (if (= obj2 obj-urinal)                   ;; Put object in urinal
		  (progn
		    (remove-obj-from-inven obj1)
		    (replace room-objects urinal (append 
						  (nth urinal room-objects)
						   (list obj1)))
		    (mprincl
		     "You hear it plop down in some water below."))
		(if (= obj2 obj-mail)
		    (mprincl "The mail chute is locked.")
		  (if (member obj1 inventory)
		      (mprincl 
"I don't know how to combine those objects.  Perhaps you should
just try dropping it.")
		    (mprincl"You can't put that there.")))))))))))

(defun type (args)
  (if (not (= current-room computer-room))
      (mprincl "There is nothing here on which you could type.")
    (if (not computer)
	(mprincl 
"You type on the keyboard, but your characters do not even echo.")
      (unix-interface))))

;;; Various movement directions

(defun n (args)
  (move north))

(defun s (args)
  (move south))

(defun e (args)
  (move east))

(defun w (args)
  (move west))

(defun ne (args)
  (move northeast))

(defun se (args)
  (move southeast))

(defun nw (args)
  (move northwest))

(defun sw (args)
  (move southwest))

(defun up (args)
  (move up))

(defun down (args)
  (move down))

(defun in (args)
  (move in))

(defun out (args)
  (move out))

(defun go (args)
  (if (or (not (car args)) 
	  (eq (doverb ignore verblist (car args) (cdr (cdr args))) -1))
      (mprinc "I don't understand where you want me to go.\n")))

;;; Uses the dungeon-map to figure out where we are going.  If the
;;; requested direction yields 255, we know something special is
;;; supposed to happen, or perhaps you can't go that way unless
;;; certain conditions are met.

(defun move (dir)
  (if (and (not (member current-room light-rooms)) 
	   (not (member obj-lamp inventory)))
      (progn
	(mprinc 
"You trip over a grue and fall into a pit and break every bone in your
body.")
	(die "a grue"))
    (let (newroom)
      (setq newroom (nth dir (nth current-room dungeon-map)))
      (if (eq newroom -1)
	  (mprinc "You can't go that way.\n")
	(if (eq newroom 255)
	    (special-move dir)
	  (setq room -1)
	  (setq lastdir dir)
	  (if inbus
	      (progn
		(if (or (< newroom 58) (> newroom 83))
		    (mprincl "The bus cannot go this way.")
		  (mprincl 
		   "The bus lurches ahead and comes to a screeching halt.")
		  (remove-obj-from-room current-room obj-bus)
		  (setq current-room newroom)
		  (replace room-objects newroom
			   (append (nth newroom room-objects)
				   (list obj-bus)))))
	    (setq current-room newroom)))))))

;;; Movement in this direction causes something special to happen if the
;;; right conditions exist.  It may be that you can't go this way unless
;;; you have a key, or a passage has been opened.

;;; coding note: Each check of the current room is on the same 'if' level,
;;; i.e. there aren't else's.  If two rooms next to each other have
;;; specials, and they are connected by specials, this could cause
;;; a problem.  Be careful when adding them to consider this, and
;;; perhaps use else's.

(defun special-move (dir)
  (if (= current-room building-front)
      (if (not (member obj-key inventory))
	  (mprincl "You don't have a key that can open this door.")
	(setq current-room old-building-hallway))
    (if (= current-room north-end-of-cave-passage)
	(let (combo)
	  (mprincl 
"You must type a 3 digit combination code to enter this room.")
	  (mprinc "Enter it here: ")
	  (setq combo (read-line))
	  (if (not batch-mode)
	      (mprinc "\n"))
	  (if (string= combo combination)
	      (setq current-room gamma-computing-center)
	    (mprincl "Sorry, that combination is incorrect."))))

    (if (= current-room bear-hangout)
	(if (member obj-bear (nth bear-hangout room-objects))
	    (progn
	      (mprinc 
"The bear is very annoyed that you would be so presumptuous as to try
and walk right by it.  He tells you so by tearing your head off.
")
	      (die "a bear"))
	  (mprincl "You can't go that way.")))

    (if (= current-room vermont-station)
	(progn
	  (mprincl
"As you board the train it immediately leaves the station.  It is a very
bumpy ride.  It is shaking from side to side, and up and down.  You
sit down in one of the chairs in order to be more comfortable.")
	  (mprincl
"\nFinally the train comes to a sudden stop, and the doors open, and some
force throws you out.  The train speeds away.\n")
	  (setq current-room museum-station)))

    (if (= current-room old-building-hallway)
	(if (and (member obj-key inventory)
		 (> key-level 0))
	    (setq current-room meadow)
	  (mprincl "You don't have a key that can open this door.")))

    (if (and (= current-room maze-button-room) (= dir northwest))
	(if (member obj-weight (nth maze-button-room room-objects))
	    (setq current-room 18)
	  (mprincl "You can't go that way.")))

    (if (and (= current-room maze-button-room) (= dir up))
	(if (member obj-weight (nth maze-button-room room-objects))
	    (mprincl "You can't go that way.")
	  (setq current-room weight-room)))

    (if (= current-room classroom)
	(mprincl "The door is locked."))

    (if (or (= current-room lakefront-north) (= current-room lakefront-south))
	(swim nil))

    (if (= current-room reception-area)
	(if (not (= sauna-level 3))
	    (setq current-room health-club-front)
	  (mprincl
"As you exit the building, you notice some flames coming out of one of the
windows.  Suddenly, the building explodes in a huge ball of fire.  The flames
engulf you, and you burn to death.")
	  (die "burning")))

    (if (= current-room red-room)
	(if (not (member obj-towel (nth red-room room-objects)))
	    (setq current-room long-n-s-hallway)
	  (mprincl "You can't go that way.")))

    (if (and (> dir down) (> current-room gamma-computing-center) 
	     (< current-room museum-lobby))
	(if (not (member obj-bus (nth current-room room-objects)))
	    (mprincl "You can't go that way.")
	  (if (= dir in)
	      (if (member obj-license inventory)
		  (progn
		    (mprincl "You board the bus and get in the driver's seat.")
		    (setq nomail t)
		    (setq inbus t))
		(mprincl "You are not licensed for this type of vehicle."))
	    (mprincl "You hop off the bus.")
	    (setq inbus nil)))
      (if (= current-room fifth-oaktree-intersection)
	  (if (not inbus)
	      (progn
		(mprincl "You fall down the cliff and land on your head.")
		(die "a cliff"))
	    (mprincl
"The bus flies off the cliff, and plunges to the bottom, where it explodes.")
	    (die "a bus accident")))
      (if (= current-room main-maple-intersection)
	  (progn
	    (if (not inbus)
		(mprincl "The gate will not open.")
	      (mprincl
"As the bus approaches, the gate opens and you drive through.")
	      (remove-obj-from-room main-maple-intersection obj-bus)
	      (replace room-objects museum-entrance 
		       (append (nth museum-entrance room-objects)
			       (list obj-bus)))
	      (setq current-room museum-entrance)))))
    (if (= current-room cave-entrance)
	(progn
	  (mprincl
"As you enter the room you hear a rumbling noise.  You look back to see
huge rocks sliding down from the ceiling, and blocking your way out.\n")
	  (setq current-room misty-room)))))

(defun long (args)
  (setq mode "long"))

(defun turn (obj)
  (let (objnum direction)
    (when (setq objnum (objnum-from-args-std obj))
      (if (not (or (member objnum (nth current-room room-objects))
		   (member objnum (nth current-room room-silents))))
	  (mprincl "I don't see that here.")
	(if (not (= objnum obj-dial))
	    (mprincl "You can't turn that.")
	  (setq direction (firstword (cdr obj)))
	  (if (or (not direction) 
		  (not (or (string= direction "clockwise")
			   (string= direction "counterclockwise"))))
	      (mprincl "You must indicate clockwise or counterclockwise.")
	    (if (string= direction "clockwise")
		(setq sauna-level (+ sauna-level 1))
	      (setq sauna-level (- sauna-level 1)))
	    
	    (if (< sauna-level 0)
		(progn
		  (mprincl 
		   "The dial will not turn further in that direction.")
		  (setq sauna-level 0))
	      (sauna-heat))))))))

(defun sauna-heat ()
  (if (= sauna-level 0)
      (mprincl "The termperature has returned to normal room termperature."))
  (if (= sauna-level 1)
      (mprincl "It is now luke warm in here.  You begin to sweat."))
  (if (= sauna-level 2)
      (mprincl "It is pretty hot in here.  It is still very comfortable."))
  (if (= sauna-level 3)
      (progn
	(mprincl 
"It is now very hot.  There is something very refreshing about this.")
	(if (or (member obj-rms inventory) 
		(member obj-rms (nth current-room room-objects)))
	    (progn
	      (mprincl 
"You notice the wax on your statuette beginning to melt, until it completely
melts off.  You are left with a beautiful diamond!")
	      (if (member obj-rms inventory)
		  (progn
		    (remove-obj-from-inven obj-rms)
		    (setq inventory (append inventory (list obj-diamond))))
		(remove-obj-from-room current-room obj-rms)
		(replace room-objects current-room
			 (append (nth current-room room-objects)
				 (list obj-diamond))))))
	(if (or (member obj-floppy inventory)
		(member obj-floppy (nth current-room room-objects)))
	    (progn
	      (mprincl
"You notice your floppy disk beginning to melt.  As you grab for it, the 
disk bursts into flames, and disintegrates.")
	      (if (member obj-floppy inventory)
		  (remove-obj-from-inven obj-floppy)
		(remove-obj-from-room current-room obj-floppy))))))

  (if (= sauna-level 4)
      (progn
	(mprincl 
"As the dial clicks into place, you immediately burst into flames.")
	(die "burning"))))

(defun press (obj)
  (let (objnum)
    (when (setq objnum (objnum-from-args-std obj))
      (if (not (or (member objnum (nth current-room room-objects))
		   (member objnum (nth current-room room-silents))))
	  (mprincl "I don't see that here.")
	(if (not (member objnum (list obj-button obj-switch)))
	    (progn
	      (mprinc "You can't ")
	      (mprinc (car line-list))
	      (mprincl " that."))
	  (if (= objnum obj-button)
	      (mprincl
"As you press the button, you notice a passageway open up, but
as you release it, the passageway closes."))
	  (if (= objnum obj-switch)
	      (if black
		  (progn
		    (mprincl "The button is now in the off position.")
		    (setq black nil))
		(mprincl "The button is now in the on position.")
		(setq black t))))))))

(defun swim (args)
  (if (not (member current-room (list lakefront-north lakefront-south)))
      (mprincl "I see no water!")
    (if (not (member obj-life inventory))
	(progn
	  (mprincl 
"You dive in the water, and at first notice it is quite cold.  You then
start to get used to it as you realize that you never really learned how
to swim.")
	  (die "drowning"))
      (if (= current-room lakefront-north)
	  (setq current-room lakefront-south)
	(setq current-room lakefront-north)))))


(defun score (args)
  (if (not endgame)
      (let (total)
	(setq total (reg-score))
	(mprinc "You have scored ")
	(mprinc total)
	(mprincl " out of a possible 90 points.") total)
    (mprinc "You have scored ")
    (mprinc (endgame-score))
    (mprincl " endgame points out of a possible 110.")
    (if (= (endgame-score) 110)
	(mprincl 
"\n\nCongratulations.  You have won.  The wizard password is 'moby'"))))

(defun help (args)
  (mprincl
"Welcome to dunnet (2.0), by Ron Schnell (ronnie@media.mit.edu).
Here is some useful information (read carefully because there are one
or more clues in here):
- If you have a key that can open a door, you do not need to explicitly
  open it.  You may just use 'in' or walk in the direction of the door.

- If you have a lamp, it is always lit.

- You will not get any points until you manage to get treasures to a certain
  place.  Simply finding the treasures is not good enough.  There is more
  than one way to get a treasure to the special place.  It is also
  important that the objects get to the special place *unharmed* and
  *untarnished*.  You can tell if you have successfully transported the
  object by looking at your score, as it changes immediately.  Note that
  an object can become harmed even after you have received points for it.
  If this happens, your score will decrease, and in many cases you can never
  get credit for it again.

- You can save your game with the 'save' command, and use restore it
  with the 'restore' command.

- There are no limits on lengths of object names.

- Directions are: north,south,east,west,northeast,southeast,northwest,
                  southwest,up,down,in,out.

- These can be abbreviated: n,s,e,w,ne,se,nw,sw,u,d,in,out.

- If you go down a hole in the floor without an aid such as a ladder,
  you probably won't be able to get back up the way you came, if at all.

- To run this game in batch mode (no emacs window), use:
     emacs -batch -l dunnet.el

If you have questions or comments, please contact ronnie@media.mit.edu."))

(defun flush (args)
  (if (not (= current-room bathroom))
      (mprincl "I see nothing to flush.")
    (mprincl "Whoooosh!!")
    (put-objs-in-treas (nth urinal room-objects))
    (replace room-objects urinal nil)))

(defun piss (args)
  (if (not (= current-room bathroom))
      (mprincl "You can't do that here, don't even bother trying.")
    (if (not gottago)
	(mprincl "I'm afraid you don't have to go now.")
      (mprincl "That was refreshing.")
      (setq gottago nil)
      (replace room-objects urinal (append (nth urinal room-objects)
					   (list obj-URINE))))))


(defun dsleep (args)
  (if (not (= current-room bedroom))
      (mprincl
"You try to go to sleep while standing up here, but can't seem to do it.")
    (setq gottago t)
    (mprincl
"As soon as you start to doze off you begin dreaming.  You see images of
workers digging caves, slaving in the humid heat.  Then you see yourself
as one of these workers.  While no one is looking, you leave the group
and walk into a room.  The room is bare except for a horseshoe
shaped piece of stone in the center.  You see yourself digging a hole in
the ground, then putting some kind of treasure in it, and filling the hole
with dirt again.  After this, you immediately wake up.")))

(defun break (obj)
  (let (objnum)
    (if (not (member obj-axe inventory))
	(mprincl "You have nothing you can use to break things.")
      (when (setq objnum (objnum-from-args-std obj))
	(if (member objnum inventory)
	    (progn
	      (mprincl
"You take the object in your hands and swing the axe.  Unfortunately, you miss
the object and slice off your hand.  You bleed to death.")
	      (die "an axe"))
	  (if (not (or (member objnum (nth current-room room-objects))
		       (member objnum (nth current-room room-silents))))
	      (mprincl "I don't see that here.")
	    (if (= objnum obj-cable)
		(progn
		  (mprincl 
"As you break the ethernet cable, everything starts to blur.  You collapse
for a moment, then straighten yourself up.
")
		  (replace room-objects gamma-computing-center
			   (append (nth gamma-computing-center room-objects)
				   inventory))
		  (if (member obj-key inventory)
		      (progn
			(setq inventory (list obj-key))
			(remove-obj-from-room gamma-computing-center obj-key))
		    (setq inventory nil))
		  (setq current-room computer-room)
		  (setq ethernet nil)
		  (mprincl "Connection closed.")
		  (unix-interface))
	      (if (< objnum 0)
		  (progn
		    (mprincl "Your axe shatters into a million pieces.")
		    (remove-obj-from-inven obj-axe))
		(mprincl "Your axe breaks it into a million pieces.")
		(remove-obj-from-room current-room objnum)))))))))

(defun drive (args)
  (if (not inbus)
      (mprincl "You cannot drive when you aren't in a vehicle.")
    (mprincl "To drive while you are in the bus, just give a direction.")))

(defun superb (args)
  (setq mode 'superb))

(defun reg-score ()
  (let (total)
    (setq total 0)
    (dolist (x (nth treasure-room room-objects))
      (setq total (+ total (nth x object-pts))))
    (if (member obj-URINE (nth treasure-room room-objects))
	(setq total 0)) total))

(defun endgame-score ()
  (let (total)
    (setq total 0)
    (dolist (x (nth endgame-treasure-room room-objects))
      (setq total (+ total (nth x object-pts)))) total))

(defun answer (args)
  (if (not correct-answer)
      (mprincl "I don't believe anyone asked you anything.")
    (setq args (car args))
    (if (not args)
	(mprincl "You must give the answer on the same line.")
      (if (members args correct-answer)
	  (progn
	    (mprincl "Correct.")
	    (if (= lastdir 0)
		(setq current-room (1+ current-room))
	      (setq current-room (- current-room 1)))
	    (setq correct-answer nil))
	(mprincl "That answer is incorrect.")))))

(defun endgame-question ()
(if (not endgame-questions)
    (progn
      (mprincl "Your question is:")
      (mprincl "No more questions, just do 'answer foo'.")
      (setq correct-answer '("foo")))
  (let (which i newques)
    (setq i 0)
    (setq newques nil)
    (setq which (% (abs (random)) (length endgame-questions)))
    (mprincl "Your question is:")
    (mprincl (setq endgame-question (car (nth which endgame-questions))))
    (setq correct-answer (cdr (nth which endgame-questions)))
    (while (< i which)
      (setq newques (append newques (list (nth i endgame-questions))))
      (setq i (1+ i)))
    (setq i (1+ which))
    (while (< i (length endgame-questions))
      (setq newques (append newques (list (nth i endgame-questions))))
      (setq i (1+ i)))
    (setq endgame-questions newques))))

(defun dun-power (args)
  (if (not (= current-room pc-area))
      (mprincl "That operation is not applicable here.")
    (if (not floppy)
	(dos-no-disk)
      (dos-interface))))

(defun touka (args)
  (setq current-room computer-room)
  (setq logged-in t)
  (setq computer t))

(defun dun-feed (args)
  (let (objnum)
    (when (setq objnum (objnum-from-args-std args))
      (if (and (= objnum obj-bear) 
	       (member obj-bear (nth current-room room-objects)))
	  (progn
	    (if (not (member obj-food inventory))
		(mprincl "You have nothing with which to feed it.")
	      (drop '("food"))))
	(if (not (or (member objnum (nth current-room room-objects))
		     (member objnum inventory)
		     (member objnum (nth current-room room-silents))))
	    (mprincl "I don't see that here.")
	  (mprincl "You cannot feed that."))))))


;;;;
;;;;  This section defines various utility functions used
;;;;  by dunnet.
;;;;


;;; Function which takes a verb and a list of other words.  Calls proper
;;; function associated with the verb, and passes along the other words.

(defun doverb (ignore verblist verb rest)
  (if (not verb)
      nil
    (if (member (intern verb) ignore)
	(if (not (car rest)) -1
	  (doverb ignore verblist (car rest) (cdr rest)))
      (if (not (cdr (assq (intern verb) verblist))) -1
	(setq numcmds (1+ numcmds))
	(eval (list (cdr (assq (intern verb) verblist)) (quote rest)))))))


;;; Function to take a string and change it into a list of lowercase words.

(defun listify-string (strin)
  (let (pos ret-list end-pos)
    (setq pos 0)
    (setq ret-list nil)
    (while (setq end-pos (string-match "[ ,:;]" (substring strin pos)))
      (setq end-pos (+ end-pos pos))
      (if (not (= end-pos pos))
	  (setq ret-list (append ret-list (list 
					   (downcase
					    (substring strin pos end-pos))))))
      (setq pos (+ end-pos 1))) ret-list))

(defun listify-string2 (strin)
  (let (pos ret-list end-pos)
    (setq pos 0)
    (setq ret-list nil)
    (while (setq end-pos (string-match " " (substring strin pos)))
      (setq end-pos (+ end-pos pos))
      (if (not (= end-pos pos))
	  (setq ret-list (append ret-list (list 
					   (downcase
					    (substring strin pos end-pos))))))
      (setq pos (+ end-pos 1))) ret-list))

(defun replace (list n number)
  (rplaca (nthcdr n list) number))


;;; Get the first non-ignored word from a list.

(defun firstword (list)
  (if (not (car list))
      nil
    (while (and list (member (intern (car list)) ignore))
      (setq list (cdr list)))
    (car list)))

(defun firstwordl (list)
  (if (not (car list))
      nil
    (while (and list (member (intern (car list)) ignore))
      (setq list (cdr list)))
    list))

;;; parse a line passed in as a string  Call the proper verb with the
;;; rest of the line passed in as a list.

(defun parse (ignore verblist line)
  (mprinc "\n")
  (setq line-list (listify-string (concat line " ")))
  (doverb ignore verblist (car line-list) (cdr line-list)))

(defun parse2 (ignore verblist line)
  (mprinc "\n")
  (setq line-list (listify-string2 (concat line " ")))
  (doverb ignore verblist (car line-list) (cdr line-list)))

;;; Read a line, in window mode

(defun read-line ()
  (let (line)
    (setq line (read-string ""))
    (mprinc line) line))

;;; Insert something into the window buffer

(defun minsert (string)
  (if (stringp string)
      (insert string)
    (insert (prin1-to-string string))))

;;; Print something out, in window mode

(defun mprinc (string)
  (if (stringp string)
      (insert string)
    (insert (prin1-to-string string))))

;;; In window mode, keep screen from jumping by keeping last line at
;;; the bottom of the screen.

(defun fix-screen ()
  (interactive)
  (forward-line (- 0 (- (window-height) 2 )))
  (set-window-start (selected-window) (point))
  (end-of-buffer))

;;; Insert something into the buffer, followed by newline.

(defun minsertl (string)
  (minsert string)
  (minsert "\n"))

;;; Print something, followed by a newline.

(defun mprincl (string)
  (mprinc string)
  (mprinc "\n"))

;;; Function which will get an object number given the list of
;;; words in the command, except for the verb.

(defun objnum-from-args (obj)
  (let (objnum)
    (setq obj (firstword obj))
    (if (not obj)
	obj-special
      (setq objnum (cdr (assq (intern obj) objnames))))))

(defun objnum-from-args-std (obj)
  (let (result)
  (if (eq (setq result (objnum-from-args obj)) obj-special)
      (mprincl "You must supply an object."))
  (if (eq result nil)
      (mprincl "I don't know what that is."))
  (if (eq result obj-special)
      nil
    result)))

;;; Take a short room description, and change spaces and slashes to dashes.

(defun space-to-hyphen (string)
  (let (space)
    (if (setq space (string-match "[ /]" string))
	(progn
	  (setq string (concat (substring string 0 space) "-"
			       (substring string (1+ space))))
	  (space-to-hyphen string))
      string)))

;;; Given a unix style pathname, build a list of path components (recursive)

(defun get-path (dirstring startlist)
  (let (slash pos)
    (if (= (length dirstring) 0)
	startlist
      (if (string= (substring dirstring 0 1) "/")
	  (get-path (substring dirstring 1) (append startlist (list "/")))
	(if (not (setq slash (string-match "/" dirstring)))
	    (append startlist (list dirstring))
	  (get-path (substring dirstring (1+ slash))
		    (append startlist
			    (list (substring dirstring 0 slash)))))))))


;;; Is a string a member of a string list?

(defun members (string string-list)
  (let (found)
    (setq found nil)
    (dolist (x string-list)
      (if (string= x string)
	  (setq found t))) found))

;;; Function to put objects in the treasure room.  Also prints current
;;; score to let user know he has scored.

(defun put-objs-in-treas (objlist)
  (let (oscore newscore)
    (setq oscore (reg-score))
    (replace room-objects 0 (append (nth 0 room-objects) objlist))
    (setq newscore (reg-score))
    (if (not (= oscore newscore))
	(score nil))))

;;; Load an encrypted file, and eval it.

(defun load-d (filename)
  (let (old-buffer result)
    (setq result t)
    (setq old-buffer (current-buffer))
    (switch-to-buffer (get-buffer-create "*loadc*"))
    (erase-buffer)
    (condition-case nil
	(insert-file-contents filename)
      (error (setq result nil)))
    (unless (not result)
      (condition-case nil
	  (dun-rot13)
	(error (yank)))
      (eval-current-buffer)
      (kill-buffer (current-buffer))
      (switch-to-buffer old-buffer))
    result))

;;; Rotate the globals file, and save it for later loading.

(defun compile-globals ()
  (let
    (switch-to-buffer (get-buffer-create "*compd*"))
    (erase-buffer)
    (insert-file-contents "dun-globals.el")
    (dun-rot13)
    (goto-char (point-min))
    (write-region 1 (point-max) "dun-globals.dat")
    (kill-buffer (current-buffer))))

;;; Functions to remove an object either from a room, or from inventory.

(defun remove-obj-from-room (room objnum)
  (let (newroom)
    (setq newroom nil)
    (dolist (x (nth room room-objects))
      (if (not (= x objnum))
	  (setq newroom (append newroom (list x)))))
    (rplaca (nthcdr room room-objects) newroom)))

(defun remove-obj-from-inven (objnum)
  (let (new-inven)
    (setq new-inven nil)
    (dolist (x inventory)
      (if (not (= x objnum))
	  (setq new-inven (append new-inven (list x)))))
    (setq inventory new-inven)))

;;; Find the global data file.

(defun get-glob-dat ()
  (let (result)
    (setq result nil)
    (dolist (x load-path)
	    (if (file-exists-p (concat x "/dun-globals.dat"))
		(setq result (concat x "/dun-globals.dat"))))
    result))

;;; rotate current buffer 13 characters

(let ((i 0) (lower "abcdefghijklmnopqrstuvwxyz") upper)
  (setq translate-table (make-vector 256 0))
  (while (< i 256)
    (aset translate-table i i)
    (setq i (1+ i)))
  (setq lower (concat lower lower))
  (setq upper (upcase lower))
  (setq i 0)
  (while (< i 26)
    (aset translate-table (+ ?a i) (aref lower (+ i 13)))
    (aset translate-table (+ ?A i) (aref upper (+ i 13)))
      (setq i (1+ i))))
  
(defun dun-rot13 ()
  (let (str len (i 0))
    (setq str (buffer-substring (point-min) (point-max)))
    (setq len (length str))
    (while (< i len)
      (aset str i (aref translate-table (aref str i)))
      (setq i (1+ i)))
    (erase-buffer)
    (insert str)))

;;;;
;;;; This section defines the globals that are used in dunnet.
;;;;
;;;; IMPORTANT
;;;; All globals which can change must be saved from 'save-game.  Add
;;;; all new globals to bottom of file.

(setq visited '(27))
(setq current-room 1)
(setq exitf nil)
(setq badcd nil)
(defvar dungeon-mode-map nil)
(setq dungeon-mode-map (make-sparse-keymap))
(define-key dungeon-mode-map "\r" 'dungeon-parse)
(defvar dungeon-batch-map (make-keymap))
(if (string= (substring emacs-version 0 2) "18")
    (let (n)
      (setq n 32)
      (while (< 0 (setq n (- n 1)))
	(aset dungeon-batch-map n 'dungeon-nil)))
  (let (n)
    (setq n 32)
    (while (< 0 (setq n (- n 1)))
      (aset (car (cdr dungeon-batch-map)) n 'dungeon-nil))))
(define-key dungeon-batch-map "\r" 'exit-minibuffer)
(define-key dungeon-batch-map "\n" 'exit-minibuffer)
(setq computer nil)
(setq floppy nil)
(setq door1 'locked)
(setq key-level 0)
(setq hole nil)
(setq correct-answer nil)
(setq lastdir 0)
(setq numsaves 0)
(setq jar nil)
(setq numcmds 0)
(setq wizard nil)
(setq endgame-question nil)
(setq logged-in nil)
(setq dungeon-mode 'dungeon)
(setq unix-verbs '((ls . ls) (ftp . ftp) (echo . echo) (exit . uexit)
		   (cd . dunnet-cd) (pwd . dunnet-pwd) (rlogin . rlogin)
		   (uncompress . uncompress) (cat . cat) (zippy . zippy)))

(setq dos-verbs '((dir . dos-dir) (type . dos-type) (exit . dos-exit)
		  (command . dos-spawn) (b: . dos-invd) (c: . dos-invd)
		  (a: . dos-nil)))


(setq batch-mode nil)

(setq cdpath "/usr/toukmond")
(setq cdroom -10)
(setq uncompressed nil)
(setq ethernet t)
(setq restricted '(room-objects dungeon-map rooms room-silents combination))
(setq path "/usr/toukmond")
(setq ftptype 'ascii)
(setq endgame nil)
(setq gottago t)
(setq black nil)

(setq rooms '(
	      (
"You are in the treasure room.  A door leads out to the north."
               "Treasure room"
	       )
	      (
"You are at a dead end of a dirt road.  The road goes to the east.
In the distance you can see that it will eventually fork off.  The
trees here are very tall royal palms, and they are spaced equidistant
from each other."
	       "Dead end"
	       )
	      (
"You are on the continuation of a dirt road.  There are more trees on
both sides of you.  The road continues to the east and west."
               "E/W Dirt road"
	       )
	      (
"You are at a fork of two passages, one to the northeast, and one to the
southeast.  The ground here seems very soft. You can also go back west."
               "Fork"
	       )
	      (
"You are on a northeast/southwest road."
               "NE/SW road"
	       )
	      (
"You are at the end of the road.  There is a building in front of you
to the northeast, and the road leads back to the southwest."
               "Building front"
	       )
	      (
"You are on a southeast/northwest road."
               "SE/NW road"
	       )
	      (
"You are standing at the end of a road.  A passage leads back to the
northwest."
               "Bear hangout"
	       )
	      (
"You are in the hallway of an old building.  There are rooms to the east
and west, and doors leading out to the north and south."
               "Old Building hallway"
	       )
	      (
"You are in a mailroom.  There are many bins where the mail is usually
kept.  The exit is to the west."
               "Mailroom"
	       )
	      (
"You are in a computer room.  It seems like most of the equipment has
been removed.  There is a VAX 11/780 in front of you, however, with
one of the cabinets wide open.  A sign on the front of the machine
says: This VAX is named 'pokey'.  To type on the console, use the
'type' command.  The exit is to the east."
               "Computer room"
	       )
	      (
"You are in a meadow in the back of an old building.  A small path leads
to the west, and a door leads to the south."
               "Meadow"
	       )
	      (
"You are in a round, stone room with a door to the east.  There
is a sign on the wall that reads: 'receiving room'."
               "Receiving room"
	       )
	      (
"You are at the south end of a hallway that leads to the north.  There
are rooms to the east and west."
               "Northbound Hallway"
	       )
	      (
"You are in a sauna.  There is nothing in the room except for a dial
on the wall.  A door leads out to west."
               "Sauna"
               )
	      (
"You are at the end of a north/south hallway.  You can go back to the south,
or off to a room to the east."
               "End of N/S Hallway"
	       )
	      (
"You are in an old weight room.  All of the equipment is either destroyed
or completely broken.  There is a door out to the west, and there is a ladder
leading down a hole in the floor."
               "Weight room"                 ;16
	       )
	      (
"You are in a maze of twisty little passages, all alike.
There is a button on the ground here."
               "Maze button room"
	       )
	      (
"You are in a maze of little twisty passages, all alike."
               "Maze"
	       )
	      (
"You are in a maze of thirsty little passages, all alike."
               "Maze"    ;19
	       )
	      (
"You are in a maze of twenty little passages, all alike."
               "Maze"
	       )
	      (
"You are in a daze of twisty little passages, all alike."
               "Maze"   ;21
	       )
	      (
"You are in a maze of twisty little cabbages, all alike."
               "Maze"   ;22
	       )
	      (
"You are in a reception area for a health and fitness center.  The place
appears to have been recently ransacked, and nothing is left.  There is
a door out to the south, and a crawlspace to the southeast."
               "Reception area"
	       )
	      (
"You are outside a large building to the north which used to be a health
and fitness center.  A road leads to the south."
               "Health Club front"
	       )
	      (
"You are at the north side of a lake.  On the other side you can see
a road which leads to a cave.  The water appears very deep."
               "Lakefront North"
	       )
	      (
"You are at the south side of a lake.  A road goes to the south."
               "Lakefront South"
	       )
	      (
"You are in a well-hidden area off to the side of a road.  Back to the
northeast through the brush you can see the bear hangout."
               "Hidden area"
	       )
	      (
"The entrance to a cave is to the south.  To the north, a road leads
towards a deep lake.  On the ground nearby there is a chute, with a sign
that says 'put treasures here for points'."
               "Cave Entrance"                      ;28
	       )
	      (
"You are in a misty, humid room carved into a mountain.
To the north is the remains of a rockslide.  To the east, a small
passage leads away into the darkness."              ;29
               "Misty Room"
	       )
	      (
"You are in an east/west passageway.  The walls here are made of
multicolored rock and are quite beautiful."
               "Cave E/W passage"                   ;30
	       )
	      (
"You are at the junction of two passages. One goes north/south, and
the other goes west."
               "N/S/W Junction"                     ;31
	       )
	      (
"You are at the north end of a north/south passageway.  There are stairs
leading down from here.  There is also a door leading west."
               "North end of cave passage"         ;32
	       )
	      (
"You are at the south end of a north/south passageway.  There is a hole
in the floor here, into which you could probably fit."
               "South end of cave passage"         ;33
	       )
	      (
"You are in what appears to be a worker's bedroom.  There is a queen-
sized bed in the middle of the room, and a painting hanging on the
wall.  A door leads to another room to the south, and stairways
lead up and down."
               "Bedroom"                          ;34
	       )
	      (
"You are in a bathroom built for workers in the cave.  There is a
urinal hanging on the wall, and some exposed pipes on the opposite
wall where a sink used to be.  To the north is a bedroom."
               "Bathroom"        ;35
	       )
	      (
"This is a marker for the urinal.  User will not see this, but it
is a room that can contain objects."
               "Urinal"          ;36
	       )
	      (
"You are at the northeast end of a northeast/southwest passageway.
Stairs lead up out of sight."
               "Ne end of ne/sw cave passage"       ;37
	       )
	      (
"You are at the junction of northeast/southwest and east/west passages."
               "Ne/sw-e/w junction"                      ;38
	       )
	      (
"You are at the southwest end of a northeast/southwest passageway."
               "Sw end of ne/sw cave passage"        ;39
	       )
	      (
"You are at the east end of an e/w passage.  There are stairs leading up
to a room above."
               "East end of e/w cave passage"    ;40
	       )
	      (
"You are at the west end of an e/w passage.  There is a hole on the ground
which leads down out of sight."
               "West end of e/w cave passage"    ;41
	       )
	      (
"You are in a room which is bare, except for a horseshoe shaped boulder
in the center.  Stairs lead down from here."     ;42
               "Horseshoe boulder room"
	       )
	      (
"You are in a room which is completely empty.  Doors lead out to the north
and east."
               "Empty room"                      ;43
	       )
	      (
"You are in an empty room.  Interestingly enough, the stones in this
room are painted blue.  Doors lead out to the east and south."  ;44
               "Blue room"
	       )
	      (
"You are in an empty room.  Interestingly enough, the stones in this
room are painted yellow.  Doors lead out to the south and west."    ;45
               "Yellow room"
	       )
	      (
"You are in an empty room.  Interestingly enough, the stones in this room
are painted red.  Doors lead out to the west and north."
               "Red room"                                 ;46
	       )
	      (
"You are in the middle of a long north/south hallway."     ;47
               "Long n/s hallway"
	       )
	      (
"You are 3/4 of the way towards the north end of a long north/south hallway."
               "3/4 north"                                ;48
	       )
	      (
"You are at the north end of a long north/south hallway.  There are stairs
leading upwards."
               "North end of long hallway"                 ;49
	       )
	      (
"You are 3/4 of the way towards the south end of a long north/south hallway."
               "3/4 south"                                 ;50
	       )
	      (
"You are at the south end of a long north/south hallway.  There is a hole
to the south."
               "South end of long hallway"                 ;51
	       )
	      (
"You are at a landing in a stairwell which continues up and down."
               "Stair landing"                             ;52
	       )
	      (
"You are at the continuation of an up/down staircase."
               "Up/down staircase"                         ;53
	       )
	      (
"You are at the top of a staircase leading down.  A crawlway leads off
to the northeast."
               "Top of staircase."                        ;54
	       )
	      (
"You are in a crawlway that leads northeast or southwest."
               "Ne crawlway"                              ;55
	       )
	      (
"You are in a small crawlspace.  There is a hole in the ground here, and
a small passage back to the southwest."
               "Small crawlspace"                         ;56
	       )
	      (
"You are in the Gamma Computing Center.  An IBM 3090/600s is whirring
away in here.  There is an ethernet cable coming out of one of the units,
and going through the ceiling.  There is no console here on which you
could type."
               "Gamma computing center"                   ;57
	       )
	      (
"You are near the remains of a post office.  There is a mail drop on the
face of the building, but you cannot see where it leads.  A path leads
back to the east, and a road leads to the north."
               "Post office"                             ;58
	       )
	      (
"You are at the intersection of Main Street and Maple Ave.  Main street
runs north and south, and Maple Ave runs east off into the distance.
If you look north and east you can see many intersections, but all of
the buildings that used to stand here are gone.  Nothing remains except
street signs.
There is a road to the northwest leading to a gate that guards a building."
               "Main-Maple intersection"                       ;59
	       )
	      (
"You are at the intersection of Main Street and the west end of Oaktree Ave."
               "Main-Oaktree intersection"   ;60
	       )
	      (
"You are at the intersection of Main Street and the west end of Vermont Ave."
               "Main-Vermont intersection"  ;61
	       )
	      (
"You are at the north end of Main Street at the west end of Sycamore Ave." ;62
               "Main-Sycamore intersection"
	       )
	      (
"You are at the south end of First Street at Maple Ave." ;63
               "First-Maple intersection"
	       )
	      (
"You are at the intersection of First Street and Oaktree Ave."  ;64
               "First-Oaktree intersection"
	       )
	      (
"You are at the intersection of First Street and Vermont Ave."  ;65
               "First-Vermont intersection"
	       )
	      (
"You are at the north end of First Street at Sycamore Ave."  ;66
               "First-Sycamore intersection"
	       )
	      (
"You are at the south end of Second Street at Maple Ave."  ;67
               "Second-Maple intersection"
	       )
	      (
"You are at the intersection of Second Street and Oaktree Ave."  ;68
               "Second-Oaktree intersection"
	       )
	      (
"You are at the intersection of Second Street and Vermont Ave."  ;69
               "Second-Vermont intersection"
	       )
	      (
"You are at the north end of Second Street at Sycamore Ave."  ;70
               "Second-Sycamore intersection"
	       )
	      (
"You are at the south end of Third Street at Maple Ave."  ;71
               "Third-Maple intersection"
	       )
	      (
"You are at the intersection of Third Street and Oaktree Ave."  ;72
               "Third-Oaktree intersection"
	       )
	      (
"You are at the intersection of Third Street and Vermont Ave."  ;73
               "Third-Vermont intersection"
	       )
	      (
"You are at the north end of Third Street at Sycamore Ave."  ;74
               "Third-Sycamore intersection"
	       )
	      (
"You are at the south end of Fourth Street at Maple Ave."  ;75
               "Fourth-Maple intersection"
	       )
	      (
"You are at the intersection of Fourth Street and Oaktree Ave."  ;76
               "Fourth-Oaktree intersection"
	       )
	      (
"You are at the intersection of Fourth Street and Vermont Ave."  ;77
               "Fourth-Vermont intersection"
	       )
	      (
"You are at the north end of Fourth Street at Sycamore Ave."  ;78
               "Fourth-Sycamore intersection"
	       )
	      (
"You are at the south end of Fifth Street at the east end of Maple Ave."  ;79
               "Fifth-Maple intersection"
	       )
	      (
"You are at the intersection of Fifth Street and the east end of Oaktree Ave.
There is a cliff off to the east."
               "Fifth-Oaktree intersection"  ;80
	       )
	      (
"You are at the intersection of Fifth Street and the east end of Vermont Ave."
               "Fifth-Vermont intersection"  ;81
	       )
	      (
"You are at the north end of Fifth Street and the east end of Sycamore Ave."
               "Fifth-Sycamore intersection"  ;82
	       )
	      (
"You are in front of the Museum of Natural History.  A door leads into
the building to the north, and a road leads to the southeast."
               "Museum entrance"                  ;83
	       )
	      (
"You are in the main lobby for the Museum of Natural History.  In the center
of the room is the huge skeleton of a dinosaur.  Doors lead out to the
south and east." 
               "Museum lobby"                     ;84
	       )
	      (
"You are in the geological display.  All of the objects that used to
be on display are missing.  There are rooms to the east, west, and 
north."
               "Geological display"               ;85
	       )
	      (
"You are in the marine life area.  The room is filled with fish tanks,
which are filled with dead fish that have apparently died due to
starvation.  Doors lead out to the south and east."
               "Marine life area"                   ;86
	       )
	      (
"You are in some sort of maintenance room for the museum.  There is a
switch on the wall labeled 'BL'.  There are doors to the west and north."
               "Maintenance room"                   ;87
	       )
	      (
"You are in a classroom where school children were taught about natural
history.  On the blackboard is written, 'No children allowed downstairs.'
There is a door to the east with an 'exit' sign on it.  There is another
door to the west."
               "Classroom"                          ;88
	       )
	      (
"You are at the Vermont St. subway station.  A train is sitting here waiting."
               "Vermont station"                    ;89
	       )
	      (
"You are at the Museum subway stop.  A passage leads off to the north."
               "Museum station"                     ;90
	       )
	      (
"You are in a north/south tunnel."
               "N/S tunnel"                          ;91
	       )
	      (
"You are at the north end of a north/south tunnel.  Stairs lead up and
down from here.  There is a garbage disposal here."
               "North end of n/s tunnel"             ;92
               )
	      (
"You are at the top of some stairs near the subway station.  There is
a door to the west."
               "Top of subway stairs"           ;93
	       )
	      (
"You are at the bottom of some stairs near the subway station.  There is
a room to the northeast."
               "Bottom of subway stairs"       ;94
	       )
	      (
"You are in another computer room.  There is a computer in here larger
than you have ever seen.  It has no manufacturers name on it, but it
does have a sign that says: This machine's name is 'endgame'.  The
exit is to the southwest.  There is no console here on which you could
type."
               "Endgame computer room"         ;95
	       )
	      (
"You are in a north/south hallway."
               "Endgame n/s hallway"           ;96
	       )
	      (
"You have reached a question room.  You must answer a question correctly in
order to get by.  Use the 'answer' command to answer the question."
               "Question room 1"              ;97
	       )
	      (
"You are in a north/south hallway."
               "Endgame n/s hallway"           ;98
	       )
	      (
"You are in a second question room."
               "Question room 2"               ;99
	       )
	      (
"You are in a north/south hallway."
               "Endgame n/s hallway"           ;100
	       )
	      (
"You are in a third question room."
               "Question room 3"               ;101
	       )
	      (
"You are in the endgame treasure room.  A door leads out to the north, and
a hallway leads to the south."
               "Endgame treasure room"         ;102
	       )
	      (
"You are in the winner's room.  A door leads back to the south."
               "Winner's room"                 ;103
	       )
	      (
"You have reached a dead end.  There is a PC on the floor here.  Above
it is a sign that reads:
          Type the 'reset' command to type on the PC. 
A hole leads north."
               "PC area"                       ;104
               )            
))

(setq light-rooms '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 24 25 26 27 28 58 59
		     60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76
		     77 78 79 80 81 82 83))

(setq verblist '((die . die) (ne . ne) (north . n) (south . s) (east . e)
		 (west . w) (u . up) (d . down) (i . inven)
		 (inventory . inven) (look . examine) (n . n) (s . s) (e . e)
		 (w . w) (se . se) (nw . nw) (sw . sw) (up . up) 
		 (down . down) (in . in) (out . out) (go . go) (drop . drop)
		 (southeast . se) (southwest . sw) (northeast . ne)
		 (northwest . nw) (save . save-game) (restore . restore)
		 (long . long) (dig . dig) (shake . shake) (wave . shake)
		 (examine . examine) (describe . examine) (climb . climb)
		 (eat . eat) (put . dput) (type . type) (insert . dput)
		 (score . score) (help . help) (quit . quit) (read . examine)
		 (verbose . long) (urinate . piss) (piss . piss)
		 (flush . flush) (sleep . dsleep) (lie . dsleep) (x . examine)
		 (break . break) (drive . drive) (board . in) (enter . in)
		 (turn . turn) (press . press) (push . press) (swim . swim)
		 (on . in) (off . out) (chop . break) (switch . press)
		 (cut . break) (exit . out) (leave . out) (reset . dun-power)
		 (flick . press) (superb . superb) (answer . answer)
		 (throw . drop) (l . examine) (take . take) (get . take)
		 (feed . dun-feed)))

(setq inbus nil)
(setq nomail nil)
(setq ignore '(the to at))
(setq mode 'moby)
(setq sauna-level 0)

(defconst north 0)
(defconst south 1)
(defconst east 2)
(defconst west 3)
(defconst northeast 4)
(defconst southeast 5)
(defconst northwest 6)
(defconst southwest 7)
(defconst up 8)
(defconst down 9)
(defconst in 10)
(defconst out 11)

(setq dungeon-map '(
;		      no  so  ea  we  ne  se  nw  sw  up  do  in  ot
		    ( 96  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;0
		    ( -1  -1   2  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;1
		    ( -1  -1   3   1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;2
		    ( -1  -1  -1   2   4   6  -1  -1  -1  -1  -1  -1 ) ;3
		    ( -1  -1  -1  -1   5  -1  -1   3  -1  -1  -1  -1 ) ;4
		    ( -1  -1  -1  -1  255 -1  -1   4  -1  -1  255 -1 ) ;5
		    ( -1  -1  -1  -1  -1   7   3  -1  -1  -1  -1  -1 ) ;6
		    ( -1  -1  -1  -1  -1  255  6  27  -1  -1  -1  -1 ) ;7
		    ( 255  5   9  10  -1  -1  -1   5  -1  -1  -1   5 ) ;8
		    ( -1  -1  -1   8  -1  -1  -1  -1  -1  -1  -1  -1 ) ;9
		    ( -1  -1   8  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;10
		    ( -1   8  -1  58  -1  -1  -1  -1  -1  -1  -1  -1 ) ;11
		    ( -1  -1  13  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;12
		    ( 15  -1  14  12  -1  -1  -1  -1  -1  -1  -1  -1 ) ;13
		    ( -1  -1  -1  13  -1  -1  -1  -1  -1  -1  -1  -1 ) ;14
		    ( -1  13  16  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;15
		    ( -1  -1  -1  15  -1  -1  -1  -1  -1  17  16  -1 ) ;16
		    ( -1  -1  17  17  17  17 255  17 255  17  -1  -1 ) ;17
		    ( 18  18  18  18  18  -1  18  18  19  18  -1  -1 ) ;18
		    ( -1  18  18  19  19  20  19  19  -1  18  -1  -1 ) ;19
		    ( -1  -1  -1  18  -1  -1  -1  -1  -1  21  -1  -1 ) ;20
		    ( -1  -1  -1  -1  -1  20  22  -1  -1  -1  -1  -1 ) ;21
		    ( 18  18  18  18  16  18  23  18  18  18  18  18 ) ;22
		    ( -1 255  -1  -1  -1  19  -1  -1  -1  -1  -1  -1 ) ;23
		    ( 23  25  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;24
		    ( 24 255  -1  -1  -1  -1  -1  -1  -1  -1 255  -1 ) ;25
		    (255  28  -1  -1  -1  -1  -1  -1  -1  -1 255  -1 ) ;26
		    ( -1  -1  -1  -1   7  -1  -1  -1  -1  -1  -1  -1 ) ;27
		    ( 26 255  -1  -1  -1  -1  -1  -1  -1  -1  255 -1 ) ;28
		    ( -1  -1  30  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;29
		    ( -1  -1  31  29  -1  -1  -1  -1  -1  -1  -1  -1 ) ;30
		    ( 32  33  -1  30  -1  -1  -1  -1  -1  -1  -1  -1 ) ;31
		    ( -1  31  -1  255 -1  -1  -1  -1  -1  34  -1  -1 ) ;32
		    ( 31  -1  -1  -1  -1  -1  -1  -1  -1  35  -1  -1 ) ;33
		    ( -1  35  -1  -1  -1  -1  -1  -1  32  37  -1  -1 ) ;34
		    ( 34  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;35
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;36
		    ( -1  -1  -1  -1  -1  -1  -1  38  34  -1  -1  -1 ) ;37
		    ( -1  -1  40  41  37  -1  -1  39  -1  -1  -1  -1 ) ;38
		    ( -1  -1  -1  -1  38  -1  -1  -1  -1  -1  -1  -1 ) ;39
		    ( -1  -1  -1  38  -1  -1  -1  -1  42  -1  -1  -1 ) ;40
		    ( -1  -1  38  -1  -1  -1  -1  -1  -1  43  -1  -1 ) ;41
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  40  -1  -1 ) ;42
		    ( 44  -1  46  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;43
		    ( -1  43  45  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;44
		    ( -1  46  -1  44  -1  -1  -1  -1  -1  -1  -1  -1 ) ;45
		    ( 45  -1  -1  43  -1  -1  -1  -1  -1  255 -1  -1 ) ;46
		    ( 48  50  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;47
		    ( 49  47  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;48
		    ( -1  48  -1  -1  -1  -1  -1  -1  52  -1  -1  -1 ) ;49
		    ( 47  51  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;50
		    ( 50  104 -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;51
		    ( -1  -1  -1  -1  -1  -1  -1  -1  53  49  -1  -1 ) ;52
		    ( -1  -1  -1  -1  -1  -1  -1  -1  54  52  -1  -1 ) ;53
		    ( -1  -1  -1  -1  55  -1  -1  -1  -1  53  -1  -1 ) ;54
		    ( -1  -1  -1  -1  56  -1  -1  54  -1  -1  -1  54 ) ;55
		    ( -1  -1  -1  -1  -1  -1  -1  55  -1  31  -1  -1 ) ;56
		    ( -1  -1  32  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;57
		    ( 59  -1  11  -1  -1  -1  -1  -1  -1  -1  255 255) ;58
		    ( 60  58  63  -1  -1  -1  255 -1  -1  -1  255 255) ;59
		    ( 61  59  64  -1  -1  -1  -1  -1  -1  -1  255 255) ;60
		    ( 62  60  65  -1  -1  -1  -1  -1  -1  -1  255 255) ;61
		    ( -1  61  66  -1  -1  -1  -1  -1  -1  -1  255 255) ;62
		    ( 64  -1  67  59  -1  -1  -1  -1  -1  -1  255 255) ;63
		    ( 65  63  68  60  -1  -1  -1  -1  -1  -1  255 255) ;64
		    ( 66  64  69  61  -1  -1  -1  -1  -1  -1  255 255) ;65
		    ( -1  65  70  62  -1  -1  -1  -1  -1  -1  255 255) ;66
		    ( 68  -1  71  63  -1  -1  -1  -1  -1  -1  255 255) ;67
		    ( 69  67  72  64  -1  -1  -1  -1  -1  -1  255 255) ;68
		    ( 70  68  73  65  -1  -1  -1  -1  -1  -1  255 255) ;69
		    ( -1  69  74  66  -1  -1  -1  -1  -1  -1  255 255) ;70
		    ( 72  -1  75  67  -1  -1  -1  -1  -1  -1  255 255) ;71
		    ( 73  71  76  68  -1  -1  -1  -1  -1  -1  255 255) ;72
		    ( 74  72  77  69  -1  -1  -1  -1  -1  -1  255 255) ;73
		    ( -1  73  78  70  -1  -1  -1  -1  -1  -1  255 255) ;74
		    ( 76  -1  79  71  -1  -1  -1  -1  -1  -1  255 255) ;75
		    ( 77  75  80  72  -1  -1  -1  -1  -1  -1  255 255) ;76
		    ( 78  76  81  73  -1  -1  -1  -1  -1  -1  255 255) ;77
		    ( -1  77  82  74  -1  -1  -1  -1  -1  -1  255 255) ;78
		    ( 80  -1  -1  75  -1  -1  -1  -1  -1  -1  255 255) ;79
		    ( 81  79  255 76  -1  -1  -1  -1  -1  -1  255 255) ;80
		    ( 82  80  -1  77  -1  -1  -1  -1  -1  -1  255 255) ;81
		    ( -1  81  -1  78  -1  -1  -1  -1  -1  -1  255 255) ;82
		    ( 84  -1  -1  -1  -1  59  -1  -1  -1  -1  255 255) ;83
		    ( -1  83  85  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;84
		    ( 86  -1  87  84  -1  -1  -1  -1  -1  -1  -1  -1 ) ;85
		    ( -1  85  88  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;86
		    ( 88  -1  -1  85  -1  -1  -1  -1  -1  -1  -1  -1 ) ;87
		    ( -1  87 255  86  -1  -1  -1  -1  -1  -1  -1  -1 ) ;88
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 255  -1 ) ;89
		    ( 91  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;90
		    ( 92  90  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;91
		    ( -1  91  -1  -1  -1  -1  -1  -1  93  94  -1  -1 ) ;92
		    ( -1  -1  -1  88  -1  -1  -1  -1  -1  92  -1  -1 ) ;93
		    ( -1  -1  -1  -1  95  -1  -1  -1  92  -1  -1  -1 ) ;94
		    ( -1  -1  -1  -1  -1  -1  -1  94  -1  -1  -1  -1 ) ;95
		    ( 97   0  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;96
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;97
		    ( 99  97  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;98
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;99
		    ( 101 99  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;100
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;101
		    ( 103 101 -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;102
		    ( -1  102 -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;103
		    ( 51  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;104
		    )
;		      no  so  ea  we  ne  se  nw  sw  up  do  in  ot
)


;;; How the user references *all* objects, permanent and regular.
(setq objnames '(
		 (shovel . 0) 
		 (lamp . 1)
		 (cpu . 2) (board . 2) (card . 2)
		 (food . 3) 
		 (key . 4) 
		 (paper . 5)
		 (rms . 6) (statue . 6) (statuette . 6)  (stallman . 6)
		 (diamond . 7)
		 (weight . 8)
		 (life . 9) (preserver . 9)
		 (bracelet . 10) (emerald . 10) 
		 (gold . 11)
		 (platinum . 12)
		 (towel . 13) (beach . 13)
		 (axe . 14)
		 (silver . 15)
		 (license . 16)
		 (coins . 17)
		 (egg . 18)
		 (jar . 19)
		 (bone . 20)
		 (acid . 21) (nitric . 21)
		 (glycerine . 22)
		 (ruby . 23)
		 (amethyst . 24) 
		 (mona . 25)
		 (bill . 26) 
		 (floppy . 27) (disk . 27)
		 
		 (boulder . -1)
		 (tree . -2) (trees . -2) 
		 (bear . -3)
		 (bin . -4) (bins . -4)
		 (cabinet . -5) (computer . -5) (vax . -5) (ibm . -5) 
		 (protoplasm . -6)
		 (dial . -7) 
		 (button . -8) 
		 (chute . -9) 
		 (painting . -10)
		 (bed . -11)
		 (urinal . -12)
		 (URINE . -13)
		 (pipes . -14) (pipe . -14) 
		 (box . -15) (slit . -15) 
		 (cable . -16) (ethernet . -16) 
		 (mail . -17) (drop . -17)
		 (bus . -18)
		 (gate . -19)
		 (cliff . -20) 
		 (skeleton . -21) (dinosaur . -21)
		 (fish . -22)
		 (tanks . -23)
		 (switch . -24)
		 (blackboard . -25)
		 (disposal . -26) (garbage . -26)
		 (ladder . -27)
		 (subway . -28) (train . -28) 
		 (pc . -29) (drive . -29)
))

(dolist (x objnames)
  (let (name)
    (setq name (concat "obj-" (prin1-to-string (car x))))
    (eval (list 'defconst (intern name) (cdr x)))))

(defconst obj-special 255)

;;; The initial setup of what objects are in each room.
;;; Regular objects have whole numbers lower than 255.
;;; Objects that cannot be taken but might move and are
;;; described during room description are negative.
;;; Stuff that is described and might change are 255, and are
;;; handled specially by 'describe-room. 

(setq room-objects (list nil 

        (list obj-shovel)                     ;; treasure-room
        (list obj-boulder)                    ;; dead-end
        nil nil nil
        (list obj-food)                       ;; se-nw-road
        (list obj-bear)                       ;; bear-hangout
        nil nil
        (list obj-special)                    ;; computer-room
        (list obj-lamp obj-license obj-silver);; meadow
        nil nil
        (list obj-special)                    ;; sauna
        nil 
        (list obj-weight obj-life)            ;; weight-room
        nil nil
        (list obj-rms obj-floppy)             ;; thirsty-maze
        nil nil nil nil nil nil nil 
        (list obj-emerald)                    ;; hidden-area
        nil
        (list obj-gold)                       ;; misty-room
        nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
        (list obj-towel obj-special)          ;; red-room
        nil nil nil nil nil
        (list obj-box)                        ;; stair-landing
        nil nil nil
        (list obj-axe)                        ;; smal-crawlspace
        nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
        nil nil nil nil nil
        (list obj-special)                    ;; fourth-vermont-intersection
        nil nil
        (list obj-coins)                      ;; fifth-oaktree-intersection
        nil
        (list obj-bus)                        ;; fifth-sycamore-intersection
        nil
        (list obj-bone)                       ;; museum-lobby
        nil
        (list obj-jar obj-special obj-ruby)   ;; marine-life-area
        (list obj-nitric)                     ;; maintenance-room
        (list obj-glycerine)                  ;; classroom
        nil nil nil nil nil
        (list obj-amethyst)                   ;; bottom-of-subway-stairs
        nil nil
        (list obj-special)                    ;; question-room-1
        nil
        (list obj-special)                    ;; question-room-2
        nil
        (list obj-special)                    ;; question-room-three
        nil
        (list obj-mona)                       ;; winner's-room
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil))

;;; These are objects in a room that are only described in the
;;; room description.  They are permanent.

(setq room-silents (list nil
        (list obj-tree)                        ;; dead-end
        (list obj-tree)                        ;; e-w-dirt-road
        nil nil nil nil nil nil
        (list obj-bin)                         ;; mailroom
        (list obj-computer)                    ;; computer-room
        nil nil nil
        (list obj-dial)                        ;; sauna
        nil
        (list obj-ladder)                      ;; weight-room
        (list obj-button obj-ladder)           ;; maze-button-room
        nil nil nil
        nil nil nil nil nil nil nil
        (list obj-chute)                       ;; cave-entrance
        nil nil nil nil nil
        (list obj-painting obj-bed)            ;; bedroom
        (list obj-urinal obj-pipes)            ;; bathroom
        nil nil nil nil nil nil
        (list obj-boulder)                     ;; horseshoe-boulder-room
        nil nil nil nil nil nil nil nil nil nil nil nil nil nil
        (list obj-computer obj-cable)          ;; gamma-computing-center
        (list obj-mail)                        ;; post-office
        (list obj-gate)                        ;; main-maple-intersection
        nil nil nil nil nil nil nil nil nil nil nil nil nil
        nil nil nil nil nil nil nil
        (list obj-cliff)                       ;; fifth-oaktree-intersection
        nil nil nil
        (list obj-dinosaur)                    ;; museum-lobby
        nil
        (list obj-fish obj-tanks)              ;; marine-life-area
        (list obj-switch)                      ;; maintenance-room
        (list obj-blackboard)                  ;; classroom
        (list obj-train)                       ;; vermont-station
        nil nil
        (list obj-disposal)                    ;; north-end-of-n-s-tunnel
        nil nil
        (list obj-computer)                    ;; endgame-computer-room
        nil nil nil nil nil nil nil nil 
	(list obj-pc)                          ;; pc-area
	nil nil nil nil nil nil
))
(setq inventory '(1))

;;; Descriptions of objects, as they appear in the room description, and
;;; the inventory.

(setq objects '(
		("There is a shovel here." "A shovel")                ;0
		("There is a lamp nearby." "A lamp")                  ;1
		("There is a CPU card here." "A computer board")      ;2
		("There is some food here." "Some food")              ;3
		("There is a shiny brass key here." "A brass key")    ;4
		("There is a slip of paper here." "A slip of paper")  ;5
		("There is a wax statuette of Richard Stallman here." ;6
		 "An RMS statuette")
		("There is a shimmering diamond here." "A diamond")   ;7
		("There is a 10 pound weight here." "A weight")       ;8
		("There is a life preserver here." "A life preserver");9
		("There is an emerald bracelet here." "A bracelet")   ;10
		("There is a gold bar here." "A gold bar")            ;11
		("There is a platinum bar here." "A platinum bar")    ;12
		("There is a beach towel on the ground here." "A beach towel")
		("There is an axe here." "An axe") ;14
		("There is a silver bar here." "A silver bar")  ;15
		("There is a bus driver's license here." "A license") ;16
		("There are some valuable coins here." "Some valuable coins")
		("There is a jewel-encrusted egg here." "A valuable egg") ;18
		("There is a glass jar here." "A glass jar") ;19
		("There is a dinosaur bone here." "A bone") ;20
		("There is a packet of nitric acid here." "Some nitric acid")
		("There is a packet of glycerine here." "Some glycerine") ;22
		("There is a valuable ruby here." "A ruby") ;23
		("There is a valuable amethyst here." "An amethyst") ;24
		("The Mona Lisa is here." "The Mona Lisa") ;25
		("There is a 100 dollar bill here." "A $100 bill") ;26
		("There is a floppy disk here." "A floppy disk") ;27
	       )
)

;;; Weight of objects

(setq object-lbs '(2 1 1 1 1 0 2 2 10 3 1 1 1 0 1 1 0 1 1 1 1 0 0 2 2 1 0 0))
(setq object-pts '(0 0 0 0 0 0 0 10 0 0 10 10 10 0 0 10 0 10 10 0 0 0 0 10 10 10 10 0))


;;; Unix representation of objects.
(setq objfiles '(
		 "shovel.o" "lamp.o" "cpu.o" "food.o" "key.o" "paper.o"
		 "rms.o" "diamond.o" "weight.o" "preserver.o" "bracelet.o"
		 "gold.o" "platinum.o" "towel.o" "axe.o" "silver.o" "license.o"
		 "coins.o" "egg.o" "jar.o" "bone.o" "nitric.o" "glycerine.o"
		 "ruby.o" "amethyst.o"
		 ))

;;; These are the descriptions for the negative numbered objects from
;;; room-objects

(setq perm-objects '(
		     nil
		     ("There is a large boulder here.")
		     nil
		     ("There is a ferocious bear here!")
		     nil
		     nil
		     ("There is a worthless pile of protoplasm here.")
		     nil
		     nil
		     nil
		     nil
		     nil
		     nil
		     ("There is a strange smell in this room.")
		     nil
		     (
"There is a box with a slit in it, bolted to the wall here."
                     )
		     nil
		     nil
		     ("There is a bus here.")
		     nil
		     nil
		     nil
))


;;; These are the descriptions the user gets when regular objects are
;;; examined.

(setq physobj-desc '(
"It is a normal shovel with a price tag attached that says $19.99."
"The lamp is hand-crafted by Geppetto."
"The CPU board has a VAX chip on it.  It seems to have
2 Megabytes of RAM onboard."
"It looks like some kind of meat.  Smells pretty bad."
nil
"The paper says: Don't forget to type 'help' for help.  Also, remember
this word: 'worms'"
"The statuette is of the likeness of Richard Stallman, the author of the
famous EMACS editor.  You notice that he is not wearing any shoes."
nil
"You observe that the weight is heavy."
"It says S. S. Minnow."
nil
nil
nil
"It has a picture of snoopy on it."
nil
nil
"It has your picture on it!"
"They are old coins from the 19th century."
"It is a valuable Fabrege egg."
"It is a a plain glass jar."
nil
nil
nil
nil
nil
                     )
)

;;; These are the descriptions the user gets when non-regular objects
;;; are examined.

(setq permobj-desc '(
		     nil
"It is just a boulder.  It cannot be moved."
"They are palm trees with a bountiful supply of coconuts in them."
"It looks like a grizzly to me."
"All of the bins are empty.  Looking closely you can see that there
are names written at the bottom of each bin, but most of them are
faded away so that you cannot read them.  You can only make out three
names:
                   Jeffrey Collier
                   Robert Toukmond
                   Thomas Stock
"
                      nil
"It is just a garbled mess."
"The dial points to a temperature scale which has long since faded away."
nil
nil
"It is a velvet painting of Elvis Presly.  It seems to be nailed to the
wall, and you cannot move it."
"It is a queen sized bed, with a very firm mattress."
"The urinal is very clean compared with everything else in the cave.  There
isn't even any rust.  Upon close examination you realize that the drain at the
bottom is missing, and there is just a large hole leading down the
pipes into nowhere.  The hole is too small for a person to fit in.  The 
flush handle is so clean that you can see your reflection in it."
nil
nil
"The box has a slit in the top of it, and on it, in sloppy handwriting, is
written: 'For key upgrade, put key in here.'"
nil
"It says 'express mail' on it."
"It is a 35 passenger bus with the company name 'mobytours' on it."
"It is a large metal gate that is too big to climb over."
"It is a HIGH cliff."
"Unfortunately you do not know enough about dinosaurs to tell very much about
it.  It is very big, though."
"The fish look like they were once quite beautiful."
nil
nil
nil
nil
"It is a normal ladder that is permanently attached to the hole."
"It is a passenger train that is ready to go."
"It is a personal computer that has only one floppy disk drive."
		    )
)

(setq diggables (list nil nil nil (list obj-cpu) nil nil nil nil nil nil nil
		  nil nil nil nil nil nil nil nil nil nil      ;11-20
		  nil nil nil nil nil nil nil nil nil nil      ;21-30
		  nil nil nil nil nil nil nil nil nil nil      ;31-40
		  nil (list obj-platinum) nil nil nil nil nil nil nil nil))

(setq scroll-step 2)
(setq room-shorts nil)
(dolist (x rooms)
  (setq room-shorts  
		     (append room-shorts (list (downcase (space-to-hyphen
							  (cadr x)))))))

(setq endgame-questions '(
			  (
"What is your password on the machine called 'pokey'?" "robert")
			  (
"What password did you use during anonymous ftp to gamma?" "foo")
			  (
"Excluding the endgame, how many places are there where you can put
treasures for points?" "4" "four")
			  (
"What is your login name on the 'endgame' machine?" "toukmond"
)
			  (
"What is the nearest whole dollar to the price of the shovel?" "20" "twenty")
			  (
"What is the name of the bus company serving the town?" "mobytours")
			  (
"Give either of the two last names in the mailroom, other than your own."
"collier" "stock")
			  (
"What cartoon character is on the towel?" "snoopy")
			  (
"What is the last name of the author of EMACS?" "stallman")
			  (
"How many megabytes of memory is on the CPU board for the Vax?" "2")
			  (
"Which street in town is named after a U.S. state?" "vermont")
			  (
"How many pounds did the weight weigh?" "ten" "10")
			  (
"Name the STREET which runs right over the subway stop." "fourth" "4" "4th")
			  (
"How many corners are there in town (excluding the one with the Post Office)?"
                  "24" "twentyfour" "twenty-four")
			  (
"What type of bear was hiding your key?" "grizzly")
			  (
"Name either of the two objects you found by digging." "cpu" "card" "vax"
"board" "platinum")
			  (
"What network protocol is used between pokey and gamma?" "tcp/ip" "ip" "tcp")
))

(let (a)
  (setq a 0)
  (dolist (x room-shorts)
    (eval (list 'defconst (intern x) a))
    (setq a (+ a 1))))



;;;;
;;;; This section defines the UNIX emulation functions for dunnet.
;;;;

(defun unix-parse (args)
  (interactive "*p")
  (beginning-of-line)
  (let (beg esign)
    (setq beg (+ (point) 2))
    (end-of-line)
    (if (and (not (= beg (point)))
	     (string= "$" (buffer-substring (- beg 2) (- beg 1))))
	(progn
	  (setq line (downcase (buffer-substring beg (point))))
	  (princ line)
	  (if (eq (parse2 nil unix-verbs line) -1)
	      (progn
		(if (setq esign (string-match "=" line))
		    (doassign line esign)		
		  (mprinc (car line-list))
		  (mprincl ": not found.")))))
      (goto-char (point-max))
      (mprinc "\n"))
    (if (eq dungeon-mode 'unix)
	(progn
	  (fix-screen)
	  (mprinc "$ ")))))

(defun doassign (line esign)
  (if (not wizard)
      (let (passwd)
	(mprinc "Enter wizard password: ")
	(setq passwd (read-line))
	(if (not batch-mode)
	    (mprinc "\n"))
	(if (string= passwd "moby")
	    (progn
	      (setq wizard t)
	      (doassign line esign))
	  (mprincl "Incorrect.")))

    (let (varname epoint afterq i value)
      (setq varname (substring line 0 esign))
      (if (not (setq epoint (string-match ")" line)))
	  (if (string= (substring line (1+ esign) (+ esign 2))
		       "\"")
	      (progn
		(setq afterq (substring line (+ esign 2)))
		(setq epoint (+
			      (string-match "\"" afterq)
			      (+ esign 3))))
	    
	    (if (not (setq epoint (string-match " " line)))
		(setq epoint (length line))))
	(setq epoint (1+ epoint))
	(while (and
		(not (= epoint (length line)))
		(setq i (string-match ")" (substring line epoint))))
	  (setq epoint (+ epoint i 1))))
      (setq value (substring line (1+ esign) epoint))
      (dungeon-eval varname value))))

(defun dungeon-eval (varname value)
  (let (eval-error)
    (switch-to-buffer (get-buffer-create "*dungeon-eval*"))
    (erase-buffer)
    (insert "(setq ")
    (insert varname)
    (insert " ")
    (insert value)
    (insert ")")
    (setq eval-error nil)
    (condition-case nil
	(eval-current-buffer)
      (error (setq eval-error t)))
    (kill-buffer (current-buffer))
    (switch-to-buffer "*dungeon*")
    (if eval-error
	(mprincl "Invalid syntax."))))
  

(defun unix-interface ()
  (login)
  (if logged-in
      (progn
	(setq dungeon-mode 'unix)
	(define-key dungeon-mode-map "\r" 'unix-parse)
	(mprinc "$ "))))



(defun login ()
  (let (tries username password)
    (setq tries 4)
    (while (and (not logged-in) (> (setq tries (- tries 1)) 0))
      (mprinc "\n\nUNIX System V, Release 2.2 (pokey)\n\nlogin: ")
      (setq username (read-line))
      (if (not batch-mode)
	  (mprinc "\n"))
      (mprinc "password: ")
      (setq password (read-line))
      (if (not batch-mode)
	  (mprinc "\n"))
      (if (or (not (string= username "toukmond"))
	      (not (string= password "robert")))
	  (mprincl "login incorrect")
	(setq logged-in t)
	(mprincl "
Welcome to Unix\n
Please clean up your directories.  The filesystem is getting full.
Our tcp/ip link to gamma is a little flakey, but seems to work.
The current version of ftp can only send files from the current
directory, and deletes them after they are sent!  Be careful.

Note: Restricted bourne shell in use.\n")))
  (setq dungeon-mode 'dungeon)))

(defun ls (args)
  (if (car args)
      (let (ocdpath ocdroom)
	(setq ocdpath cdpath)
	(setq ocdroom cdroom)
	(if (not (eq (dunnet-cd args) -2))
	    (ls nil))
	(setq cdpath ocdpath)
	(setq cdroom ocdroom))
    (if (= cdroom -10)
	(ls-inven))
    (if (= cdroom -2)
	(ls-rooms))
    (if (= cdroom -3)
	(ls-root))
    (if (= cdroom -4)
	(ls-usr))
    (if (> cdroom 0)
	(ls-room))))

(defun ls-root ()
  (mprincl "total 4
drwxr-xr-x  3 root     staff           512 Jan 1 1970 .
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 ..
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 usr
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 rooms"))

(defun ls-usr ()
  (mprincl "total 4
drwxr-xr-x  3 root     staff           512 Jan 1 1970 .
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 ..
drwxr-xr-x  3 toukmond restricted      512 Jan 1 1970 toukmond"))

(defun ls-rooms ()
  (mprincl "total 16
drwxr-xr-x  3 root     staff           512 Jan 1 1970 .
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 ..")
  (dolist (x visited)
    (mprinc
"drwxr-xr-x  3 root     staff           512 Jan 1 1970 ")
    (mprincl (nth x room-shorts))))

(defun ls-room ()
  (mprincl "total 4
drwxr-xr-x  3 root     staff           512 Jan 1 1970 .
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 ..
-rwxr-xr-x  3 root     staff          2048 Jan 1 1970 description")
  (dolist (x (nth cdroom room-objects))
    (if (and (>= x 0) (not (= x 255)))
	(progn
	  (mprinc "-rwxr-xr-x  1 toukmond restricted        0 Jan 1 1970 ")
	  (mprincl (nth x objfiles))))))

(defun ls-inven ()
  (mprinc "total 467
drwxr-xr-x  3 toukmond restricted      512 Jan 1 1970 .
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 ..")
  (dolist (x unix-verbs)
    (if (not (eq (car x) 'IMPOSSIBLE))
	(progn
	  (mprinc"
-rwxr-xr-x  1 toukmond restricted    10423 Jan 1 1970 ")
	  (mprinc (car x)))))
  (mprinc "\n")
  (if (not uncompressed)
      (mprincl
"-rwxr-xr-x  1 toukmond restricted        0 Jan 1 1970 paper.o.Z"))
  (dolist (x inventory)
    (mprinc 
"-rwxr-xr-x  1 toukmond restricted        0 Jan 1 1970 ")
    (mprincl (nth x objfiles))))

(defun echo (args)
  (let (nomore var)
    (setq nomore nil)
    (dolist (x args)
	    (if (not nomore)
		(progn
		  (if (not (string= (substring x 0 1) "$"))
		      (progn
			(mprinc x)
			(mprinc " "))
		    (setq var (intern (substring x 1)))
		    (if (not (boundp var))
			(mprinc " ")
		      (if (member var restricted)
			  (progn
			    (mprinc var)
			    (mprinc ": Permission denied")
			    (setq nomore t))
			(eval (list 'mprinc var))
			(mprinc " ")))))))
	    (mprinc "\n")))


(defun ftp (args)
  (let (host username passwd ident newlist)
    (if (not (car args))
	(mprincl "ftp: hostname required on command line.")
      (setq host (intern (car args)))
      (if (not (member host '(gamma endgame)))
	  (mprincl "ftp: Unknown host.")
	(if (eq host 'endgame)
	    (mprincl "ftp: connection to endgame not allowed")
	  (if (not ethernet)
	      (mprincl "ftp: host not responding.")
	    (mprincl "Connected to gamma. FTP ver 0.9 00:00:00 01/01/70")
	    (mprinc "Username: ")
	    (setq username (read-line))
	    (if (string= username "toukmond")
		(if batch-mode
		    (mprincl "toukmond ftp access not allowed.")
		  (mprincl "\ntoukmond ftp access not allowed."))
	      (if (string= username "anonymous")
		  (if batch-mode
		      (mprincl
		       "Guest login okay, send your user ident as password.")
		    (mprincl 
		     "\nGuest login okay, send your user ident as password."))
		(if batch-mode
		    (mprinc "Password required for ")
		  (mprinc "\nPassword required for "))
		(mprincl username))
	      (mprinc "Password: ")
	      (setq ident (read-line))
	      (if (not (string= username "anonymous"))
		  (if batch-mode
		      (mprincl "Login failed.")
		    (mprincl "\nLogin failed."))
		(if batch-mode
		   (mprincl "Guest login okay, user access restrictions apply.")
		  (mprincl "\nGuest login okay, user access restrictions apply."))
		(ftp-commands)
		(setq newlist 
'("What password did you use during anonymous ftp to gamma?"))
		(setq newlist (append newlist (list ident)))
		(rplaca (nthcdr 1 endgame-questions) newlist)))))))))
  
(defun ftp-commands ()
    (setq exitf nil)
    (let (line)
      (while (not exitf)
	(mprinc "ftp> ")
	(setq line (read-line))
	(if 
	    (eq
	     (parse2 nil 
		    '((type . ftptype) (binary . bin) (bin . bin) (send . send)
		      (put . send) (quit . ftpquit) (help . ftphelp)
		      (ascii . fascii)
		      ) line)
	     -1)
	    (mprincl "No such command.  Try help.")))
      (setq ftptype 'ascii)))

(defun ftptype (args)
  (if (not (car args))
      (mprincl "Usage: type [binary | ascii]")
    (setq args (intern (car args)))
    (if (eq args 'binary)
	(bin nil)
      (if (eq args 'ascii)
	  (fascii 'nil)
	(mprincl "Unknown type.")))))

(defun bin (args)
  (mprincl "Type set to binary.")
  (setq ftptype 'binary))

(defun fascii (args)
  (mprincl "Type set to ascii.")
  (setq ftptype 'ascii))

(defun ftpquit (args)
  (setq exitf t))

(defun send (args)
  (if (not (car args))
      (mprincl "Usage: send <filename>")
    (setq args (car args))
    (let (counter foo)
      (setq foo nil)
      (setq counter 0)

;;; User can send commands!  Stupid user.


      (if (assq (intern args) unix-verbs)
	  (progn
	    (rplaca (assq (intern args) unix-verbs) 'IMPOSSIBLE)
	    (mprinc "Sending ")
	    (mprinc ftptype)
	    (mprinc " file for ")
	    (mprincl args)
	    (mprincl "Transfer complete."))

	(dolist (x objfiles)
	  (if (string= args x)
	      (progn
		(if (not (member counter inventory))
		    (progn
		      (mprincl "No such file.")
		      (setq foo t))
		  (mprinc "Sending ")
		  (mprinc ftptype)
		  (mprinc " file for ")
		  (mprinc (downcase (cadr (nth counter objects))))
		  (mprincl ", (0 bytes)")
		  (if (not (eq ftptype 'binary))
		      (progn
			(if (not (member obj-protoplasm
					 (nth receiving-room room-objects)))
			    (replace room-objects receiving-room
				     (append (nth receiving-room room-objects)
					     (list obj-protoplasm))))
			(remove-obj-from-inven counter))
		    (remove-obj-from-inven counter)
		    (replace room-objects receiving-room
			     (append (nth receiving-room room-objects)
				     (list counter))))
		  (setq foo t)
		  (mprincl "Transfer complete."))))
	  (setq counter (+ 1 counter)))
	(if (not foo)
	    (mprincl "No such file."))))))

(defun ftphelp (args)
  (mprincl 
   "Possible commands are:\nsend    quit    type   ascii  binary   help"))

(defun uexit (args)
  (setq dungeon-mode 'dungeon)
  (mprincl "\nYou step back from the console.")
  (define-key dungeon-mode-map "\r" 'dungeon-parse)
  (if (not batch-mode)
      (dungeon-messages)))

(defun dunnet-pwd (args)
  (mprincl cdpath))

(defun uncompress (args)
  (if (not (car args))
      (mprincl "Usage: uncompress <filename>")
    (setq args (car args))
    (if (or uncompressed
	    (and (not (string= args "paper.o"))
		 (not (string= args "paper.o.z"))))
	(mprincl "Uncompress command failed.")
      (setq uncompressed t)
      (setq inventory (append inventory (list obj-paper))))))

(defun rlogin (args)
  (let (passwd)
    (if (not (car args))
	(mprincl "Usage: rlogin <hostname>")
      (setq args (car args))
      (if (string= args "endgame")
	  (rlogin-endgame)
	(if (not (string= args "gamma"))
	    (mprincl "No such host.")
	  (if (not ethernet)
	      (mprincl "Host not responding.")
	    (mprinc "Password: ")
	    (setq passwd (read-line))
	    (if (not (string= passwd "worms"))
		(mprincl "\nlogin incorrect")
	      (mprinc 
"\nYou begin to feel strange for a moment, and you lose your items."
)
	      (replace room-objects computer-room 
		       (append (nth computer-room room-objects) inventory))
	      (setq inventory nil)
	      (setq current-room receiving-room)
	      (uexit nil))))))))
  
(defun dunnet-cd (args)
  (let (tcdpath tcdroom path-elemants room-check)
    (if (not (car args))
	(mprincl "Usage: cd <path>")
      (setq tcdpath cdpath)
      (setq tcdroom cdroom)
      (setq badcd nil)
      (condition-case nil
	  (setq path-elements (get-path (car args) nil))
	(error (mprincl "Invalid path.")
	       (setq badcd t)))
      (dolist (pe path-elements)
	      (unless badcd
		      (if (not (string= pe "."))
			  (if (string= pe "..")
			      (progn
				(if (> tcdroom 0)                  ;In a room
				    (progn
				      (setq tcdpath "/rooms")
				      (setq tcdroom -2))
					;In /rooms,/usr,root
				  (if (or 
				       (= tcdroom -2) (= tcdroom -4) 
				       (= tcdroom -3))
				      (progn
					(setq tcdpath "/")
					(setq tcdroom -3))
				    (if (= tcdroom -10)       ;In /usr/toukmond
					(progn
					  (setq tcdpath "/usr")
					  (setq tcdroom -4))))))
			    (if (string= pe "/")
				(progn
				  (setq tcdpath "/")
				  (setq tcdroom -3))
			      (if (= tcdroom -4)
				  (if (string= pe "toukmond")
				      (progn
					(setq tcdpath "/usr/toukmond")
					(setq tcdroom -10))
				    (nosuchdir))
				(if (= tcdroom -10)
				    (nosuchdir)
				  (if (> tcdroom 0)
				      (nosuchdir)
				    (if (= tcdroom -3)
					(progn
					  (if (string= pe "rooms")
					      (progn
						(setq tcdpath "/rooms")
						(setq tcdroom -2))
					    (if (string= pe "usr")
						(progn
						  (setq tcdpath "/usr")
						  (setq tcdroom -4))
					      (nosuchdir))))
				      (if (= tcdroom -2)
					  (progn
					    (dolist (x visited)
						    (setq room-check 
							  (nth x room-shorts))
						    (if (string= room-check pe)
							(progn
							  (setq tcdpath 
						 (concat "/rooms/" room-check))
							  (setq tcdroom x))))
					    (if (= tcdroom -2)
						(nosuchdir)))))))))))))
      (if (not badcd)
	  (progn
	    (setq cdpath tcdpath)
	    (setq cdroom tcdroom)
	    0)
      -2))))

(defun nosuchdir ()
  (mprincl "No such directory.")
  (setq badcd t))

(defun cat (args)
  (let (doto checklist)
    (if (not (setq args (car args)))
	(mprincl "Usage: cat <ascii-file-name>")
      (if (string-match "/" args)
	  (mprincl "cat: only files in current directory allowed.")
	(if (and (> cdroom 0) (string= args "description"))
	    (mprincl (car (nth cdroom rooms)))
	  (if (setq doto (string-match "\\.o" args))
	      (progn
		(if (= cdroom -10)
		    (setq checklist inventory)
		  (setq checklist (nth cdroom room-objects)))
		(if (not (member (cdr 
				  (assq (intern 
					 (substring args 0 doto)) objnames))
				 checklist))
		    (mprincl "File not found.")
		  (mprincl "Ascii files only.")))
	    (if (assq (intern args) unix-verbs)
		(mprincl "Ascii files only.")
	      (mprincl "File not found."))))))))
  
(defun zippy (args)
  (mprincl (yow)))

(defun rlogin-endgame ()
  (if (not (= (score nil) 90))
      (mprincl "You have not achieved enough points to connect to endgame.")
    (mprincl"\nWelcome to the endgame.  You are a truly noble adventurer.")
    (setq current-room treasure-room)
    (setq endgame t)
    (replace room-objects endgame-treasure-room (list obj-bill))
    (uexit nil)))


(random t)
(setq tloc (+ 60 (% (abs (random)) 18)))
(replace room-objects tloc (append (nth tloc room-objects) (list 18)))
(setq tcomb (+ 100 (% (abs (random)) 899)))
(setq combination (prin1-to-string tcomb))

;;;;
;;;; This section defines the DOS emulation functions for dunnet
;;;;

(defun dos-parse (args)
  (interactive "*p")
  (beginning-of-line)
  (let (beg)
    (setq beg (+ (point) 3))
    (end-of-line)
    (if (not (= beg (point)))
	(let (line)
	  (setq line (downcase (buffer-substring beg (point))))
	  (princ line)
	  (if (eq (parse2 nil dos-verbs line) -1)
	      (progn
		(sleep-for 1)
		(mprincl "Bad command or file name"))))
      (goto-char (point-max))
      (mprinc "\n"))
    (if (eq dungeon-mode 'dos)
	(progn
	  (fix-screen)
	  (dos-prompt)))))

(defun dos-interface ()
  (dos-boot-msg)
  (setq dungeon-mode 'dos)
  (define-key dungeon-mode-map "\r" 'dos-parse)
  (dos-prompt))

(defun dos-type (args)
  (sleep-for 2)
  (if (setq args (car args))
      (if (string= args "foo.txt")
	  (dos-show-combination)
	(if (string= args "command.com")
	    (mprincl "Cannot type binary files")
	  (mprinc "File not found - ")
	  (mprincl (upcase args))))
    (mprincl "Must supply file name")))

(defun dos-invd (args)
  (sleep-for 1)
  (mprincl "Invalid drive specification"))

(defun dos-dir (args)
  (sleep-for 1)
  (if (or (not (setq args (car args))) (string= args "\\"))
      (mprincl "
 Volume in drive A is FOO        
 Volume Serial Number is 1A16-08C9
 Directory of A:\\

COMMAND  COM     47845 04-09-91   2:00a
FOO      TXT        40 01-20-93   1:01a
        2 file(s)      47845 bytes
                     1065280 bytes free
")
    (mprincl "
 Volume in drive A is FOO        
 Volume Serial Number is 1A16-08C9
 Directory of A:\\

File not found")))


(defun dos-prompt ()
  (mprinc "A> "))

(defun dos-boot-msg ()
  (sleep-for 3)
  (mprinc "Current time is ")
  (mprincl (substring (current-time-string) 12 20))
  (mprinc "Enter new time: ")
  (read-line)
  (if (not batch-mode)
      (mprinc "\n")))

(defun dos-spawn (args)
  (sleep-for 1)
  (mprincl "Cannot spawn subshell"))

(defun dos-exit (args)
  (setq dungeon-mode 'dungeon)
  (mprincl "\nYou power down the machine and step back.")
  (define-key dungeon-mode-map "\r" 'dungeon-parse)
  (if (not batch-mode)
      (dungeon-messages)))

(defun dos-no-disk ()
  (sleep-for 3)
  (mprincl "Boot sector not found"))


(defun dos-show-combination ()
  (sleep-for 2)
  (mprinc "\nThe combination is ")
  (mprinc combination)
  (mprinc ".\n"))

(defun dos-nil (args))


;;;;
;;;; This section defines the save and restore game functions for dunnet.
;;;;

(defun save-game (filename)
  (if (not (setq filename (car filename)))
      (mprincl "You must supply a filename for the save.")
    (if (file-exists-p filename)
	(delete-file filename))
    (setq numsaves (1+ numsaves))
    (make-save-buffer)
    (save-val "current-room")
    (save-val "computer")
    (save-val "door1")
    (save-val "combination")
    (save-val "visited")
    (save-val "diggables")
    (save-val "key-level")
    (save-val "floppy")
    (save-val "numsaves")
    (save-val "numcmds")
    (save-val "logged-in")
    (save-val "dungeon-mode")
    (save-val "jar")
    (save-val "lastdir")
    (save-val "black")
    (save-val "nomail")
    (save-val "unix-verbs")
    (save-val "hole")
    (save-val "uncompressed")
    (save-val "ethernet")
    (save-val "sauna-level")
    (save-val "room-objects")
    (save-val "room-silents")
    (save-val "inventory")
    (save-val "endgame-question")
    (save-val "endgame")
    (save-val "endgame-questions")
    (save-val "cdroom")
    (save-val "cdpath")
    (save-val "correct-answer")
    (save-val "inbus")
    (if (compile-save-out filename)
	(mprincl "Error saving to file.")
      (do-logfile 'save nil)
      (switch-to-buffer "*dungeon*")
      (princ "")
      (mprincl "Done."))))

(defun make-save-buffer ()
  (switch-to-buffer (get-buffer-create "*save-dungeon*"))
  (erase-buffer))

(defun compile-save-out (filename)
  (let (ferror)
    (setq ferror nil)
    (condition-case nil
	(dun-rot13)
      (error (setq ferror t)))
    (if (not ferror)
	(progn
	  (goto-char (point-min))))
    (condition-case nil
        (write-region 1 (point-max) filename nil 1)
        (error (setq ferror t)))
    (kill-buffer (current-buffer))
    ferror))
    

(defun save-val (varname)
  (let (value)
    (setq varname (intern varname))
    (setq value (eval varname))
    (minsert "(setq ")
    (minsert varname)
    (minsert " ")
    (if (or (listp value)
	    (symbolp value))
	(minsert "'"))
    (if (stringp value)
	(minsert "\""))
    (minsert value)
    (if (stringp value)
	(minsert "\""))
    (minsertl ")")))


(defun restore (args)
  (let (file)
    (if (not (setq file (car args)))
	(mprincl "You must supply a filename.")
      (if (not (load-d file))
	  (mprincl "Could not load restore file.")
	(mprincl "Done.")
	(setq room 0)))))


(defun do-logfile (type how)
  (let (ferror newscore)
    (setq ferror nil)
    (switch-to-buffer (get-buffer-create "*score*"))
    (erase-buffer)
    (condition-case nil
	(insert-file-contents log-file)
      (error (setq ferror t)))
    (unless ferror
	    (goto-char (point-max))
	    (minsert (current-time-string))
	    (minsert " ")
	    (minsert (user-login-name))
	    (minsert " ")
	    (if (eq type 'save)
		(minsert "saved ")
	      (if (= (endgame-score) 110)
		  (minsert "won ")
		(if (not how)
		    (minsert "quit ")
		  (minsert "killed by ")
		  (minsert how)
		  (minsert " "))))
	    (minsert "at ")
	    (minsert (cadr (nth (abs room) rooms)))
	    (minsert ". score: ")
	    (if (> (endgame-score) 0)
		(minsert (setq newscore (+ 90 (endgame-score))))
	      (minsert (setq newscore (reg-score))))
	    (minsert " saves: ")
	    (minsert numsaves)
	    (minsert " commands: ")
	    (minsert numcmds)
	    (minsert "\n")
	    (write-region 1 (point-max) log-file nil 1))
    (kill-buffer (current-buffer))))


;;;;
;;;; These are functions, and function re-definitions so that dungeon can
;;;; be run in batch mode.


(defun batch-mprinc (arg)
   (if (stringp arg)
       (send-string-to-terminal arg)
     (send-string-to-terminal (prin1-to-string arg))))


(defun batch-mprincl (arg)
   (if (stringp arg)
       (progn
           (send-string-to-terminal arg)
           (send-string-to-terminal "\n"))
     (send-string-to-terminal (prin1-to-string arg))
     (send-string-to-terminal "\n")))

(defun batch-parse (ignore verblist line)
  (setq line-list (listify-string (concat line " ")))
  (doverb ignore verblist (car line-list) (cdr line-list)))

(defun batch-parse2 (ignore verblist line)
  (setq line-list (listify-string2 (concat line " ")))
  (doverb ignore verblist (car line-list) (cdr line-list)))

(defun batch-read-line ()
  (read-from-minibuffer "" nil dungeon-batch-map))


(defun dungeon-batch-loop ()
  (setq dead nil)
  (setq room 0)
  (while (not dead)
    (if (eq dungeon-mode 'dungeon)
	(progn
	  (if (not (= room current-room))
	      (progn
		(describe-room current-room)
		(setq room current-room)))
	  (mprinc ">")
	  (setq line (downcase (read-line)))
	  (if (eq (parse ignore verblist line) -1)
	      (mprinc "I don't understand that.\n"))))))

(defun batch-dos-interface ()
  (dos-boot-msg)
  (setq dungeon-mode 'dos)
  (while (eq dungeon-mode 'dos)
    (dos-prompt)
    (setq line (downcase (read-line)))
    (if (eq (parse2 nil dos-verbs line) -1)
	(progn
	  (sleep-for 1)
	  (mprincl "Bad command or file name"))))
  (goto-char (point-max))
  (mprinc "\n"))

(defun batch-unix-interface ()
    (login)
    (if logged-in
	(progn
	  (setq dungeon-mode 'unix)
	  (while (eq dungeon-mode 'unix)
	    (mprinc "$ ")
	    (setq line (downcase (read-line)))
	    (if (eq (parse2 nil unix-verbs line) -1)
		(let (esign)
		  (if (setq esign (string-match "=" line))
		      (doassign line esign)		
		    (mprinc (car line-list))
		    (mprincl ": not found.")))))
	  (goto-char (point-max))
	  (mprinc "\n"))))

(defun dungeon-nil (arg)
  "noop"
  (interactive "*p"))

(defun batch-dungeon ()
  (load "dun-batch")
  (setq visited '(27))
  (mprinc "\n")
  (dungeon-batch-loop))

(unless (not noninteractive)
  (fset 'mprinc 'batch-mprinc)
  (fset 'mprincl 'batch-mprincl)
  (fset 'parse 'batch-parse)
  (fset 'parse2 'batch-parse2)
  (fset 'read-line 'batch-read-line)
  (fset 'dos-interface 'batch-dos-interface)
  (fset 'unix-interface 'batch-unix-interface)
  (mprinc "\n")
  (setq batch-mode t)
  (dungeon-batch-loop))

