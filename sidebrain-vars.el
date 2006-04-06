;;;; sidebrain-vars.el -- global variables for sidebrain
;;; Time-stamp: <2006-03-24 10:37:35 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'sidebrain-vars)

(defstruct sidebrain-task-stack
  "Sidebrain data relating to a particular task and its subtasks."
  tasks
  observations
  priority
  link-group
  link-project
  link-task
  )

(defstruct sidebrain-task
  "Sidebrain data relating to a particular task."
  text
  file
  line-number

  time-started
  time-ended
  time-this-time			; gets added sporadically to time-spent
  time-spent

  time-started-current
  time-ended-current
  time-current

  keystrokes

  subtasks
  suspensions

  abandoned

  display-start
  display-end
)

;;;; big global variables

(defvar sidebrain-project-groups nil
  "alist of project group names to project groups.
a group is an alist of projects.
this is the top-level data structure for sidebrain; the other globals point
to parts of this structure. the main such variables are:
  sidebrain-current-project-group
  sidebrain-current-project
  sidebrain-current-stack")

(defvar sidebrain-current-project-group nil
  "a pair representing the current project group.
the car is the name, and the cdr is the alist of project names to projects.
this pair is a member of sidebrain-project-groups.")

(defvar sidebrain-current-project nil
  "a pair representing the current project.
the car is the project name, and the cdr is the alist of task names to tasks.
this pair is a member of sidebrain-current-project-group.")

(defvar sidebrain-current-stack nil
  "the task (and subtasks) that the user is currently working on;
and also the observations.
these are stored collectively as cons of the stack name and a structure
type, sidebrain-task-stack.")

(defvar sidebrain-history nil
  "The history of completed tasks.")

(defvar sidebrain-suspension-history nil
  "The list of suspended tasks.")

(defvar sidebrain-record-task-hook nil
  "Functions to filter tasks before putting them on the history list.
These can modify their argument, or return nil to stop it going onto the history list.")

(defvar sidebrain-record-abandon-task-hook nil
  "Functions to filter tasks before putting them on the history list when the task is abandoned.
These can modify their argument, or return nil to stop it going onto the history list.")

(defgroup sidebrain
  nil
  "Customization for sidebrain.")

(defcustom sidebrain-save-after-commands t
  "Whether to save sidebrain data after every user-level operation that modifies it.
Nil means don't do this, t means do this every time, an integer means once every n times.")

(defvar sidebrain-potential-save-count 8
  "Counter for potential saves.")

(defun sidebrain-save-after-command ()
  "Return whether to save after this command.
Controlled by sidebrain-save-after-commands."
  (or (eq sidebrain-save-after-commands t)
      (and (integerp sidebrain-save-after-commands)
	   (zerop (% (setq sidebrain-potential-save-count
			   (1+ sidebrain-potential-save-count))
		     sidebrain-save-after-commands)))))

(defcustom sidebrain-auto-ask-info-gathering-results ".+\\?$"
  "If a string, when you end a task that that string matches as a pattern, you are asked to make an observation.
If not a string, should be nil.
This lets you use a convention of marking information-gathering tasks in a particular way --
the default is ending them in a question mark -- and getting prompted to store the result
of the investigative task.")

(defcustom sidebrain-clear-observations-on-emptying-stack 'ask
  "*Whether to clear the observations list on ending the last task.
If nil, this is never done; if t, it is always done; otherwise, the
user is prompted.")

(defcustom sidebrain-mark-done-comments t
  "*Whether to change \"todo\" comments to \"done\" when ending tasks generated from them.")

(defcustom sidebrain-edit-labels t
  "*Whether to let the user edit the label on suspending a task.")

(defcustom sidebrain-file-name "~/.sidebrain.xml"
  "File in which we save the sidebrain data, apart from the history -- see sidebrain-history-file-name for that.")

(defcustom sidebrain-history-file-name "~/.sidebrain-history.xml"
  "File in which we save the sidebrain history.")

(defvar sidebrain-nested-tasks-pattern "\\([^{]+\\) *\\({\\(.+\\)}\\)?$"
  "Pattern describing our notation for nested tasks.")

(defvar sidebrain-filename-save-hooks nil
  "Functions to modify filenames before writing them into sidebrain-file-name.
This lets you put in environment variables to substitute, etc, so you can make
the filenames relative to a project directory, for example, and have them make
sense on machines where the project directory is in a different place.")

(defvar sidebrain-filename-load-hooks nil
  "Functions to modify filenames after reading them from sidebrain-file-name.
This lets you substitute in environment variables, etc -- see sidebrain-filename-save-hooks.")

(defvar sidebrain-save-more-readable t
  "Whether to make the XML file more readable, rather than more compact.")

;; Chris Exton suggested that it would be good to record what the user
;; had been doing just before suspending a task, and play it back to
;; them when they resume that task. Rather than hard coding that
;; specific feature, I have put in this family of hooks, to allow
;; various such things to be added.

(defvar sidebrain-suspend-hook nil
  "Functions to be applied to the task stack structure on suspending the task.
You could save extra information here, using properties.")

(defvar sidebrain-resume-hook nil
  "Functions to be applied to the task stack structure on resuming the task.
You could use extra information stored as properties by sidebrain-suspend-hook functions.")

(defvar sidebrain-save-label-hook nil
  "Functions to be applied to the task stack label on saving the stack to file.
You could use this to save information stored by sidebrain-suspend-hook.")

(defvar sidebrain-load-label-hook nil
  "Functions to be applied on loading the stack from file.
Arguments are the task stack label and the XML structure from which it was created.
You could use this to restoreinformation stored by sidebrain-save-label-hook.")

;;; end of sidebrain-vars.el
