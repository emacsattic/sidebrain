;;;; sidebrain-display.el -- display sidebrain data
;;; Time-stamp: <2006-02-20 11:06:24 john>

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

(provide 'sidebrain-display)

(defvar sidebrain-buffer "*Task stack*"
 "The buffer for displaying the task stack.")

(defvar sidebrain-task-format "%s\n"
  "Format string for displaying tasks.")

(defvar sidebrain-observation-format "%s\n"
  "Format string for displaying observations.")

(defvar sidebrain-popup-frame nil
  "*Whether to use a popup frame for sidebrain.
It might be better to remove this variable, and couple of others, and
let the user set special-display-buffer-names to include
sidebrain-buffer.")

(defvar sidebrain-frame-parameters 
  '((width . 80) (height . 8)
    (auto-raise t)
    (top . 0)
    (left . 0)
    (menu-bar-lines 1)
    (tool-bar-lines 1)
    (title . "Sidebrain")
    (modeline))
  "*Parameters for popup frame for sidebrain.")

(defun sidebrain-make-visible ()
  "Make the task stack visible."
  (let ((display-buffer-reuse-frames sidebrain-popup-frame)
	;; could use special-display-buffer-names?
	(pop-up-frames sidebrain-popup-frame)
	(buffer (get-buffer sidebrain-buffer))
	(pop-up-frame-alist sidebrain-frame-parameters)
	(old-frame (selected-frame)))
    (message "in sidebrain-make-visible, old-frame=%S" old-frame)
    (display-buffer buffer t)
    (shrink-window-if-larger-than-buffer (get-buffer-window buffer))
    (when nil pop-up-frames
      (set-frame-height
       (selected-frame)			; use get-buffer-window and something else, to get the frame? this is getting the wrong one
       (count-lines (point-min) (point-max))))
    (message "in sidebrain-make-visible, frame is %S, old-frame is %S" (selected-frame) old-frame)
    (select-frame old-frame) ; todo: should I be using redirect-frame-focus instead?
    (message "Restored old frame, supposedly; now %S is current" (selected-frame))))

(defvar sidebrain-display-divider nil
  "Divider string for sidebrain display.")

(defun sidebrain-task-extra-text (task &optional update)
  "Return some extra text to display with TASK.
Set sidebrain-task-format to a format string that will display two strings, to use this."
  (when update
    (sidebrain-update-stopwatch task 'time-this-time 'time-started))
  (let ((subtasks (sidebrain-task-subtasks task))
	(suspensions (sidebrain-task-suspensions task)))
    (format "(%s so far, started %s)"
	    (informal-format-time (time-as-seconds (sidebrain-get-task-property task 'time-spent)))
	    (format-time-string "%y-%m-%e %H:%M" (sidebrain-get-task-property task 'time-started))
	    (if subtasks (format ", %S subtasks" subtasks) "")
	    (if suspensions (format ", %S suspensions" suspensions) ""))))

(setq sidebrain-task-format "%s\n")
(setq sidebrain-task-format "%s: %s\n")

(defun sidebrain-display-task (task &optional overwrite)
  "Display TASK."
  (when overwrite
    (delete-region (sidebrain-task-display-start task)
		   (sidebrain-task-display-end task))
    (goto-char (sidebrain-task-display-start task)))
  (setf (sidebrain-task-display-start task) (point-marker))
  (insert (format sidebrain-task-format
		  (sidebrain-task-text task)
		  (sidebrain-task-extra-text task t)))
  (setf (sidebrain-task-display-end task) (point-marker)))

;;;###autoload
(defun sidebrain-display ()
  "Display the current task stack, etc.
Creates the buffer as needed."
  (interactive)
  (let ((buffer (get-buffer-create sidebrain-buffer)))
    (set-buffer buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (sidebrain-task-list-mode)
    (when sidebrain-current-stack
      (dolist (task (sidebrain-task-stack))
	(sidebrain-display-task task)))
    (unless sidebrain-display-divider
      (setq sidebrain-display-divider 
	    (make-string
	     (1- (if (and sidebrain-popup-frame
			  (assoc 'width sidebrain-frame-parameters))
		     (cdr (assoc 'width sidebrain-frame-parameters))
		   (frame-width)))
	     ?-)))
    (insert sidebrain-display-divider)
    (insert "\n")
    (when sidebrain-current-stack
      (dolist (observation (sidebrain-observations))
	(insert (format sidebrain-observation-format observation))))
    (goto-char (point-min))
    (setq buffer-read-only t)
    (sidebrain-make-visible)))

;;;; major mode for task list

(defun sidebrain-require-buffer ()
  "Check that we are in the task list buffer."
  (unless (and (eq major-mode 'sidebrain-task-list)
	       (eq (current-buffer) (get-buffer sidebrain-buffer)))
    (error "%S only available in task list buffer" this-command)))  

(defun sidebrain-task-list-end-to-here ()
  "End all the tasks as far as point."
  (interactive)
  (sidebrain-require-buffer)
  (let ((line (1+ (count-lines (point-min) (point)))))
    (dotimes (i line)
      (sidebrain-end-task t))
    (sidebrain-display)))

(defvar sidebrain-task-mode-map (make-keymap "Sidebrain task list")
  "The keymap for sidebrain task list mode.")

(defun sidebrain-task-list-mode ()
  "Major mode for sidebrain task list display.
Commands available are:
\\{sidebrain-task-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sidebrain-task-list-mode
	mode-name "Sidebrain tasks"
	mode-line-format '("--Sidebrain: "
			   (:eval (or (car sidebrain-current-project-group)
				      "<no project group>"))
			   ":"
			   (:eval (or (car sidebrain-current-project)
				      "<no project>")))
	)
  (use-local-map sidebrain-task-mode-map)
  )

;;; end of sidebrain-display.el
