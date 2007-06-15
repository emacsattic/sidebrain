;;;; sidebrain-planner.el -- interface between planner and sidebrain
;;; Time-stamp: <2006-12-10 22:19:28 jcgs>

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

(defun planner-task-short-description (info)
  "Return a short description of INFO"
  ;; todo: it would be nice to strip all properties here
  (let* ((full (planner-task-description info))
	 (sep (string-match ":\\|{" full)))
    (if sep
	(substring full 0 (1- sep))
      full)))

(defun sidebrain-planner-task-marked (old-status new-status)
  "Update sidebrain state from planner."
  (save-window-excursion
    (let* ((task-info (planner-current-task-info))
	   (description (planner-task-short-description task-info)))
      ;; (message "new status for %s is %s" description new-status)
      (cond
       ((string= new-status "X")
	(sidebrain-end-task-stack t))
       ((string= new-status "P")
	(sidebrain-suspend-task t t))
       ((string= new-status "o")
	(let* ()
	  (sidebrain-set-task-triplet (list "planner"
					    (planner-task-plan task-info)
					    description)))))))
  t)

(add-hook 'planner-mark-task-hook 'sidebrain-planner-task-marked)

(defalias 'subtask 'sidebrain-begin-task)
(defalias 'supertask 'sidebrain-end-task)

(provide 'sidebrain-planner)

;;; end of sidebrain-planner.el
