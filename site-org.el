;;; ~/.emacs.d/site-emacs/site-org.el --- Configurations for org-mode

;; requires
(require 'org-install)
(require 'org-habit)
(require 'auto-complete)
(auto-complete-mode t)

(add-to-list 'auto-mode-alist
	 '("\\.org$" . org-mode))

;;; misc document handling
(add-to-list 'auto-mode-alist
	     '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(setq org-modules '(org-habit
		    org-irc
		    org-inlinetask
		    org-info
		    org-id))
(setq org-agenda-files '("~/Dropbox/org"))

;; variables
(setq org-clock-persist t)
(setq org-clock-in-resume t)
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
(setq org-clock-into-drawer t)
(setq org-clock-out-when-done t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-persist-query-resume nil)
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
(setq org-clock-report-include-clocking-task t)
(setq bh/keep-clock-running nil)
(setq org-clock-idle-time 10)
(org-clock-persistence-insinuate)
(setq org-default-notes-file (expand-file-name "~/Dropbox/org/refile.org"))
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
				    ("STYLE_ALL" . "habit"))))
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
	      (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE"))))
(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-time-stamp-rounding-minutes (quote (1 1)))
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))
(setq org-agenda-log-mode-items (quote (state)))
(setq org-tag-alist (quote ((:startgroup)
			    ("@errand" . ?e)
			    ("@office" . ?o)
			    ("@home" . ?H)
			    (:endgroup)
			    ("PHONE" . ?p)
			    ("WAITING" . ?w)
			    ("HOLD" . ?h)
			    ("PERSONAL". ?P)
			    ("WORK" . ?W)
			    ("FIX" . ?f)
			    ("IDEA" . ?i)
			    ("MEETINGS" . ?M)
			    ("ORG" . ?O)
			    ("SOCRATA" . ?S)
			    ("NOTE" . ?n)
			    ("CANCELLED" . ?c)
			    ("FLAGGED" . ??))))
(setq org-fast-tag-selection-single-key (quote expert))
(setq org-agenda-tags-todo-honor-ignore-options t)

;; triggers
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
	      ("WAITING" ("WAITING" . t))
	      ("HOLD" ("WAITING" . t) ("HOLD" . t))
	      (done ("WAITING") ("HOLD"))
	      ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
	      ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
	      ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Dropbox/org/refile.org")
	       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t :empty-lines 1)
	      ("m" "meeting/interrupt" entry (file "~/Dropbox/org/refile.org")
	       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resumne t :empty-lines 1)
	      ("r" "respond" entry (file "~/Dropbox/org/refile.org")
	       "* TODO Respond to %:from on %:subject\n%U\n%a\n"
	       :clock-in t :clock-resume t :immediate-finish t :empty-lines 1)
	      ("n" "note" entry (file "~/Dropbox/org/refile.org")
	       "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t :empty-lines 1)
	      ("j" "Journal" entry (file+datetree "~/Dropbox/org/log.org")
	       "* %?\n%U\n" :clock-in t :clock-resume t :empty-lines 1)
	      ("w" "org-protocol" entry (file "~/Dropbox/org/refile.org")
	       "* TODO Review %c\n%U\n" :immediate-finish t :empty-lines 1)
	      ("p" "Phone call" entry (file "~/Dropbox/org/refile.org")
	       "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t :empty-lines 1)
	      ("h" "Habit" entry (file "~/Dropbox/org/refile.org")
	       "* NEXT %?\n%U\n%a\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
	      ("b" "Blog Entry" entry (file+datetree "~/Dropbox/org/sulci/blog.org")
	       "* %^{Title} :DRAFT:blog:noexport:\n:PROPERTIES:\n:on: %T\n:END:\n%?"
	       :clock-in t :clock-resume t :empty-lines 1))))
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
				 (org-agenda-files :maxlevel . 9))))
; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)
; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)
; Allow refile to create parent tasks with confirmation
(setq org-file-allow-creating-parent-nodes (quote confirm))
; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
(setq org-refile-target-verify-function 'bh/verify-refile-target)

;;;; key bindings
;; global
(global-set-key (kbd "C-c l") 'org-store-link)
(global-unset-key (kbd "s-A"))
(global-set-key (kbd "s-A") 'org-agenda)
(global-set-key (kbd "s-r") 'org-capture)
(global-unset-key (kbd "s-o"))
(global-set-key (kbd "s-o") 'bh/punch-out)
(global-set-key (kbd "s-;") 'org-clock-in-last)
(global-set-key (kbd "s-I") 'org-clock-goto)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; org-mode only
(define-key org-mode-map (kbd "s-n") 'org-add-note)
;; not sure if i'm going to use this key over the punch-in function
;(define-key org-mode-map (kbd "s-i") 'org-clock-in)
(define-key org-mode-map (kbd "s-i") 'bh/punch-in)
(define-key org-mode-map (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(define-key org-mode-map (kbd "<f9> T") 'tabify)
(define-key org-mode-map (kbd "<f9> U") 'untabify)
(define-key org-mode-map (kbd "C-x n r") 'narrow-to-region)
(define-key org-mode-map (kbd "s-a") 'org-archive-subtree)
(define-key org-mode-map (kbd "C-s-t") 'org-todo)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("N" "Notes" tags "NOTE"
	 ((org-agenda-overriding-header "Notes")
	  (org-tags-match-list-sublevels t)))
	("h" "Habits" tags-todo "STYLE=\"habit\""
	 ((org-agenda-overriding-header "Habits")
	  (org-agenda-sorting-strategy
	   '(todo-state-down effort-up category-keep))))
	(" " "Agenda"
	 ((agenda "")
	  (tags "REFILE"
		((org-agenda-overriding-header "Tasks to refile")
		 (org-tags-match-list-sublevels nil)))
	  (tags-todo "-CANCELLED/!"
		     ((org-agenda-overriding-header "Stuck Projects")
		      (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
	  (tags-todo "-WAITING-CANCELLED/!NEXT"
		     ((org-agenda-overriding-header "Next Tasks")
;                      (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
		      (org-agenda-todo-ignore-scheduled t)
		      (org-agenda-todo-ignore-deadlines t)
		      (org-agenda-todo-ignore-with-date t)
		      (org-tags-match-list-sublevels t)
		      (org-agenda-sorting-strategy
		       '(todo-state-down effort-up category-keep))))
	 (tags-todo "-CANCELLED/!WAITING|HOLD"
		    ((org-agenda-overriding-header "Waiting and Postponed Tasks")
		     (org-tags-match-list-sublevels nil)
		     (org-agenda-todo-ignore-scheduled 'future)
		     (org-agenda-todo-ignore-deadlines 'future)))
	  (tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
		     ((org-agenda-overriding-header "Tasks")
;                      (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
		      (org-agenda-todo-ignore-scheduled t)
		      (org-agenda-todo-ignore-deadlines t)
		      (org-agenda-todo-ignore-with-date t)
		      (org-agenda-sorting-strategy
		       '(category-keep))))
	 (tags "-REFILE/"
	       ((org-agenda-overriding-header "Tasks to Archive")
		(org-agenda-skip-function 'bh/skip-non-archivable-tasks)
		(org-tags-match-list-sublevels nil))))
	 nil)
       ("r" "Tasks to Refile" tags "REFILE"
	((org-agenda-overriding-header "Tasks to Refile")
	 (org-tags-match-list-sublevels nil)))
       ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
	((org-agenda-overriding-header "Stuck Projects")
	 (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
       ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
	((org-agenda-overriding-header "Next Tasks")
	 (org-agenda-skip-function 'bh/skip-projects-and-habbits-and-single-tasks)
	 (org-agenda-todo-ignore-scheduled t)
	 (org-agenda-todo-ignore-deadlines t)
	 (org-agenda-todo-ignore-with-date t)
	 (org-tags-match-list-sublevels t)
	 (org-agenda-sorting-strategy
	  '(todo-state-down effort-up category-keep))))
       ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
	((org-agenda-overriding-header "Tasks")
;         (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
	 (org-agenda-sorting-strategy
	  '(category-keep))))
       ("f" "Fixes" tags-todo "-REFILE-CANCELLED+FIXES/!-HOLD-WAITING"
	((org-agenda-overriding-header "Tasks to fix stuff")
	 (org-agenda-sorting-strategy
	  '(todo-state-down effort-up category-keep))))
       ("T" "Org Tasks" tags-todo "-REFILE-CANCELLED+FIXES/!-HOLD-WAITING"
	((org-agenda-overriding-header "Tasks to implement new Org stuff")
	 (org-agenda-sorting-strategy
	  '(todo-state-down effort-up category-keep))))
       ("O" "Org Work"
	((agenda "")
	 (tags-todo "ORG-REFILE-CANCELLED-FIXES/!NEXT"
		    ((org-agenda-overriding-header "Org Next Task")
		     (org-tags-match-list-sublevels nil)
		     (org-agenda-todo-ignore-scheduled t)
		     (org-agenda-todo-ignore-deadlines t)
		     (org-agenda-todo-ignore-with-date t)
		     (org-agenda-sorting-strategy
		      '(todo-state-down effort-up category-keep))))
	 (tags-todo "ORG-REFILE-CANCELLED-FIXES/!NEXT"
		    ((org-agenda-overriding-header "Org Next Project")
		     (org-agenda-skip-function 'bh/skip-non-projects)
		     (org-agenda-todo-ignore-scheduled t)
		     (org-agenda-todo-ignore-deadlines t)
		     (org-agenda-todo-ignore-with-date t)
		     (org-agenda-sorting-strategy
		      '(Category-Keep)))))
	nil)
       ("P" "Projects" tags-todo "-HOLD-CANCELLED/!"
	((org-agenda-overriding-header "Projects")
	 (org-agenda-skip-function 'bh/skip-non-projects)
	 (org-agenda-sorting-strategy
	  '(category-keep))))
       ("w" "Waiting Tasks" tags-todo "-CANCELLED/!WAITING|HOLD"
	((org-agenda-overriding-header "Waiting and Postponed tasks"))
	(org-tags-match-list-sublevels nil))
       ("A" "Tasks to Archive" tags "-REFILE/"
	((org-agenda-overriding-header "Tasks to Archive")
	 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
	 (org-tags-match-list-sublevels nil)))))

;; clocking
;; Resume clockig task when emacs is restarted
(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (org-return)
  (org-cycle)
  (bh/insert-inactive-timestamp))

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)
(setq org-export-with-timestamps nil)
(setq org-return-follows-link t)
(setq org-remove-highlights-with-change nil)
(setq org-clone-delete-id t)
(setq org-cycle-include-plain-lists t)
(setq org-src-fontify-natively t)
;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
	  '(lambda () (hl-line-mode 1))
	  'append)

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)
(setq org-enforce-todo-dependencies t)
(setq org-startup-indented t)
(setq org-cycle-separator-lines 2)
(setq org-blank-before-new-entry (quote ((heading)
					 (plain-list-item))))
(setq org-reverse-note-order nil)
(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings (quote ((default))))
(setq org-special-ctrl-a/e 'reversed)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)
(setq org-log-done (quote time))
(setq org-log-into-drawer "LOGBOOK")
(setq global-auto-revert-mode t)

;; functions
;; disable C-c [ ,  C-c ] , C-c ; in org-mode
(add-hook 'org-mode-hook
	  '(lambda ()
	     ;; Undefine C-c [ and C-c ] since this breaks my
	     ;; org-agenda files when directories are include It
	     ;; expands the files in the directories individually
	     (org-defkey org-mode-map (kbd "C-c [") 'undefined)
	     (org-defkey org-mode-map (kbd "C-c ]") 'undefined))
	  'append)
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))
(setq org-stuck-projects (quote ("" nil nil "")))
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
	  (subtree-end (save-excursion (org-end-of-subtree t)))
	  (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
	(forward-line 1)
	(while (and (not has-subtask)
		    (< (point) subtree-end)
		    (re-search-forward "^\*+ " subtree-end t))
	  (when (member (org-get-todo-state) org-todo-keywords-1)
	    (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
			      (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
	  nil
	t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
	  (subtree-end (save-excursion (org-end-of-subtree t)))
	  (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
	(forward-line 1)
	(while (and (not has-subtask)
		    (< (point) subtree-end)
		    (re-search-forward "^\*+ " subtree-end t))
	  (when (member (org-get-todo-state) org-todo-keywords-1)
	    (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
	(is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
	(when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
	  (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
	  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
		 (has-next ))
	    (save-excursion
	      (forward-line 1)
	      (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
		(unless (member "WAITING" (org-get-tags-at))
		    (setq has-next t))))
	    (if has-next
		next-headline
	      nil)) ; a stuck project, has subtasks but no next task
	next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
	(widen)
	(let ((subtree-end (save-excursion (org-end-of-subtree t))))
	  (if (bh/is-project-p)
	      nil
	    subtree-end)))
    (org-end-of-subtree t)))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
	subtree-end)
       ((org-is-habit-p)
	subtree-end)
       (t
	nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
	next-headline)
       ((bh/is-project-p)
	next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
	next-headline)
       (t
	nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
	   (next-headline (save-excursion (or (outline-next-heading) (point-max))))
	   (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
	next-headline)
       ((org-is-habit-p)
	subtree-end)
       ((and (not limit-to-project)
	     (bh/is-project-subtree-p))
	subtree-end)
       ((and limit-to-project
	     (bh/is-project-subtree-p)
	     (member (org-get-todo-state) (list "NEXT")))
	subtree-end)
       (t
	nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
	subtree-end)
       ((org-is-habit-p)
	subtree-end)
       (t
	nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
	nil
      next-headline)))

(defun bh/verify-refile-target ()
  "Exlude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))
(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
	   (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
	   (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
	(when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
	  (setq parent-task (point)))
	(goto-char parent-task)
	parent-task))))

(defun bh/punch-in (arg)
  "Start continous clocking and set default task to the selected task.
If no task is selected set the Organization task as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
	     (tags (org-with-point-at marker (org-get-tags-at))))
	(if (and (eq arg 4) tags)
	    (org-agenda-clock-in '(16))
	  (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
	  (org-clock-in '(16))
	(bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
	(widen)
	(while (and (not parent-task) (org-up-heading-safe))
	  (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
	    (if parent-task
		(org-with-point-at parent-task
		  (org-clock-in))
	      (when bh/keep-clock-running
		(bh/clock-in-default-task)))))))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
	     (not org-clock-clocking-in)
	     (marker-buffer org-clock-default-task)
	     (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)
(require 'org-id)
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clcok in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
	 (cond
	  ((eq arg 4) org-clock-default-task)
	  ((and (org-clock-is-active)
		(equal org-clock-default-task (cadr org-clock-history)))
	   (caddr org-clock-history))
	  (org-clock-is-active) (cadr org-clock-history))
	 ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
	 (t (car org-clock-history))))
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

;;;; blogging stuff
(require 'org-jekyll)
(setq blog-sulci-site-name "http://sulci.net/blog/")
