;;;; Post init configurations

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; global keybingdings
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; file handling
(global-unset-key (kbd "C-x f"))
(global-set-key (kbd "C-x f") 'find-file-other-window)
(global-unset-key (kbd "C-\\"))
(global-set-key (kbd "C-x C-d") 'dired-at-point)
(global-set-key (kbd "C-x F") 'find-file-other-frame)
(global-set-key [f7] 'find-file-in-repository)
(global-set-key (kbd "S-<f7>") 'find-file-in-project)
(global-set-key (kbd "C-x M-f") 'find-file-at-point)

;; scm
(global-set-key (kbd "M-?") 'magit-status)

;; editing
(global-set-key (kbd "C-\\") 'comment-region)

;; misc
(global-set-key (kbd "<f1>") 'open-init)

;; full screen
; disable the stupidity that is os x full screen 
(setq ns-use-native-fullscreen nil)

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
(set-frame-parameter
 nil 'fullscreen
 (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(global-set-key (kbd "s-<return>") 'toggle-fullscreen)

;; keybinding functions
(defun open-init ()
  "Open emacs init file found in ~/.emacs.d and open it in another buffer"
  (interactive)
  (find-file-other-window "~/.emacs.d/init.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Simple package configurations
;;;;
;;;; If a packge configuration needs more than 10 or so lines,
;;;; I move it into it's own site-<package>.el
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; company mode
(eval-after-load "company"
  '(progn
     (add-to-list 'company-backends 'company-jedi)
     (add-to-list 'company-backends 'company-robe)))
;; python stuff
(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd ".") 'company-jedi-complete-on-dot)
     (define-key python-mode-map (kbd "<C-tab>") 'company-jedi)
     (define-key python-mode-map (kbd "M-?") 'company-jedi-show-doc)
     (define-key python-mode-map (kbd "M-r") 'company-jedi-find-references)
     (define-key python-mode-map (kbd "M-.") 'company-jedi-goto-definition)
     (define-key python-mode-map (kbd "M-,") 'pop-tag-mark)))
(add-hook 'python-mode-hook 'company-jedi-eldoc-setup)
(add-hook 'python-mode-hook 'company-jedi-start)
(global-company-mode)

(eval-after-load 'company
  '(define-key company-active-map (kbd "C-:") 'helm-company))

;;;; helm
(require 'helm-config)
(require 'helm-company)
(helm-mode 1)
(global-set-key (kbd "C-x SPC") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;;;; ido
(require 'flx-ido)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(setq flx-ido-threshhold 10000)

;;;; yasnippt
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/site-emacs/snippets"                 ;; personal snippets
	))


;;;; auto-complete.el
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-modes 'elsip-mode)
(add-to-list 'ac-modes 'python-mode)
(add-to-list 'ac-modes 'scala-mode)

(global-auto-complete-mode)
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 20)


;;;; ag
(require 'ag)
(setq ag-highlight-search t)


;;;; comint mode: cycle backwards and forwards through input history using up/down
(require 'comint)
(define-key comint-mode-map (kbd "M-<down>") 'comint-next-input)
(define-key comint-mode-map (kbd "M-<up>") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)


;;;; projectile
(require 'grizzl)
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completeion-system 'grizzl)
(global-set-key (kbd "s-p") 'projectile-find-file)
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)


;;;; yasnippets
(require 'yasnippet)
(require 'yasnippet-bundle)
(yas/global-mode 1)
(yas/initialize)

;;;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "C-x x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;;; window-numbering
(window-numbering-mode t)

;;;; bookmarks
(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (bookmark-set "SAVED")))
(global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-jump "SAVED")))


;; flyspell
(setq-default ispell-program-name "/usr/local/bin/ispell")
(setq-default ispell-list-command "list")
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)
(mapcar (lambda (mode-hook) (add-hook mode-hook 'flyspell-prog-mode))
     '(c-mode-common-hook R-mode-hook emacs-lisp-mode-hook))


;;;; some default hooks
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'fundamental-mode-hook 'auto-fill-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Development
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "site-dev")
