;;;; ~/.emacs.d/site-emacs/site-java.el
;;;;
;;;; configurations for java


;;;; eclim
(defvar eclipse-home "/Applications/eclipse")
(custom-set-variables
 '(eclim-eclipse-dirs '(eclipse-home)))
(setenv "ECLIPSE_HOME" eclipse-home)

(require 'eclim)
(require 'eclimd)
(global-eclim-mode)

;; variable tweaks
(setq eclimd-executable
      (concat eclipse-home "/eclimd"))
(setq eclim-executable
      (concat eclipse-home "/eclim"))
(setq eclimd-default-workspace
      (concat core-lib-path "/java"))


;; show errors in the echo area
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

;; autocompletion
(require 'auto-complete-config)
(ac-config-default)

(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)
(add-hook 'eclim-mode-hook 
	  (lambda () (add-to-list 'ac-sources 'ac-source-emacs-eclim)))


;; setup company mode to provide popups
(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)

;; keybindings
(global-set-key (kbd "C-c C-x i") 'eclim-java-import-organize)
(global-set-key (kbd "C-c C-x d") 'eclim-java-show-documentation-for-current-element)
(global-set-key (kbd "C-c C-x s") 'eclim-java-find-type)
