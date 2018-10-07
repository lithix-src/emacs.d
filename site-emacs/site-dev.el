;;;; all
(defvar core-path
  (concat (getenv "HOME") "/core"))
(defvar core-lib-path
  (concat core-path "/lib"))

(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a
   symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

;;;; yasnippets
(require 'yasnippet)
(yas/global-mode 1)

;;;; project management via projectile
(require 'ivy)
(require 'projectile)
(setq projectile-enable-caching t)
(setq projectile-completeion-system 'grizzl)
(projectile-global-mode)
(setq projectile-switch-project-action 'projectile-dired)

(global-unset-key (kbd "C-t"))
(global-set-key (kbd "C-t") 'ido-goto-symbol)
(global-set-key (kbd "s-p") 'projectile-find-file)
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)
(global-set-key (kbd "s-g") 'projectile-grep)
(global-set-key (kbd "s-h") 'helm-projectile)
(global-set-key (kbd "s-P") 'projectile-switch-project)
(global-set-key (kbd "s-f") 'projectile-find-file-other-window)

;;;; multiple cursors are fun!
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-x s-e") 'mc/edit-lines)

;;;; textmade mode, cuz I'd too lazy do define these
;;;; shortcuts myself
(require 'textmate)
(textmate-mode)

;;;; gimme colors for all my color codes!
(require 'rainbow-mode)

;;;; ac-etags
(eval-after-load "etags"
  '(progn (ac-etags-setup)))

;;;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

;; pair management
(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

;;;; elisp
(add-to-list 'auto-mode-alist
	 '("\\.el$" . emacs-lisp-mode))

;;;; magit
;; by default, C-x m starts compose-mail
(global-unset-key
 (kbd "C-x m"))

(global-set-key
 (kbd "C-x m") `magit-status)

(global-set-key
 (kbd "C-c a") `markdown-table-align)
