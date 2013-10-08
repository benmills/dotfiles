;Got all this from http://viget.com/extend/emacs-24-rails-development-environment-from-scratch-to-productive-in-5-minu

(require 'cl)
(push "/usr/local/bin" exec-path)

(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq visible-bell t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode t)
(setq column-number-mode t)
(tooltip-mode -1)
;(load-theme 'sanityinc-tomorrow-day t)

; Marks (from https://github.com/abedra/emacs.d/blob/master/init.org)
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

; Display settings (from abedra)
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

; Indentation
(setq tab-width 2
      indent-tabs-mode nil)

; Yes and No prompt
(defalias 'yes-or-no-p 'y-or-n-p)

; Packges
(load "package")
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(defvar benmills/packages '(ac-slime
                            auto-complete
                            autopair
                            go-mode
                            magit
                            markdown-mode
                            puppet-mode
                            rvm
                            smex
                            flycheck
                            rspec-mode
                            yaml-mode)
  "Default packages")

(defun benmills/packages-installed-p ()
  (loop for pkg in benmills/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (benmills/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg benmills/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

; Ido mode
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

; rspec-mode
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))
(ad-activate 'rspec-compile)

					; auto complete
(require 'auto-complete-config)
(ac-config-default)

; autopair
(require 'autopair)

; smex
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

; Ruby
(require 'rvm)
(rvm-use-default)

; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda () (visual-line-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
