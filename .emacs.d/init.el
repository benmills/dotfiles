;;; init.el --- My emacs config

;;; Commentary:
;;; Got all this from http://viget.com/extend/emacs-24-rails-development-environment-from-scratch-to-productive-in-5-minu

;;; Code:

(push "/usr/local/bin" exec-path)
(require 'cl)
(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'go-alternate)
(require 'go-focused-test)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(setq visible-bell t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode t)
(setq column-number-mode t)
(tooltip-mode -1)
(cd "~")

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
;(setq tab-width 2
;      indent-tabs-mode nil)

; Yes and No prompt
(defalias 'yes-or-no-p 'y-or-n-p)

; Packges
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar benmills/packages '(ac-slime
                            auto-complete
                            go-mode
                            magit
                            markdown-mode
                            puppet-mode
                            smex
                            flycheck
                            yaml-mode
                            color-theme-solarized
			    projectile
			    flx-ido
			    git-gutter
			    ruby-mode
			    ruby-end
			    rspec-mode
			    rvm
			    js2-mode
			    )
  "Default Packages")

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

; auto complete
(require 'auto-complete-config)
(ac-config-default)

; smex
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

; Ruby
(setq ruby-deep-indent-paren nil)
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-hook 'ruby-mode-hook
          (lambda () 
	    (rvm-activate-corresponding-ruby)
	    (ruby-end-mode)))
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))
(defun bash-compile (compile-command)
  (interactive "sCompile Command: ")
  (let ((shell-file-name "/bin/bash"))
    (compile compile-command)))
(ad-activate 'rspec-compile)

; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda () (visual-line-mode t)))

; Look
(load-theme 'solarized-light t)
(set-face-attribute 'default nil :height 100 :family "Source Code Pro")
(fringe-mode 4)

; Projectile
(projectile-global-mode)

; Git Gutter
(global-git-gutter-mode +1)

; golang
(setenv "GOPATH" "/Users/benmills/.go")
(add-hook
 'go-mode-hook
 '(lambda ()
    (auto-complete-mode)
    (define-key go-mode-map [?\C-c ?\C-t] 'run-go-tests)
    (define-key go-mode-map [?\C-c ?\C-g] 'find-test-name-at-point)
    (define-key go-mode-map [?\C-c ?\C-f] 'run-go-test-at-point)
    (define-key go-mode-map [?\C-c ?\C-b] 'run-go-test-for-buffer)
    (define-key go-mode-map [?\C-c ?\C-p] 'go-alternate-visit-at-point)
    (setq
     compilation-exit-message-function
     'compilation-autoclose-on-success)))


(defun rspec-test-for-buffer()
  (interactive)
  (compile (format "echo '%s' && rvm" (buffer-name))))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "8bb1e9a22e9e9d405ca9bdf20b91301eba12c0b9778413ba7600e48d2d3ad1fb" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
