;;; anzu.el --- Show number of matches in mode-line while searching

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-anzu
;; Version: 20131021.2204
;; X-Original-Version: 0.11

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `anzu.el' is an Emacs port of `anzu.vim'.
;;
;; `anzu.el' provides a minor mode which displays 'current match/total
;; matches' in the mode-line in various search modes.  This makes it
;; easy to understand how many matches there are in the current buffer
;; for your search query.

;; To use this package, add following code to your init.el or .emacs
;;   (require 'anzu)
;;   (global-anzu-mode +1)
;;

;;; Code:

(eval-when-compile
  (require 'cl)
  (defvar migemo-isearch-enable-p))

(defgroup anzu nil
  "Show searched position in mode-line"
  :group 'isearch)

(defcustom anzu-mode-lighter " Anzu"
  "Lighter of anzu-mode"
  :type 'string
  :group 'anzu)

(defcustom anzu-cons-mode-line-p t
  "Set nil if you use your own mode-line setting"
  :type 'boolean
  :group 'anzu)

(defcustom anzu-minimum-input-length 1
  "Minimum input length to enable anzu"
  :type 'integer
  :group 'anzu)

(defcustom anzu-search-threshold nil
  "Limit of search number"
  :type '(choice (integer :tag "Threshold of search")
                 (boolean :tag "No threshold" nil))
  :group 'anzu)

(defcustom anzu-use-migemo nil
  "Flag of using migemo"
  :type 'boolean
  :group 'anzu)

(defcustom anzu-mode-line-update-function nil
  "Function which return mode-line string"
  :type 'function
  :group 'anzu)

(defcustom anzu-regexp-search-commands '(isearch-forward-regexp
                                         isearch-backward-regexp)
  "Search function which use regexp."
  :type '(repeat function)
  :group 'anzu)

(defcustom anzu-input-idle-delay 0.05
  "Idle second for updating modeline at replace commands"
  :type 'number
  :group 'anzu)

(defface anzu-mode-line
  '((t (:foreground "magenta" :weight bold)))
  "face of anzu modeline"
  :group 'anzu)

(defface anzu-replace-highlight
  '((t :inherit query-replace))
  "highlight of replaced string"
  :group 'anzu)

(defvar anzu--total-matched 0)
(defvar anzu--current-posion 0)
(defvar anzu--overflow-p nil)
(defvar anzu--last-isearch-string nil)
(defvar anzu--cached-positions nil)
(defvar anzu--last-command nil)
(defvar anzu--state nil)
(defvar anzu--cached-count 0)
(defvar anzu--replace-begin nil)
(defvar anzu--replace-end nil)
(defvar anzu--window-height 0)

(defun anzu--validate-regexp (regexp)
  (condition-case err
      (progn
        (string-match-p regexp "")
        t)
    (invalid-regexp nil)))

(defsubst anzu--construct-position-info (count overflow positions)
  (list :count count :overflow overflow :positions positions))

(defun anzu--search-all-position (str)
  (unless anzu--last-command
    (setq anzu--last-command last-command))
  (when (and (not (memq anzu--last-command anzu-regexp-search-commands))
             (not isearch-regexp))
    (setq str (regexp-quote str)))
  (if (not (anzu--validate-regexp str))
      anzu--cached-positions
    (save-excursion
      (goto-char (point-min))
      (let ((positions '())
            (count 0)
            (overflow nil)
            (finish nil)
            (search-func (if (and anzu-use-migemo migemo-isearch-enable-p)
                             'migemo-forward
                           're-search-forward)))
        (while (and (not finish) (funcall search-func str nil t))
          (push (cons (match-beginning 0) (match-end 0)) positions)
          (incf count)
          (when (= (match-beginning 0) (match-end 0)) ;; Case of anchor such as "^"
            (if (eobp)
                (setq finish t)
              (forward-char 1)))
          (when (and anzu-search-threshold (>= count anzu-search-threshold))
            (setq overflow t finish t)))
        (let ((result (anzu--construct-position-info count overflow (reverse positions))))
          (setq anzu--cached-positions (copy-sequence result))
          result)))))

(defun anzu--where-is-here (positions here)
  (loop for (start . end) in positions
        for i = 1 then (1+ i)
        when (and (>= here start) (<= here end))
        return i
        finally return 0))

(defun anzu--update ()
  (when (>= (length isearch-string) anzu-minimum-input-length)
    (let ((result (if (string= isearch-string anzu--last-isearch-string)
                      anzu--cached-positions
                    (anzu--search-all-position isearch-string))))
      (let ((curpos (anzu--where-is-here (plist-get result :positions) (point))))
        (setq anzu--total-matched (plist-get result :count)
              anzu--overflow-p (plist-get result :overflow)
              anzu--current-posion curpos
              anzu--last-isearch-string isearch-string)
        (force-mode-line-update)))))

(defsubst anzu--mode-line-not-set-p ()
  (and (listp mode-line-format)
       (equal (car mode-line-format) '(:eval (anzu--update-mode-line)))))

(defun anzu--cons-mode-line-search ()
  (anzu--cons-mode-line 'search))

(defun anzu--cons-mode-line (state)
  (setq anzu--state state)
  (when (and anzu-cons-mode-line-p (not (anzu--mode-line-not-set-p)))
    (setq mode-line-format (cons '(:eval (anzu--update-mode-line))
                                 mode-line-format))))

(defsubst anzu--reset-status ()
  (setq anzu--total-matched 0
        anzu--current-posion 0
        anzu--last-command nil
        anzu--overflow-p nil))

(defun anzu--reset-mode-line ()
  (anzu--reset-status)
  (when (and anzu-cons-mode-line-p (anzu--mode-line-not-set-p))
    (setq mode-line-format (cdr mode-line-format))))

(defsubst anzu--format-here-position (here total)
  (if (and anzu--overflow-p (zerop here))
      (format "%d+" total)
    here))

(defun anzu--update-mode-line-default (here total)
  (case anzu--state
    (search (propertize (format "(%s/%d%s)"
                                (anzu--format-here-position here total)
                                total (if anzu--overflow-p "+" ""))
                        'face 'anzu-mode-line))
    (replace (propertize (format "(%d replace)" total)
                         'face 'anzu-mode-line))
    (otherwise "")))

(defun anzu--update-mode-line ()
  (let ((update-func (or anzu-mode-line-update-function
                         'anzu--update-mode-line-default)))
    (funcall update-func anzu--current-posion anzu--total-matched)))

;;;###autoload
(define-minor-mode anzu-mode
  "minor-mode which display search information in mode-line."
  :group      'anzu
  :init-value nil
  :global     nil
  :lighter    anzu-mode-lighter
  (if anzu-mode
      (progn
        (add-hook 'isearch-update-post-hook 'anzu--update nil t)
        (add-hook 'isearch-mode-hook 'anzu--cons-mode-line-search nil t)
        (add-hook 'isearch-mode-end-hook 'anzu--reset-mode-line nil t))
    (remove-hook 'isearch-update-post-hook 'anzu--update t)
    (remove-hook 'isearch-mode-hook 'anzu--cons-mode-line t)
    (remove-hook 'isearch-mode-end-hook 'anzu--reset-mode-line t)
    (anzu--reset-mode-line)))

;;;###autoload
(define-global-minor-mode global-anzu-mode
  anzu-mode
  (lambda ()
    (unless (minibufferp)
      (anzu-mode t)))
  :group 'anzu)

(defsubst anzu--query-prompt-base (use-region use-regexp)
  (concat "Query replace"
          (if current-prefix-arg " word" "")
          (if use-regexp " regexp" "")
          (if use-region " in region" ""))  )

(defun anzu--query-prompt (use-region use-regexp)
  (let ((prompt (anzu--query-prompt-base use-region use-regexp)))
    (if query-replace-defaults
        (format "%s (default %s -> %s) " prompt
                (query-replace-descr (car query-replace-defaults))
                (query-replace-descr (cdr query-replace-defaults)))
      prompt)))

(defun anzu--add-overlay (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'anzu-replace-highlight)
    (overlay-put ov 'anzu-replace t)))

(defsubst anzu--case-fold-search (input)
  (let ((case-fold-search nil))
    (not (string-match-p "[A-Z]" input))))

(defun anzu--count-matched (buf str use-regexp)
  (when (not use-regexp)
    (setq str (regexp-quote str)))
  (if (not (anzu--validate-regexp str))
      anzu--cached-count
    (with-current-buffer buf
      (save-excursion
        (let* ((overlay-beg anzu--replace-begin)
               (overlay-end (min (save-excursion
                                   (forward-line anzu--window-height)
                                   (point))
                                 anzu--replace-end)))
          (goto-char anzu--replace-begin)
          (let ((count 0)
                (finish nil)
                (case-fold-search (anzu--case-fold-search str)))
            (while (and (not finish) (re-search-forward str anzu--replace-end t))
              (incf count)
              (let ((beg (match-beginning 0))
                    (end (match-end 0)))
                (if (= beg end)
                    (if (eobp)
                        (setq finish t)
                      (forward-char 1))
                  (when (and (>= beg overlay-beg) (<= end overlay-end))
                    (anzu--add-overlay beg end)))))
            (setq anzu--cached-count count)))))))

(defun anzu--check-minibuffer-input (buf use-regexp)
  (let ((content (minibuffer-contents)))
    (let ((matched (if (string= content "")
                       (setq anzu--cached-count 0)
                     (anzu--count-matched buf content use-regexp))))
      (setq anzu--total-matched matched)
      (force-mode-line-update))))

(defun anzu--clear-overlays (buf beg end)
  (with-current-buffer buf
    (dolist (ov (overlays-in (or beg (point-min)) (or end (point-max))))
      (when (overlay-get ov 'anzu-replace)
        (delete-overlay ov)))))

(defun anzu--read-from-string (prompt use-regexp)
  (let ((curbuf (current-buffer))
        (timer nil))
    (unwind-protect
        (minibuffer-with-setup-hook
            #'(lambda ()
                (setq timer (run-with-idle-timer
                             (max anzu-input-idle-delay 0.01)
                             'repeat
                             (lambda ()
                               (anzu--clear-overlays curbuf nil nil)
                               (with-selected-window (or (active-minibuffer-window)
                                                         (minibuffer-window))
                                 (anzu--check-minibuffer-input curbuf use-regexp))))))
          (read-from-minibuffer (format "%s: " prompt)
                                nil nil nil
                                query-replace-from-history-variable nil t))
      (when timer
        (cancel-timer timer)
        (setq timer nil)))))

(defun anzu--query-validate-from-regexp (from)
  (when (string-match "\\(?:\\`\\|[^\\]\\)\\(?:\\\\\\\\\\)*\\(\\\\[nt]\\)" from)
    (let ((match (match-string 1 from)))
      (cond
       ((string= match "\\n")
        (message "`\\n' here doesn't match a newline; type C-q C-j instead!!"))
       ((string= match "\\t")
        (message "\\t' here doesn't match a tab; to do that, just type TAB!!")))
      (sit-for 2))))

(defun anzu--query-from-string (prompt use-regexp)
  (let ((from (anzu--read-from-string prompt use-regexp)))
    (if (and (string= from "") query-replace-defaults)
        (cons (car query-replace-defaults)
              (query-replace-compile-replacement
               (cdr query-replace-defaults) use-regexp))
      (add-to-history query-replace-from-history-variable from nil t)
      (when use-regexp
        (anzu--query-validate-from-regexp from))
      from)))

(defsubst anzu--set-region-information (beg end)
  (setq anzu--replace-begin beg
        anzu--replace-end end))

(defun anzu--query-replace-common (use-regexp)
  (setq anzu--window-height (window-height))
  (anzu--cons-mode-line 'replace)
  (let* ((use-region (use-region-p))
         (beg (if use-region (region-beginning) (point)))
         (end (if use-region (region-end) (point-max)))
         (prompt (anzu--query-prompt use-region use-regexp))
         (delimited current-prefix-arg)
         (curbuf (current-buffer))
         (clear-overlay nil))
    (anzu--set-region-information beg end)
    (unwind-protect
        (let* ((from (anzu--query-from-string prompt use-regexp))
               (to (if (consp from)
                       (prog1 (cdr from) (setq from (car from)))
                     (query-replace-read-to from prompt use-regexp))))
          (anzu--clear-overlays curbuf beg end)
          (setq clear-overlay t)
          (if use-regexp
              (perform-replace from to t t delimited nil nil beg end)
            (query-replace from to delimited beg end)))
      (progn
        (unless clear-overlay
          (anzu--clear-overlays curbuf beg end))
        (anzu--reset-mode-line)
        (force-mode-line-update)))))

;;;###autoload
(defun anzu-query-replace ()
  (interactive)
  (anzu--query-replace-common nil))

(defun anzu-query-replace-regexp ()
  (interactive)
  (anzu--query-replace-common t))

(provide 'anzu)
;;; anzu.el ends here
