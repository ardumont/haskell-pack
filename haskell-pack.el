;;; haskell-pack.el --- Haskell configuration

;;; Commentary:

;;; Code:

(require 'install-packages-pack)
(install-packs '(flymake
                 flymake-shell
                 haskell-mode
                 ghci-completion
                 flymake-hlint
                 smartscan))

(require 'flymake)
(require 'haskell-mode)
(require 'inf-haskell)

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(require 'flymake-hlint)
(add-hook 'haskell-mode-hook 'flymake-hlint-load)
(add-hook 'haskell-mode-hook (lambda () (interactive) (inf-haskell-mode 1)))

;;;;;;;;;;;;;;;;;;;;; haskell

(defun haskell-pack/mode-defaults ()
  "Activate default haskell modes."
  (subword-mode +1)
  (turn-on-haskell-doc-mode)
  (turn-on-ghci-completion)
  ;; Ignore compiled Haskell files in filename completions
  (add-to-list 'completion-ignored-extensions ".hi")
  (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line))

(add-hook 'haskell-mode-hook 'haskell-pack/mode-defaults)

;; Forces flymake to underline bad lines, instead of fully
;; highlighting them; remove this if you prefer full highlighting.
;; (custom-set-faces
;;  '(flymake-errline ((((class color)) (:underline "red"))))
;;  '(flymake-warnline ((((class color)) (:underline "yellow")))))

;; ######### HELP IN SWITCHING BETWEEN BUFFERS

(defvar HASKELL-PACK/LAST-HASKELL-BUFFER (make-hash-table :test 'equal))
(defvar HASKELL-PACK/REPL-MODE 'inferior-haskell-mode)

(defun haskell-pack/switch-to-relevant-repl-buffer ()
  "Select the repl buffer, when possible in an existing window.
The buffer chosen is based on the file open in the current buffer."
  (interactive)
  (let ((current-buf         (current-buffer)) ;; current-buffer-name from which we switch to
        (haskell-repl-buffer (switch-to-haskell)))
    (puthash haskell-repl-buffer current-buf HASKELL-PACK/LAST-HASKELL-BUFFER)))

(defun haskell-pack/switch-to-last-haskell-buffer ()
  "Switch to the last haskell buffer.
The default keybinding for this command is
the same as `haskell-pack/switch-to-relevant-repl-buffer',
so that it is very convenient to jump between a
haskell buffer and the REPL buffer."
  (interactive)
  (let* ((cbuf (current-buffer))
         (last-haskell-pack (gethash cbuf HASKELL-PACK/LAST-HASKELL-BUFFER)))
    (message "Trying to switch from %s to %s" (buffer-name cbuf) last-haskell-pack)
    (when (and (eq major-mode HASKELL-PACK/REPL-MODE) (buffer-live-p last-haskell-pack))
      (pop-to-buffer last-haskell-pack))))

;; hook

(add-hook 'haskell-mode-hook
          (lambda ()
            ;; on haskell-mode, C-c C-z is switch-to-haskell
            (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-pack/switch-to-relevant-repl-buffer)))

(add-hook 'inferior-haskell-mode-hook
          (lambda ()
            ;; on inf-haskell, C-c C-z is on comint-stop-subjob
            (define-key inferior-haskell-mode-map (kbd "C-c C-z") 'haskell-pack/switch-to-last-haskell-buffer)
            (define-key inferior-haskell-mode-map (kbd "C-j") 'comint-send-input)))

(require 'smartscan)
(add-hook 'haskell-mode-hook (lambda () (smartscan-mode)))

(setq haskell-tags-on-save t)

(provide 'haskell-pack)
;;; haskell-pack.el ends here
