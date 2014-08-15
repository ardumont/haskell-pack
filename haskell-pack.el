;;; haskell-pack.el --- Haskell configuration

;;; Commentary:

;;; Code:

(require 'install-packages-pack)
(install-packs '(flymake
                 flymake-shell
                 haskell-mode
                 ghci-completion
                 flymake-hlint
                 smartscan
                 w3m))

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
  (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
  (require 'w3m-haddock)
  (define-key haskell-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock))

(add-hook 'haskell-mode-hook 'haskell-pack/mode-defaults)

;; Forces flymake to underline bad lines, instead of fully
;; highlighting them;
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

;; ######### HELP IN SWITCHING BETWEEN BUFFERS

(defvar HASKELL-PACK/LAST-HASKELL-BUFFER (make-hash-table :test 'equal))
(defvar HASKELL-PACK/REPL-MODE 'inferior-haskell-mode)

(defun haskell-pack/switch-to-relevant-repl-buffer ()
  "Select the repl buffer, when possible in an existing window.
The buffer chosen is based on the file open in the current buffer."
  (interactive)
  (let ((cbuf (current-buffer)))
    (puthash (switch-to-haskell) cbuf HASKELL-PACK/LAST-HASKELL-BUFFER))
  HASKELL-PACK/LAST-HASKELL-BUFFER)

(defun haskell-pack/switch-to-last-haskell-buffer ()
  "Switch to the last haskell buffer.
The default keybinding for this command is
the same as `haskell-pack/switch-to-relevant-repl-buffer',
so that it is very convenient to jump between a
haskell buffer and the REPL buffer."
  (interactive)
  (let* ((key-haskell-repl (current-buffer))
         (last-buffer      (gethash key-haskell-repl HASKELL-PACK/LAST-HASKELL-BUFFER)))
    (message "Switch back from %s to %s" key-haskell-repl last-buffer)
    (when (and (eq major-mode HASKELL-PACK/REPL-MODE) (buffer-live-p last-buffer))
      (pop-to-buffer last-buffer))))

;; hook

(add-hook 'inf-haskell-mode-hook ;; inferior haskell in haskell buffer
          (lambda ()
            ;; on inf-haskell-mode (minor mode for haskell buffer designed for interaction with repl), C-c C-z is switch-to-haskell, we override this
            (define-key inf-haskell-mode-map (kbd "C-c C-z") 'haskell-pack/switch-to-relevant-repl-buffer)))

(add-hook 'inferior-haskell-mode-hook ;; inferior-haskell-mode in repl
          (lambda ()
            ;; on inf-haskell, C-c C-z is on comint-stop-subjob
            (define-key inferior-haskell-mode-map (kbd "C-c C-z") 'haskell-pack/switch-to-last-haskell-buffer)
            (define-key inferior-haskell-mode-map (kbd "C-j") 'comint-send-input)))

(require 'smartscan)
(add-hook 'haskell-mode-hook (lambda () (smartscan-mode)))

(setq haskell-tags-on-save t)

(provide 'haskell-pack)
;;; haskell-pack.el ends here
