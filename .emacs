(require 'package)
(package-initialize)

;; open .emacs on load
(find-file user-init-file)

;; (require 'exec-path-from-shell)
;; (when (memq window-system '(mac ns x))
;;   (exec-path-froms-hell-initialize))

;; disable audio/alerts
(setq ring-bell-function 'ignore)

;; disable scroll bars
(scroll-bar-mode -1)

;; disable toolbar
(tool-bar-mode -1)

;; fill screen
(setq frame-resize-pixelwise t)

;; blink cursor different colors
(blink-cursor-mode 1)
(defvar blink-cursor-colors (list "#ff0000" "#7fff00"))
(setq blink-cursor-count 1)
(defun blink-cursor-timer-function ()
  (when (not (internal-show-cursor-p))
    (when (>= blink-cursor-count (length blink-cursor-colors))
      (setq blink-cursor-count 0))
    (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
    (setq blink-cursor-count (+ 1 blink-cursor-count)))
  (internal-show-cursor nil (not (internal-show-cursor-p))))

(require 'powerline)
(powerline-default-theme)

(load-theme 'cyberpunk t)

(set-face-attribute 'default nil :font "Consolas")
(set-frame-font "Consolas" nil t)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; open new files in same frame
(setq ns-pop-up-frames nil)

;; strack trace for debugging
(setq debug-on-error t)

;; line numbering that doesn't crash emacs
(require 'nlinum)
(global-nlinum-mode 1)

;; make terminals less dumb
(server-start)

;; eshell prompt
(setq eshell-prompt-function
  (lambda ()
    (concat
     (eshell/pwd)
     "┣▇▇▇═─ ")))

;; command for opening new shells
(defun new-shell (name)
  (interactive "sName: ")
  (pop-to-buffer (concat "*" name "*"))
  (unless (eq major-mode 'shell-mode)
    (shell (current-buffer))
    (sleep-for 0 200)
    (delete-region (point-min) (point-max))
    (comint-simple-send (get-buffer-process (current-buffer)) 
			(concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\""))))
(global-set-key (kbd "C-c s") 'new-shell)

;; truncate erc buffers
(defvar erc-insert-post-hook)
    (add-hook 'erc-insert-post-hook
              'erc-truncate-buffer)
    (setq erc-truncate-buffer-on-save t)

(defun xscheme ()
      "Loads xscheme and runs a scheme process in the current buffer."
      (interactive)
      (load-library "xscheme")
      (xscheme-start "petite"
		     (buffer-name)
                     (buffer-name)))
(load-library "xscheme")
(autoload 'run-scheme "petite" "Run an inferior Scheme" t)
(setq scheme-program-name "petite")

;; pretty lambda
(defun my-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          )))
(add-hook 'scheme-mode-hook 'my-pretty-lambda)
(global-prettify-symbols-mode 1)

(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'xscheme-hook 'rainbow-delimiters-mode)
(add-hook 'js2-mode-hook 'rainbow-delimiters-mode)
(add-hook 'web-mode-hook 'rainbow-delimiters-mode)
(add-hook 'json-mode-hook 'rainbow-delimiters-mode)

;; vertical lines for nested YAML and JSON
;; (set-face-background 'highlight-indentation-face "#e3e3d3")
(add-hook 'yaml-mode-hook 'highlight-indentation-mode)
(add-hook 'json-mode-hook 'highlight-indentation-mode)

(require 'haskell-interactive-mode)
(require 'haskell-process)
(eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
(eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
(setq haskell-compile-cabal-build-command "stack build")
;; (setq haskell-process-compute-process-log-and-command '(('name . "test")) 'stack-ghci)
(setq haskell-process-type 'stack-ghci)
(setq haskell-process-path-stack "/usr/local/bin/stack")
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
(setq haskell-interactive-mode-eval-mode 'haskell-mode)
(defun haskell-interactive-toggle-print-mode ()
  (interactive)
  (setq haskell-interactive-mode-eval-mode
        (intern
         (ido-completing-read "Eval result mode: "
                              '("fundamental-mode"
                                "haskell-mode"
                                "espresso-mode"
                                "ghc-core-mode"
                                "org-mode")))))
(define-key haskell-interactive-mode-map (kbd "C-c C-v")
            'haskell-interactive-toggle-print-mode)
(defun create-unfocused-frame ()
  (let*
    ((prv (window-frame))
     (created (make-frame)))
    (select-frame-set-input-focus prv) created))
(defun create-haskell-interactive-frame ()
  (interactive)
  (haskell-interactive-bring)
  (create-unfocused-frame)
  (delete-window))

(require 'tuareg)
(setq tuareg-indent-align-with-first-arg nil)
(add-hook
 'tuareg-mode-hook
 (lambda()
   (setq show-trailing-whitespace t)
   (setq indicate-empty-lines t)
   (when (functionp 'prettify-symbols-mode)
     (prettify-symbols-mode))
   (when (functionp 'flyspell-prog-mode)
     (flyspell-prog-mode))))
(define-key tuareg-mode-map [(f12)] 'next-error)
(define-key tuareg-mode-map [(shift f12)] 'previous-error)
(when (require 'merlin nil t)
  (setq merlin-command 'opam)
  (add-to-list 'auto-mode-alist '("/\\.merlin\\'" . conf-mode))
  (when (functionp 'merlin-document)
    (define-key tuareg-mode-map (kbd "\C-c\C-h") 'merlin-document))
  (add-hook 'tuareg-mode-hook
            (lambda()
              (let ((fn (buffer-file-name)))
                (if (and fn (locate-dominating-file fn ".merlin"))
		    (merlin-mode))))))
(setq auto-mode-alist
      (append '(("_oasis\\'" . conf-mode)
		("_tags\\'" . conf-mode)
		("_log\\'" . conf-mode))
	      auto-mode-alist))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (setq web-mode-style-padding 1)	       
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0)
  (setq web-mode-comment-style 2)
  (setq web-mode-enable-auto-pairing) 
  (setq web-mode-enable-css-colorization) 
  (setq web-mode-enable-block-face) 
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-comment-keywords t)
  (setq web-mode-enable-heredoc-fontification t)
)
(add-hook 'web-mode-hook 'my-web-mode-hook)
(define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)

;; browser preview
(defun open-in-browser()
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))
(global-set-key (kbd "C-c p") 'open-in-browser)

(require 'js-comint)
(setq inferior-js-program-command "/usr/local/bin/node")

(require 'js2-mode)
(add-hook 'js2-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
            (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
            (local-set-key (kbd "C-c b") 'js-send-buffer)
            (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
            (local-set-key (kbd "C-c l") 'js-load-file-and-go)))

(custom-set-variables
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (cyberpunk)))
 '(custom-safe-themes
   (quote
    ("dcbe22bc74153257f412183dd14ab9652197f59adf65646e618c2577e7cca34d" "40da996f3246a3e99a2dff2c6b78e65307382f23db161b8316a5440b037eb72c" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "0820d191ae80dcadc1802b3499f84c07a09803f2cb90b343678bdb03d225b26b" default)))
 '(erc-truncate-mode t)
 '(haskell-interactive-types-for-show-ambiguous nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote ghci))
 '(haskell-tags-on-save t)
 '(org-startup-truncated nil)
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/")
    ("gnu" . "http://elpa.gnu.org/packages/")))
 '(package-selected-packages
   (quote
    (json-mode yaml-mode highlight-indentation rainbow-blocks rainbow-identifiers rainbow-delimiters rainbow-mode groovy-mode idris-mode exec-path-from-shell js2-mode js-comint web-mode tuareg prop-menu powerline nlinum merlin magit lush-theme haskell-mode cyberpunk-theme)))
 '(scheme-program-name "petite"))
