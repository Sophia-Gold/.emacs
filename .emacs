(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;For important compatibility libraries like cl-lib
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;set correct path (setenv "PATH"
(setenv "PATH" "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin")
(add-to-list 'exec-path "/usr/local/bin")

;Powerline
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)

;Blackboard-theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/blackboard-theme")
(load-theme 'blackboard t)

;font
(set-face-attribute 'default nil :font "DejaVuSansMono")
(set-frame-font "DejaVuSansMono" nil t)

;open .emacs on load
(find-file user-init-file)

;ido mode
(require 'ido)
(ido-mode t)

;open new files in same frame
(setq ns-pop-up-frames nil)

;strack trace for debugging
(setq debug-on-error t)

;;line numbering
(require 'nlinum)
(global-nlinum-mode 1)

;mit-scheme eval
(load-library "xscheme")

;webmode
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

  ;(set-face-attribute 'web-mode-css-rule-face nil :foreground "Pink3")
)
(add-hook 'web-mode-hook 'my-web-mode-hook)
(define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)

;; I want an easy command for opening new shells:
(defun new-shell (name)
  "Opens a new shell buffer with the given name in
asterisks (*name*) in the current directory and changes the
prompt to 'name>'."
  (interactive "sName: ")
  (pop-to-buffer (concat "*" name "*"))
  (unless (eq major-mode 'shell-mode)
    (shell (current-buffer))
    (sleep-for 0 200)
    (delete-region (point-min) (point-max))
    (comint-simple-send (get-buffer-process (current-buffer)) 
                        (concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\""))))
(global-set-key (kbd "C-c s") 'new-shell)

;browser preview
(defun open-in-browser()
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))
(global-set-key (kbd "C-c p") 'open-in-browser)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-truncate-mode t)
 '(scheme-program-name "scheme"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
