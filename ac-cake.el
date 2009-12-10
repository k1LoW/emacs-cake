;;; ac-cake.el --- CakePHP Minor Mode auto-complete.el source
;; -*- Mode: Emacs-Lisp -*-
(require 'auto-complete)

(defvar ac-cake-index nil)

(defun ac-cake-build-index ()
  (unless (not
           (and (cake-set-app-path) (executable-find "grep")))
    (ignore-errors
      (setq ac-cake-index nil)
      (with-temp-buffer
        ;;Model Method
        (call-process-shell-command
         (concat "grep '[^_]function' "
                 cake-app-path
                 "models/*.php --with-filename")
         nil (current-buffer))
        ;;Component Method
        (call-process-shell-command
         (concat "grep '[^_]function' "
                 cake-app-path
                 "controllers/components/*.php --with-filename")
         nil (current-buffer))
        ;;Behavior Method
        (call-process-shell-command
         (concat "grep '[^_]function' "
                 cake-app-path
                 "models/behaviors/*.php --with-filename")
         nil (current-buffer))
        (goto-char (point-min))
        (flush-lines "^ *$")
          (while (not (eobp))
            (if (not (re-search-forward ".+\\/\\(.+\\)\.php:.*function *\\([^ ]+\\) *(.*).*" nil t))
                (goto-char (point-max))
              (setq class-name (cake-camelize (match-string 1)))
              (setq function-name (match-string 2))
              (delete-region (point) (save-excursion (end-of-line) (point)))
              (push (concat class-name "->" function-name) ac-cake-index)
              ))
          ac-cake-index))))

(defvar ac-source-cake
  '((init . (lambda () (unless ac-cake-index
                         (ac-cake-build-index))))
    (candidates . ac-cake-index)
    (requires . 3))
  "Source for CakePHP")

(provide 'ac-cake)