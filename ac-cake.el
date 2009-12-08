;;; ac-cake.el --- CakePHP Minor Mode auto-complete.el source
;; -*- Mode: Emacs-Lisp -*-
(require 'auto-complete)

(defun ac-cake-candidate ()
  (unless (nohjtt
           (and (cake-set-app-path) (executable-find "grep")))
    (ignore-errors
      (message "%s" ac-prefix)
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
        (let (ac-cake-index)
          (while (not (eobp))
            (if (not (re-search-forward ".+\\/\\(.+\\)\.php:.*function *\\([^ ]+\\) *(.*).*" nil t))
                (goto-char (point-max))
              (setq cp (match-beginning 0))
              (setq function-name (match-string 2))
              (setq cake-singular-name (match-string 1))
              (setq class-name (cake-camelize cake-singular-name))
              (goto-char cp)
              (delete-region (point) (save-excursion (end-of-line) (point)))
              (push (concat class-name "->" function-name) ac-cake-index)
              ))
          ac-cake-index)))))

(defvar ac-source-cake
  '((candidates . ac-cake-candidate)
    (requires . 3)
    (cache)
    )
  "Source for CakePHP")

(provide 'ac-cake)