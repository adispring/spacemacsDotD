;;; keyfreq-config.el --- a simple package

;; Copyright (C) 2020  wangzengdi

;; Author: wangzengdi <506064082@qq.com>
;; Keywords: keyfreq config
;; Version: 0.0.1

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

;; keyfreq config package

;;; Code:

;;;###autoload
(defun turnon-keyfreq-mode ()
  (interactive)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;;###autoload
(defun turnoff-keyfreq-mode ()
  (interactive)
  (keyfreq-mode -1)
  (keyfreq-autosave-mode -1))

;;;###autoload
(defun init-keyfreq-excluded-commands ()
  (setq keyfreq-excluded-commands
        '(self-insert-command
          helm-next-line
          ivy-previous-line
          forward-char
          backward-char
          previous-line
          next-line
          )))

(provide 'keyfreq-config)

;;; keyfreq-config.el ends here
